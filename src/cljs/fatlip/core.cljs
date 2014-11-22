(ns fatlip.core
  (:require [clojure.core.rrb-vector :as rrb]
            [clojure.set :as set]
            [clojure.string :as s]))


(defprotocol Reversible
  (rev [item] "It... reverses"))


(defrecord Node [id layer-id characters weight])

(defrecord Edge [src dest characters weight]
  Reversible
  (rev [this]
    (assoc this
      :src dest
      :dest src)))

(defrecord Segment [endpoints layer-id characters weight])

(defrecord SparseGraph [layers succs preds ps qs rs]
  Reversible
  (rev [this]
    (assoc this
      :succs preds
      :preds succs
      :ps qs
      :qs ps
      :layers (vec (rseq layers)))))

(defrecord SparseLayer [id duration nodes])
(defrecord OrderedGraph [layers succs preds ps qs minus-ps minus-qs]
  Reversible
  (rev [this]
    (assoc this
      :succs preds
      :preds succs
      :ps qs
      :qs ps
      :minus-ps minus-qs
      :minus-qs minus-ps
      :layers (vec (rseq layers)))))

(defrecord OrderedLayer [id duration items])
(defrecord CountedAndMarkedGraph [layers succs preds crossings marked]) ; still has OrderedLayers
(defrecord FlatGraph [layers succs preds aboves belows top-idxs bot-idxs])
(defrecord FlatLayer [id duration items])
(defrecord BlockGraph [blocks succs sources])
(defrecord BlockEdge [src dest weight])
(defrecord ClassGraph [classes succs sources])
(defrecord AccumulatorNode [weight node-edges is-seg-c])


(defn Edge->Segment
  "Does what it says on the tin; Segments represent the portion of an Edge that
  crosses a particular layer"
  [edge layer-id]
  (map->Segment {:endpoints #{(:src edge) (:dest edge)}
                 :layer-id layer-id
                 :characters (:characters edge)
                 :weight (:weight edge)}))


(defn add-edge [graph last-node node characters]
  "Creates an edge and adds it to the graph as a whole and to each participating node"
  (let [weight (count characters)
        edge (Edge. last-node node characters weight)]
    (-> graph
        (update-in [:succs last-node] (fnil conj #{}) edge)
        (update-in [:preds node] (fnil conj #{}) (rev edge)))))


(defn make-node [graph layer-id input]
  "Utility function that was abstracted to avoid mutually recursing back to add-node
  when creating helper (p, q, and r) nodes. In particular, helper nodes don't require
  character group processing or bookkeeping metadata"
  (let [node-num (count (-> graph :layers (get layer-id) :nodes))
        node-id (keyword (s/join "-" [layer-id node-num]))
        weight (-> input :characters count)
        node (map->Node (assoc input :id node-id :layer-id layer-id :weight weight))
        g (update-in graph [:layers layer-id :nodes] conj node)]
    [g node]))


(defn add-p-q-nodes [graph last-node node characters]
  "When an edge would span more than 2 layers, two helper nodes are created. P nodes
  are placed on the layer following last-node's layer, and q nodes are placed on the
  layer preceding node's layer. Edges are then drawn from last-node -> p node and from
  p node to q node, regardless of the span between the p node and the q node"
  (let [proto-node {:characters characters}
        p-layer-id (inc (:layer-id last-node))
        q-layer-id (dec (:layer-id node))
        [p-g p-node] (make-node graph p-layer-id proto-node)
        [q-g q-node] (make-node p-g q-layer-id proto-node)
        g (-> q-g
              (update-in [:ps] conj p-node)
              (update-in [:qs] conj q-node)
              (add-edge last-node p-node characters)
              (add-edge p-node q-node characters))]
    [g q-node]))


(defn add-r-node [graph last-node node characters]
  "R nodes are created to assist in drawing the graph, when an edge skips over a layer.
  In that case, the r node is placed in the intervening layer, and an edge is created
  from last-node to r node"
  (let [r-layer-id (inc (:layer-id last-node))
        [r-g r-node] (make-node graph r-layer-id {:characters characters})
        g (-> (update-in r-g [:rs] conj r-node)
              (add-edge last-node r-node characters))]
    [g r-node]))


(defn process-edge-characters [graph last-node node characters]
  "Create edges and possibly new helper nodes for a set of characters that
  remain together from last-node to node"
  (let [span (- (:layer-id node) (:layer-id last-node))
        [g last-n] (cond
                    (= span 2) (add-r-node graph last-node node characters)
                    (> span 2) (add-p-q-nodes graph last-node node characters)
                    :else [graph last-node])]
    (add-edge g last-n node characters)))


(defn process-characters
  "Process each set of characters, where a set is defined as those characters
  present in the current node that also share their last node (ie. they define
  an edge in the graph)"
  [graph node]
  (loop [graph graph
         characters (set (:characters node))]
     (if (empty? characters)
       graph
       (let [character (first characters)
             last-node (-> graph :last-nodes-by-character character)
             [g c] (if last-node
                     (let [last-node-characters (-> graph
                                                    :last-nodes-by-node
                                                    ((:id last-node) #{}))
                           edge-characters (vec (set/intersection last-node-characters characters))
                           g (process-edge-characters graph last-node node edge-characters)]
                       [g (set/difference characters last-node-characters)])
                     [graph (disj characters character)])]
         (recur g c)))))


(defn add-node
  "Create a new node, then deduce and create edges to that node
  from previous layers. Then, update the bookkeeping metadata for each
  character in a node for calculating future edges from this node"
  [graph layer-id input]
  (let [[grph node] (make-node graph layer-id input)]
    (loop [new-g (process-characters grph node)
           characters (:characters node)]
      ;; Update node metadata for all characters in the new node
      (if (empty? characters)
        new-g
        (let [character (first characters)
              last-node (-> new-g :last-nodes-by-character character)
              disappeared (contains? (-> node :path-mods character) :disappeared)
              g (cond-> new-g
                        disappeared (update-in [:last-nodes-by-character] dissoc character)
                        (not disappeared) (->
                                           (assoc-in [:last-nodes-by-character character] node)
                                           (update-in [:last-nodes-by-node (:id node)] (fnil conj #{}) character))
                        last-node (update-in [:last-nodes-by-node (:id last-node)] disj character))]
          (recur g (rest characters)))))))


(defn add-layer
  "Create the layer of our graph representation, and recursively
  create and add nodes from the groups in the input layer"
  [graph input-layer]
  (let [layers (:layers graph)
        layer-id (count layers)
        layer (SparseLayer. layer-id
                            (:duration input-layer)
                            [])]
    (loop [g (update-in graph [:layers] conj layer)
           input-groups (input-layer :groups)]
      (if (empty? input-groups)
        g
        (recur (add-node g layer-id (first input-groups))
               (rest input-groups))))))


(defn inp->SparseGraph
  "Creates a sparse graph, as defined in ESK"
  [input]
  (loop [graph (map->SparseGraph {:layers []
                                  :succs {}
                                  :preds {}
                                  :ps #{}
                                  :qs #{}
                                  :rs #{}})
         inp input]
    (if (empty? inp)
      (if (empty? (:succs graph))
        (throw (js/Error. "At least one character must appear twice"))
        graph)
      (let [i (first inp)
            dup-character (->> (mapcat #(:characters %) (:groups i))
                               frequencies
                               (filter (fn [[_ c]] (> c 1)))
                               first)]
        (if dup-character
          (throw (js/Error. (str (get dup-character 0) " is specified "
                                 (get dup-character 1) " times in one layer")))
          (recur (add-layer graph i) (rest inp)))))))


(defn replace-ps
  "Step 1 of ESK - replace all p nodes with edges and merge segment containers"
  [items ps succs]
  (->> items
       (map #(if (contains? ps %)
               ;; p nodes always have only one successor
               [(-> succs (get %) first)]
               %))
       (reduce #(if (and (vector? (peek %1))
                         (vector? %2))
                  (update-in %1 [(dec (count %1))] rrb/catvec %2)
                  (conj %1 %2))
               [])))


(defn set-positions
  "Step 2a of ESK. Positions in an ordered layer are used to calculate the order of the
  next layer. ESK's description of the position algorithm is almost willfully circuitous
  and obtuse, so here's a simplified description: An item's position in an ordered layer
  is the sum of the size of all previous items, plus 1, where the size of a segment
  container is the number of edges it contains, and the size of a node is 1.

  Also, it doesn't really matter what seed you choose for the initial value of the sum. I
  chose -1, which is implied by the description in ESK."
  [minus-ps]
  (loop [m-ps minus-ps
         current-position -1
         positions {}]
    (if (empty? m-ps)
      positions
      (let [item (first m-ps)
            c-p (if (vector? item)
                  (+ current-position (count item))
                  (inc current-position))]
        (recur (rest m-ps) c-p (assoc positions item (inc current-position)))))))


(defn get-measure
  "Calculate the average weighted position of a node's predecessors"
  [node preds pred-positions]
  (if (empty? preds)
    0
    (apply / (->> preds
                  (map #(let [weight (:weight %)
                              pos (get pred-positions (:dest %))]
                          [(* weight pos) weight]))
                  (apply map +)))))


(defn set-measures
  "Step 2b of ESK - Use nodes' predecessors to calculate a 'measure' for the nodes
  and containers in a layer, which is used for ordering"
  [non-qs preds positions]
  (into {} (map #(-> [% (get-measure % (get preds %) positions)]) non-qs)))


(defn merge-layer
  "Step 3 of ESK - Considers a layer as two lists, one of nodes and the other of segment
  containers. The items in these lists have 'measures' (for segment containers, this is
  equivalent to the position in the previous layer, so we just use that), and we merge
  the two lists into a single ordering based on these measures."
  [minus-ps positions non-qs measures]
  (let [ns (sort-by #(get measures %) non-qs)
        ss (->> (filter vector? minus-ps)
                (sort-by #(get positions %)))]
    (loop [nodes ns
           segments ss
           pos positions
           ord []]
      (if (or (empty? nodes) (empty? segments))
        ;; ESK's algorithm doesn't specify what to do with leftover things
        ;; I think this is because it doesn't take into account nodes that don't have
        ;; parents in layers > 0. In any case, at most one of nodes or segments
        ;; will be non-empty
        (-> ord (into nodes) (into segments))
        (let [node-1 (first nodes)
              seg-1 (first segments)
              node-measure (get measures node-1)
              seg-position (get pos seg-1)
              node-first (<= node-measure seg-position)
              seg-first (>= node-measure (+ seg-position (count seg-1)))]
          (cond node-first (recur (rest nodes) segments pos (conj ord node-1))
                seg-first (recur nodes (rest segments) pos (conj ord seg-1))
                :else (let [k (.ceil js/Math (- node-measure seg-position))
                            s-1 (rrb/subvec seg-1 0 k)
                            s-2 (rrb/subvec seg-1 k)]
                        (recur
                         (rest nodes)
                         (cons s-2 segments)
                         (assoc pos s-2 (+ (get pos seg-1) 1))
                         (into ord [s-1 node-1])))))))))


(defn add-qs
  "Step 4 of ESK - takes the results of step 3, which doesn't include the q-nodes, and adds them,
  splitting their segment containers in the process"
  [minus-qs qs]
  (->> (map #(if (contains? qs (:dest %))
               (:dest %)
               %)
            (flatten minus-qs))
       (partition-by (partial instance? Node))
       (reduce #(if (instance? Node (first %2))
                  (into %1 %2)
                  (conj %1 (vec %2)))
               [])))


(defn sorted-edge-order
  "Sorts edges between two ordered layers first by their index in the source
  layer, and then by their index in the destination layer. Then, returns seq
  of [order edge] pairs of the edge targets in the destination layer, using
  this edge ordering"
  [ordered next-ordered graph-edges]
  (let [next-order-map (into {} (map-indexed #(-> [%2 %1]) next-ordered))
        ;; Edges between segment containers need to be counted as well
        ;; but they change with each new ordering, so we just temporarily
        ;; merge the current segment edges with the never-changing
        ;; node -> node edges
        edges (->> (filter vector? ordered)
                   (map (juxt identity
                              (partial mapcat #(-> % :characters))))
                   (map (fn [[seg-c characters]]
                          [seg-c #{(Edge. seg-c seg-c characters (count characters))}]))
                   (into {})
                   (merge graph-edges))]
    (->> ordered
         (mapcat (fn [item]
                   (sort-by #(get next-order-map (:dest %))
                            (get edges item))))
         (map #(-> [(get next-order-map (:dest %)) %])))))


(defn next-power-of-2
  "A helper for cross counting; the number of leaf nodes in the accumulator tree
  needs to be the first power of 2 greater than the number of nodes in one layer.
  This function sets all the bits to the right of the first bit set in a 32 bit
  number, and then increments, which is the same thing"
  [x]
  (loop [num x
         exp 0]
    (if (> exp 4)
      (inc num)
      (let [shifted (bit-shift-right num (.pow js/Math 2 exp))]
        (recur (bit-or num shifted)
               (inc exp))))))


(defn single-edge-super-crossings
  "Counts the number of crossings that result from adding an edge, in order,
  to the accumulator tree. If the index is even, meaning it's a right
  child of its parent, we increment its value. If the index is
  odd (left child), we add the value of its right sibling times the
  current weight to the cross count, as the right sibling represents
  weight of the edges that were added ahead of this one, and
  therefore, crossings. Then we walk up the tree to the root,
  incrementing and adding right siblings, for a total count of edges
  that cross this one"
  [tree orig-index edge]
  (let [weight (:weight edge)
        is-seg-c (vector? (:dest edge))]
    (loop [tree tree
           crossings 0
           marked #{}
           index orig-index]
      (if (zero? index)
        [tree crossings marked]
        (let [parent-index (quot (dec index) 2)
              ;; By kind of a coincidence, the index of a node's parent in our
              ;; mental model of the accumulator tree is the same as that node's
              ;; index (if it's a right child), or its sibling's index (if it's
              ;; a left child) in the compact representation of the tree
              real-right-index parent-index]
          (if (odd? index)
            (let [right-sib (get tree real-right-index)
                  c (+ crossings (* weight (:weight right-sib)))
                  ;; Segments cannot cross each other, so the potential hole
                  ;; in this logic is not actually a hole
                  m (if is-seg-c
                      (:node-edges right-sib)
                      (if (:is-seg-c right-sib)
                        ;; Add this edge and its reverse to marked, because
                        ;; we can go in either direction during later
                        ;; blockification
                        #{edge (rev edge)}
                        #{}))]
              (recur tree c (set/union marked m) parent-index))
            (let [t (-> (update-in tree [real-right-index :weight] + weight)
                        (cond->
                         is-seg-c (assoc-in [real-right-index :is-seg-c] true)
                         (not is-seg-c) (update-in [real-right-index :node-edges]
                                                   conj edge (rev edge))))]
              (recur t crossings marked parent-index))))))))


(defn count-and-mark-super-crossings
  "Counts crossings between separate nodes from one layer to the next"
  [minus-ps minus-qs preds succs]
  (let [[layer-1 layer-2 edges] (if (< (count minus-ps) (count minus-qs))
                                  [minus-qs minus-ps preds]
                                  [minus-ps minus-qs succs])
        num-acc-leaves (next-power-of-2 (count layer-2))
        first-leaf (dec num-acc-leaves)
        ;; Our compact representation has the same size as the index
        ;; of the first leaf in the mental model of the tree
        tree-size first-leaf]
    (loop [tree (vec (repeat tree-size (AccumulatorNode. 0 #{} false)))
           crossings 0
           marked #{}
           order (sorted-edge-order layer-1 layer-2 edges)]
      (if (empty? order)
        [crossings marked]
        (let [[ord edge] (first order)
              index (+ first-leaf ord)
              [t c m] (single-edge-super-crossings tree index edge)]
          (recur t (+ crossings c) (set/union marked m) (rest order)))))))


(defn single-edge-sub-crossings
  "Counts the sub-crossings that result from adding a single sub-edge
  to the accumulator tree"
  ([tree index]
     (single-edge-sub-crossings tree index 0))
  ([tree index crossings]
     (if (zero? index)
       [tree crossings]
       (let [parent-index (quot (dec index) 2)
             real-right-index parent-index]
         (if (odd? index)
           (recur tree parent-index (+ crossings (get tree real-right-index)))
           (recur (update-in tree [real-right-index] inc) parent-index crossings))))))


(defn count-sub-crossings-single-node
  "Counts sub-crossings for a node; short circuits if there are fewer
  than two successor nodes"
  [node dests]
  (let [num-dests (count dests)]
    (if (< num-dests 2)
      0
      (let [order-map (->> (map-indexed (fn [idx n]
                                          (map #(-> [% idx])
                                               (:characters n)))
                                        dests)
                           (reduce into {}))
            num-acc-leaves (next-power-of-2 num-dests)
            first-leaf (dec num-acc-leaves)
            tree-size first-leaf]
        (loop [tree (vec (repeat tree-size 0))
               sub-crossings 0
               order (map #(get order-map %) (:characters node))]
          (if (empty? order)
            sub-crossings
            (let [ord (first order)
                  index (+ first-leaf ord)
                  [t c] (single-edge-sub-crossings tree index)]
              (recur t (+ sub-crossings c) (rest order)))))))))


(defn count-sub-crossings
  "A modification of ESK. In our scheme, each node in the graph
  represents a set of characters, and those characters have an order
  within their node. If those characters then diverge to different
  successor nodes, then depending on their relative positions and
  the positions of the successors, their lines may cross in a way
  that is not detected by the coarser-grained node-by-node cross
  counting. We apply the same methodology here, but without needing
  to deal with crossing segments, or with edge weight"
  [minus-ps minus-qs succs]
  (loop [nodes (filter #(instance? Node %) minus-ps)
         crossings 0]
    (if (empty? nodes)
      crossings
      (let [node (first nodes)
            dest-set (into #{} (map :dest (get succs node)))
            dests (filter #(contains? dest-set %) minus-qs)
            c (count-sub-crossings-single-node node dests)]
        (recur (rest nodes) (+ crossings c))))))


(defn count-and-mark-crossings
  "Step 5 of ESK, counts the number of crossings that result from a
  bi-layer ordering. Implements the algorithm found in Bilayer Cross
  Counting, by Wilhelm Barth, Petra Mutzel and Michael Jünger

  I've made the following modifications:
  - Counting inter-node crossings and intra-node crossings. For inter-node
    crossings, edges that originate from separate nodes in one layer and
    arrive at separate nodes in the next layer may cross each other. These
    is what is normally counted in layered graph drawing. However, for
    this application, those coarse edges may be composed of several finer
    sub-edges, and the sub-edges may cross each other as well, depending on
    their initial ordering and the ordering of their destination nodes. We
    can use basically the same algorithm at a smaller scale to count these
    crossings as well
  - Added weights to the inter-node edges
  - Marking inter-node edges that cross segment containers, which helps
    in the layout process later, by:
    - Storing accumulated node -> node edges in the accumulator tree
    - Storing a flag for whether a node has seen a segment container in the
      accumulator tree
  - Only storing information on right siblings, as the information on the root
    or left siblings is never accessed
  - Due to not storing information on the root or left siblings, we can store
    everything in a compact representation of the tree that is one less than
    half the size of the mental model of the tree, by only accounting for
    right siblings. This leads to a few coincidences, namely, the size of the
    compact representation is the same as the index of the first leaf in the
    expanded tree, and the index of a right sibling in the compact
    representation is the same as that node's parent in the expanded tree"
  [minus-ps minus-qs preds succs]
  (let [[sup-crossings marked] (count-and-mark-super-crossings minus-ps minus-qs preds succs)
        sub-crossings (count-sub-crossings minus-ps minus-qs succs)]
    [(+ sup-crossings sub-crossings) marked]))


(defn flip-graph
  "Flips a graph along the axis perpendicular to the layers, so that nodes and
  edges within a layer reverse their order, more or less. This doesn't touch
  some of the attributes of a layer that give us an ordering, but only the maps
  that we need to touch during coordinate assignment"
  [graph]
  (assoc graph
    :aboves (:belows graph)
    :belows (:aboves graph)
    :top-idxs (:bot-idxs graph)
    :bot-idxs (:top-idxs graph)))


(defn SparseGraph->OrderedGraph
  "Performs one layer-by-layer sweep of the graph using ESK's algorithm"
  [sparse-graph first-layer]
  (let [{:keys [ps qs preds succs]} sparse-graph]
    (loop [ordered-graph (map->OrderedGraph {:layers [first-layer]
                                             :succs succs
                                             :preds preds
                                             :ps ps
                                             :qs qs
                                             :minus-ps []
                                             :minus-qs []})
           prev-layer first-layer
           layers (-> sparse-graph :layers rest)]
      (if (empty? layers)
        ordered-graph
        (let [minus-ps (replace-ps (:items prev-layer) ps succs)
              sparse-layer (first layers)
              positions (set-positions minus-ps)
              [qs non-qs] (map set
                               ((juxt filter remove) #(contains? qs %)
                                (:nodes sparse-layer)))
              measures (set-measures non-qs preds positions)
              minus-qs (merge-layer minus-ps positions non-qs measures)
              items (add-qs minus-qs qs)
              ordered-layer (OrderedLayer. (:id sparse-layer)
                                           (:duration sparse-layer)
                                           items)
              g (-> ordered-graph
                    (update-in [:layers] conj ordered-layer)
                    (update-in [:minus-ps] conj minus-ps)
                    (update-in [:minus-qs] conj minus-qs))]
          (recur g ordered-layer (rest layers)))))))


(defn SparseGraph->ordered-graphs
  "Implements the 2-layer crossing minimization algorithm on a sparse graph found in
  'An Efficient Implementation of Sugiyama’s Algorithm for Layered Graph Drawing',
  a paper by Markus Eiglsperger, Martin Sieberhaller, and Michael Kaufmann (ESK)

  I've made the following modifications:
  - Removed the concept of alternating layers, which don't help much, and hurt a bit
  - Added weights to nodes and edges
  - Added short-circuiting if we've seen a seed layer before
  - Now that short-circuiting doesn't rely on crossing counts, I pulled counting
    crossings and marking edges out of the ordering algorithm. This allows the
    counting and marking to be parallelized, whereas the ordering is inherently
    serial."
  ([sparse-graph]
     (SparseGraph->ordered-graphs sparse-graph 20))
  ([sparse-graph max-sweeps]
     (let [layers (:layers sparse-graph)
           first-sparse-layer (first layers)
           last-layer-idx (dec (count layers))]
       (loop [orderings []
              ;; seed the first layer with initial ordered layer
              first-layer (map->OrderedLayer
                           (-> first-sparse-layer
                               (dissoc :nodes)
                               (assoc :items (:nodes first-sparse-layer))))
              first-layers #{}]
         (let [c (count orderings)]
           ;; Graph orderings are determined by the first layer. If we've seen this
           ;; first layer before, then we can be sure that we're about to enter
           ;; a cycle, and can thus short circuit
           (if (or (= c max-sweeps) (contains? first-layers first-layer))
             orderings
             (let [reverse? (odd? c)
                   ordered-graph (-> (if reverse?
                                       (rev sparse-graph)
                                       sparse-graph)
                                     (SparseGraph->OrderedGraph first-layer))
                   ords (conj orderings (if reverse?
                                          (rev ordered-graph)
                                          ordered-graph))
                   layers (:layers ordered-graph)]
               ;; The last layer of the current ordering is the first layer
               ;; of the next
               (recur ords (layers last-layer-idx) (conj first-layers first-layer)))))))))


(defn OrderedGraph->CountedAndMarkedGraph
  "Takes an OrderedGraph, counts edge crossings and marks edges that should not
  be drawn straight, and returns a new graph with this information"
  [ordered-graph]
  (let [{:keys [minus-ps minus-qs preds succs]} ordered-graph
        [crossings marked] (->> (map #(count-and-mark-crossings
                                       %1 %2 preds succs)
                                     minus-ps minus-qs)
                                (apply map vector))]
    (map->CountedAndMarkedGraph (assoc ordered-graph
                                      :crossings (reduce + crossings)
                                      :marked (reduce set/union marked)))))


(defn best-ordering
  "Chooses the graph with the fewest crossings from a collection of OrderedGraphs"
  [ordered-graphs]
  (->> (map OrderedGraph->CountedAndMarkedGraph ordered-graphs)
       (sort-by #(-> [(:crossings %) (count (:marked %))]))
       first))


(defn neighborify
  "Maps things in a layer (nodes and segments) to the things that are directly
  above or below"
  [layer]
  (loop [aboves {}
         belows {}
         o (-> layer :items first)
         os (-> layer :items rest)]
    (if (empty? os)
      [aboves belows]
      (let [next-o (first os)
            a (assoc aboves next-o o)
            b (assoc belows o next-o)]
        (recur a b next-o (rest os))))))


(defn indexify
  "Indexes each layer by Node and Segment, from the top and bottom"
  [layer]
  (let [items (:items layer)
        len (count items)]
    (->> (map-indexed #(-> [[%2 %1] [%2 (- len %1 1)]]) items)
         (apply map vector)
         (map #(into {} %)))))


(defn OrderedLayer->FlatLayer
  "An OrderedLayer is composed of Nodes and vectors of Edges. This function
  transforms an OrderedLayer into a FlatLayer composed of Nodes and Segments"
  [ordered-layer]
  (map->FlatLayer (assoc ordered-layer
                    :items (map #(if (instance? Edge %)
                                   (Edge->Segment % (:id ordered-layer))
                                   %)
                                (-> ordered-layer :items flatten)))))


(defn CountedAndMarkedGraph->FlatGraph
  "Transforms each OrderedLayer into a FlatLayer, and calculates attributes
  that are useful for organizing Nodes and Segments into horizontally-
  aligned blocks"
  [cm-graph]
  (let [layers (into [] (map OrderedLayer->FlatLayer (:layers cm-graph)))
        [aboves belows] (apply map merge (map neighborify layers))
        [top-idxs bot-idxs] (apply map merge (map indexify layers))]
    (map->FlatGraph {:layers layers
                     :succs (:succs cm-graph)
                     :preds (:preds cm-graph)
                     :marked (:marked cm-graph)
                     :aboves aboves
                     :belows belows
                     :top-idxs top-idxs
                     :bot-idxs bot-idxs})))


(defn check-alignment
  "Checks whether a predecessor is a valid alignment candidate"
  [pred last-idx marked]
  (if (and (< last-idx (:idx pred))
           (not (contains? marked (:edge pred))))
    pred))


(defn pred->segs+src
  "Takes a predecessor Edge and returns a vector of Segments, one for each layer
  this Edge crosses, plus the source Node"
  [pred]
  (let [src (:src pred)
        dest (:dest pred)
        src-layer (:layer-id src)
        dest-layer (:layer-id dest)
        segs (if (> src-layer dest-layer)
               (map (partial Edge->Segment pred)
                    (range (inc dest-layer) src-layer))
               (map (partial Edge->Segment pred)
                    (range (dec dest-layer) src-layer -1)))]
    (conj (into [] segs) src)))


(defn blockify-layer
  "Assign every node in a layer to an already-existing block, or begin a new
  block with it"
  [layer pred-layer roots blocks preds top-idxs marked]
  (let [pred-layer-id (:id pred-layer)]
    (loop [nodes (filter #(instance? Node %) (:items layer))
           last-idx -1
           roots roots
           blocks blocks]
      (if (empty? nodes)
        [roots blocks]
        (let [node (first nodes)
              preds (->> (get preds node)
                         (map #(if (= pred-layer-id (-> % :dest :layer-id))
                                 {:edge % :item (:dest %)}
                                 {:edge % :item (Edge->Segment % pred-layer-id)}))
                         (map #(assoc % :idx (get top-idxs (:item %))))
                         (sort-by :idx)
                         (mapcat #(repeat (-> % :edge :weight) %)))
              num-preds (count preds)
              median (quot (dec num-preds) 2)
              aligned (and (pos? num-preds)
                           (or (check-alignment (nth preds median) last-idx marked)
                               ;; If we kind of have two medians because there are an even number
                               ;; of predecessors, then we allow the "second" median to be used
                               ;; if the first isn't available
                               (and (even? num-preds)
                                    (check-alignment (nth preds (inc median)) last-idx marked))))
              [root items idx] (if aligned
                                 ;; If we have an alignment, then we have the same root, and add this
                                 ;; node to that root's block
                                 [(get roots (-> aligned :edge :dest))
                                  (pred->segs+src (:edge aligned))
                                  (:idx aligned)]
                                 ;; No aligned nodes means it's a new block root
                                 [node [node] last-idx])
              rs (reduce #(assoc-in %1 [%2] root) roots items)
              bs (update-in blocks [root] (fnil rrb/catvec []) items)]
          (recur (rest nodes) idx rs bs))))))


(defn blockify
  "Associate nodes with their median-ly positioned parent if it exists, hasn't been
  aligned with by another node, and we haven't aligned with nodes of greater index
  in the past. Continue finding these alignments until we've assigned every node
  into a horizontally aligned block"
  [flat-graph]
  (loop [pred-layer nil
         layers (:layers flat-graph)
         roots {}
         blocks {}]
    (if (empty? layers)
      [roots blocks]
      (let [layer (first layers)
            [rs bs] (blockify-layer layer pred-layer roots blocks
                                    (:preds flat-graph) (:top-idxs flat-graph)
                                    (:marked flat-graph))]
        (recur layer (rest layers) rs bs)))))


(defn topo-sort
  "An implementation of a Kahn topological sort, cribbed with modification from
  https://gist.github.com/alandipert/1263783"
  [sources succs]
  (loop [sources sources
         succs succs
         sorted []]
    (if (empty? sources)
      sorted
      (let [node (first sources)
            dests (get succs node)
            succs' (dissoc succs node)
            all-dests (reduce set/union (vals succs'))
            sources' (apply conj (rest sources)
                            (set/difference dests all-dests))]
        (recur sources' succs' (conj sorted node))))))


(defn FlatGraph->BlockGraph
  "Organizes all of the Nodes and Segments in a FlatGraph into horizontally-
  aligned blocks. These blocks are then organized into a graph of their own,
  where the nodes are blocks and the edges are determined by adjacency
  between their constituent Nodes and Segments in each FlatLayer.

  This approach is very heavily based on Fast and Simple Horizontal Coordinate
  Assignment by Ulrik Brandes and Boris Köpf (BK), even though the algorithm
  is pretty much entirely dissimilar. I've taken BK's basic idea (which is to
  align nodes into 'blocks' group these blocks into 'classes' and then
  recursively position each class and block on the x axis), discarded the
  algorithm they provided, and made my own. Their algorithm operated on blocks
  and classes as sort of emergent properties from relationships between nodes,
  whereas mine explicity constructs blocks and classes and organizes each into
  graphs, which are then used for positioning. I just found my approach easier
  to reason about.

  It is also important to note that BK (and pretty much all of the academic
  literature, for that matter), is concerned with drawing graphs vertically,
  while we are drawing them horizontally. The practical result of this is that
  our graphs are reflected across the upper-left-to-lower-right diagonal as
  compared to the graphs in BK, and as a result we sometimes do things in the
  opposite manner. The most obvious example is that while BK-style classes are
  defined by their sinks in a block graph, ours are defined by sources in the
  block graph. If you flip the graph though, you'll see that we're working from
  the same block and getting the same result."
  [flat-graph]
  (let [[roots blocks] (blockify flat-graph)]
    (loop [bs blocks
           succs {}]
      (if (empty? bs)
        (let [block-set (into #{} (vals blocks))
              simple-succs (->> (map (fn [[src edges]]
                                       [src (into #{} (map :dest edges))])
                                     succs)
                                (into {}))
              all-succs (into #{} (reduce set/union (vals simple-succs)))
              long-block (first (filter #(> (count %) 1) block-set))
              layer-id-compare (if (< (-> long-block first :layer-id)
                                      (-> long-block second :layer-id))
                                 < >)
              sources (->> (set/difference block-set all-succs)
                           (sort-by #(:layer-id (get % 0))
                                    layer-id-compare))
              topo-blocks (topo-sort sources simple-succs)]
          (map->BlockGraph {:blocks topo-blocks :succs succs :sources sources}))
        (let [block (-> bs first second)  ; we want the value in the map
              b-succs (->> (map #(get-in flat-graph [:aboves %]) block)
                           (remove nil?)
                           (group-by #(get roots %))
                           (map (fn [[above-root above-nodes]]
                                  (BlockEdge.
                                   (get blocks above-root)
                                   block
                                   (apply max (map :weight above-nodes))))))
              ss (reduce #(update-in %1
                                     [(:src %2)]
                                     (fnil conj #{})
                                     %2)
                         succs
                         b-succs)]
          (recur (rest bs) ss))))))


(defn classify-source
  "Given a class that is a source in a ClassGraph, returns a set containing
  that source and all of its descendants"
  [source succs]
  (set/union #{source}
             (apply set/union
                    (map #(classify-source (:dest %) succs)
                         (get succs source)))))


(defn classify
  "Given a BlockGraph, organizes the blocks into classes that are defined as
  all blocks that are reachable from a block that is a source in its BlockGraph,
  with preference given to the left-most sources"
  [block-graph]
  (loop [sources (:sources block-graph)
         classes {}]
    (if (empty? sources)
      classes
      (let [root-block (first sources)
            proto-class (classify-source root-block (:succs block-graph))
            class (apply set/difference proto-class (vals classes))]
        (recur (rest sources)
               (reduce #(assoc %1 %2 class) classes class))))))


(defn BlockGraph->ClassGraph
  "Given a BlockGraph, organizes the blocks into classes and then constructs a
  ClassGraph, where the nodes are classes and the edges are BlockEdges that span
  classes. This means there can be multiple edges per class pair."
  [block-graph]
  (let [classes (classify block-graph)]
    (loop [cs (vals classes)
           succs {}]
      (if (empty? cs)
        (let [class-set (into #{} (vals classes))
              simple-succs (->> (map (fn [[src edges]]
                                       [src
                                        (into #{}
                                              (map #(get-in classes [(:dest %)])
                                                   edges))])
                                     succs)
                                (into {}))
              all-succs (reduce set/union (vals simple-succs))
              sources (set/difference class-set all-succs)
              topo-classes (topo-sort sources simple-succs)]
          (map->ClassGraph {:classes topo-classes :succs succs :sources sources}))
        (let [class (first cs)
              block-succs (->> (mapcat #(-> block-graph :succs (get %)) class)
                               (remove #(contains? class (:dest %)))
                               (into #{}))]
          (recur (rest cs) (assoc-in succs [class] block-succs)))))))
