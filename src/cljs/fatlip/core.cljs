(ns fatlip.core
  (:require [cljs.core.match :refer-macros [match]]
            [clojure.core.rrb-vector :as rrb]
            [clojure.set :as set]
            [clojure.string :as s]))


(defprotocol Reversible
  (rev [item] "It... reverses"))

(defprotocol Flippable
  (flip [item] "Flips a graph along the axis perpendicular to the layers, so
                that nodes and edges within a layer reverse their order"))

(defprotocol Nodey
  (nodes [this] "Returns constituent nodes as close to ordered as possible"))

(defprotocol YPlotted
  (min-y [graph] "Minimum y-coordinate in the graph")
  (max-y [graph] "Maximum y-coordinate in the graph")
  (mid-y [graph] "Midpoint of y-coordinates in the graph")
  (shift-y [graph delta] "Shift the y-coordinate of every node by delta")
  (width [graph] "Width of the graph in y"))

(defrecord Node [id layer-id characters weight])

(defrecord Edge [src dest characters weight]
  Reversible
  (rev [this]
    (assoc this
           :src dest
           :dest src)))

(defrecord Segment [endpoints layer-id characters weight])

(defrecord SparseGraph [layers succs preds ps qs rs characters]
  Reversible
  (rev [this]
    (assoc this
           :succs preds
           :preds succs
           :ps qs
           :qs ps
           :layers (vec (rseq layers)))))

(defrecord SparseLayer [id duration nodes])
(defrecord OrderedGraph [layers succs preds ps qs minus-ps minus-qs characters]
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
(defrecord CountedAndMarkedGraph [layers succs preds crossings marked characters]) ; still has OrderedLayers
(defrecord FlatGraph [layers succs preds aboves belows top-idxs bot-idxs crossings marked characters]
  Reversible
  (rev [this]
    (assoc this
           :succs preds
           :preds succs
           :layers (vec (rseq layers))))
  Flippable
  (flip [this]
    (assoc this
           :aboves belows
           :belows aboves
           :top-idxs bot-idxs
           :bot-idxs top-idxs))
  Nodey
  (nodes [this]
    (mapcat (fn [layer]
              (filter #(instance? Node %) (:items layer)))
            layers)))


(defrecord FlatLayer [id duration items])
(defrecord BlockGraph [blocks succs preds sources])
(defrecord BlockEdge [src dest weight]
  Reversible
  (rev [this]
    (assoc this
           :src dest
           :dest src)))

(defrecord ClassGraph [classes succs preds block-succs block-preds sources sinks]
  Reversible
  (rev [this]
    (assoc this
           :classes (vec (reverse (map #(vec (reverse %)) classes)))
           :succs preds
           :preds succs
           :block-succs block-preds
           :block-preds block-succs
           :sources sinks
           :sinks sources)))

(defrecord YPlottedClassGraph [classes ys char-sep]
  YPlotted
  (min-y [_]
    (apply min (map #(ys (ffirst %)) classes)))
  (max-y [_]
    (->> classes
         (mapcat peek)
         (map #(+ (ys %) (* char-sep (dec (:weight %)))))
         (apply max)))
  (width [this]
    (- (max-y this) (min-y this)))
  (shift-y [this delta]
    (assoc this :ys (into {}
                          (map (fn [[node y]]
                                 [node (+ y delta)])
                               ys)))))

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
        edge (Edge. last-node node (set characters) weight)]
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
        characters (:characters input)
        node (map->Node {:id node-id :layer-id layer-id
                         :characters (set characters) :weight weight})
        g (-> graph
              (update-in [:layers layer-id :nodes] conj node)
              (assoc-in [:characters node] characters))]
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
         characters (:characters node)]
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
    (reduce (fn [g character]
              (let [last-node (-> g :last-nodes-by-character character)
                    disappeared (contains? (-> node :path-mods character) :disappeared)]
                (cond-> g
                  disappeared (update-in [:last-nodes-by-character] dissoc character)
                  (not disappeared) (->
                                     (assoc-in [:last-nodes-by-character character] node)
                                     (update-in [:last-nodes-by-node (:id node)] (fnil conj #{}) character))
                  last-node (update-in [:last-nodes-by-node (:id last-node)] disj character))))
            (process-characters grph node)
            (-> grph :characters (get node)))))


(defn add-layer
  "Create the layer of our graph representation, and recursively
  create and add nodes from the groups in the input layer"
  [graph input-layer]
  (let [layers (:layers graph)
        layer-id (count layers)
        layer (SparseLayer. layer-id
                            (:duration input-layer)
                            [])]
    (reduce (fn [g input-group]
              (add-node g layer-id input-group))
            (update-in graph [:layers] conj layer)
            (input-layer :groups))))


(defn inp->SparseGraph
  "Creates a sparse graph, as defined in ESK"
  [input]
  (let [graph (reduce
               (fn [g i]
                 (let [dup (->> (mapcat #(:characters %) (:groups i))
                                frequencies
                                (filter (fn [[_ c]] (> c 1)))
                                first)]
                   (if dup
                     (throw (js/Error.
                             (str (get dup 0) " is specified "
                                  (get dup 1) " times in one layer")))
                     (add-layer g i))))
               (map->SparseGraph {:layers []
                                  :succs {}
                                  :preds {}
                                  :ps #{}
                                  :qs #{}
                                  :rs #{}
                                  :characters {}})
               input)]
    (if (empty? (:succs graph))
      (throw (js/Error. "At least one character must appear twice"))
      graph)))


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
  chose 0, which is implied by the description in ESK."
  [minus-ps]
  (into {} (reductions (fn [[last-item last-position] item]
                         (let [position (if (vector? last-item)
                                          (+ last-position (count last-item))
                                          (inc last-position))]
                           [item position]))
                       [(first minus-ps) 0]
                       (rest minus-ps))))


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
                              (partial mapcat :characters)
                              (partial map :weight)))
                   (map (fn [[seg-c characters weights]]
                          [seg-c #{(Edge. seg-c seg-c
                                          (set characters)
                                          (reduce + weights))}]))
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
  (inc (reduce (fn [num exp]
                 (let [shifted (bit-shift-right num (.pow js/Math 2 exp))]
                   (bit-or num shifted)))
               x
               (range 5))))


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
    (->> (sorted-edge-order layer-1 layer-2 edges)
         (reduce (fn [[crossings marked tree] [order edge]]
                   (let [index (+ first-leaf order)
                         [t c m] (single-edge-super-crossings tree index edge)]
                     [(+ crossings c) (set/union marked m) t]))
                 [0 #{} (vec (repeat tree-size
                                     (AccumulatorNode. 0 #{} false)))])
         (take 2))))


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
  [node dests characters]
  (let [num-dests (count dests)]
    (if (< num-dests 2)
      0
      (let [order-map (->> (map-indexed (fn [idx n]
                                          (map #(-> [% idx])
                                               (get characters n)))
                                        dests)
                           (reduce into {}))
            num-acc-leaves (next-power-of-2 num-dests)
            first-leaf (dec num-acc-leaves)
            tree-size first-leaf]
        (->> (get characters node)
             (map #(get order-map %))
             (reduce (fn [[sub-crossings tree] order]
                       (let [index (+ first-leaf order)
                             [t c] (single-edge-sub-crossings tree index)]
                         [(+ sub-crossings c) t]))
                     [0 (vec (repeat tree-size 0))])
             first)))))


(defn count-sub-crossings
  "A modification of ESK. In our scheme, each node in the graph
  represents a set of characters, and those characters have an order
  within their node. If those characters then diverge to different
  successor nodes, then depending on their relative positions and
  the positions of the successors, their lines may cross in a way
  that is not detected by the coarser-grained node-by-node cross
  counting. We apply the same methodology here, but without needing
  to deal with crossing segments, or with edge weight"
  [minus-ps minus-qs succs characters]
  (->> minus-ps
       (filter #(instance? Node %))
       (reduce (fn [crossings node]
                 (let [dest-set (set (map :dest (get succs node)))
                       dests (filter #(contains? dest-set %) minus-qs)
                       c (count-sub-crossings-single-node node
                                                          dests
                                                          characters)]
                   (+ crossings c)))
               0)))


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
  [minus-ps minus-qs preds succs characters]
  (let [[sup-crossings marked] (count-and-mark-super-crossings minus-ps minus-qs
                                                               preds succs)
        sub-crossings (count-sub-crossings minus-ps minus-qs succs characters)]
    [(+ sup-crossings sub-crossings) marked]))


(defn SparseGraph->OrderedGraph
  "Performs one layer-by-layer sweep of the graph using ESK's algorithm"
  [sparse-graph first-layer]
  (let [{:keys [ps qs preds succs characters layers]} sparse-graph
        [ordered-layers minus-ps minus-qs]
        (reduce (fn [[layers minus-ps minus-qs prev-layer] sparse-layer]
                  (let [minus-p (replace-ps (:items prev-layer) ps succs)
                        positions (set-positions minus-p)
                        [qs non-qs] (map set
                                         ((juxt filter remove)
                                          #(contains? qs %)
                                          (:nodes sparse-layer)))
                        measures (set-measures non-qs preds positions)
                        minus-q (merge-layer minus-p positions
                                             non-qs measures)
                        items (add-qs minus-q qs)
                        ordered-layer (OrderedLayer. (:id sparse-layer)
                                                     (:duration sparse-layer)
                                                     items)]
                    [(conj layers ordered-layer)
                     (conj minus-ps minus-p)
                     (conj minus-qs minus-q)
                     ordered-layer]))
                [[first-layer] [] [] first-layer]
                (rest layers))]
    (map->OrderedGraph {:layers ordered-layers
                        :minus-ps minus-ps
                        :minus-qs minus-qs
                        :succs succs
                        :preds preds
                        :ps ps
                        :qs qs
                        :characters characters})))


(defn get-ordered
  [node preds characters prev-l-characters]
  (let [pred-nodes (get preds node)
        node-characters (:characters node)
        ordered (condp = (count pred-nodes)
                  0 (get characters node)
                  1 (vec (filter #(contains? node-characters %)
                                 (get characters (:dest (first pred-nodes)))))
                  (vec (filter #(contains? node-characters %)
                               prev-l-characters)))]
    (if (= (count ordered) (count node-characters))
      ordered
      (into ordered (set/difference node-characters (set ordered))))))


(defn order-subnodes
  [co-graph]
  (let [{:keys [layers characters preds]} co-graph
        sorted
        (->> (rest layers)
             (reduce (fn [[sorted prev-l] layer]
                       (let [prev-l-characters (->> (:items prev-l)
                                                    (remove vector?)
                                                    (mapcat #(get sorted %)))
                             s (merge
                                sorted
                                (->> (:items layer)
                                     (remove vector?)
                                     (map (fn [n]
                                            [n (get-ordered
                                                n preds sorted
                                                prev-l-characters)]))
                                     (into {})))]
                         [s layer]))
                     [characters (first layers)])
             first)]
    (assoc co-graph :characters sorted)))


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
  [sparse-graph & {:keys [max-sweeps] :or {max-sweeps 20}}]
  (let [layers (:layers sparse-graph)
        last-layer-idx (dec (count layers))
        first-sparse-layer (first layers)
        ;; seed the first layer with initial ordered layer
        first-ordered-layer (map->OrderedLayer
                             (-> first-sparse-layer
                                 (dissoc :nodes)
                                 (assoc :items (:nodes first-sparse-layer))))]
    (->>
     (range max-sweeps)
     (reduce (fn [[orderings first-layers first-layer] c]
               ;; Graph orderings are determined by the first layer. If we've
               ;; seen this first layer before, then we can be sure that we're
               ;; about to enter a cycle, and can thus short circuit
               (if (contains? first-layers first-layer)
                 (reduced [orderings first-layers first-layer])
                 (let [reverse? (odd? c)
                       graph (-> (if reverse?
                                   (rev sparse-graph)
                                   sparse-graph)
                                 (SparseGraph->OrderedGraph first-layer))
                       backward-graph (if reverse?
                                        graph
                                        (rev graph))
                       ;; This is sort of ugly, but once we order the nodes,
                       ;; we still have to come up with a good subnode
                       ;; ordering. Conceptually, this belongs in
                       ;; SparseGraph->OrderedGraph, but it's also
                       ;; conceptually cromulent to have
                       ;; SparseGraph->OrderedGraph know nothing about the
                       ;; forward/reverse dance that we do here. However,
                       ;; subnode ordering is dependent on direction. My
                       ;; choice then, is to make  SparseGraph->OrderedGraph
                       ;; direction-aware, or pull the subnode ordering out
                       ;; and put it here, where we're aware of the direction.
                       ;; For now, I've chosen the latter.
                       backward-subnode-pass (order-subnodes backward-graph)
                       forward-subnode-pass (order-subnodes
                                             (rev backward-subnode-pass))
                       layers (:layers graph)]
                   ;; The last layer of the current ordering is the first layer
                   ;; of the next
                   [(conj orderings forward-subnode-pass)
                    (conj first-layers first-layer)
                    (layers last-layer-idx)])))
             [[] #{} first-ordered-layer])
     first)))


(defn OrderedGraph->CountedAndMarkedGraph
  "Takes an OrderedGraph, counts edge crossings and marks edges that should not
  be drawn straight, and returns a new graph with this information"
  [ordered-graph]
  (let [{:keys [minus-ps minus-qs preds succs characters]} ordered-graph
        [crossings marked] (->> (map #(count-and-mark-crossings
                                       %1 %2 preds succs characters)
                                     minus-ps minus-qs)
                                (apply map vector))]
    (map->CountedAndMarkedGraph (assoc ordered-graph
                                       :crossings (reduce + crossings)
                                       :marked (reduce set/union marked)))))


(defn best-ordering
  "Chooses the graph with the fewest crossings from a collection of OrderedGraphs"
  [ordered-graphs]
  (->> (map OrderedGraph->CountedAndMarkedGraph ordered-graphs)
       (sort-by (fn [g]
                  [(:crossings g)
                   (reduce #(+ %1 (:weight %2)) 0 (:marked g))]))
       first))


(defn neighborify
  "Maps things in a layer (nodes and segments) to the things that are directly
  above or below"
  [layer]
  (->> (-> layer :items rest)
       (reduce (fn [[aboves belows last-item] item]
                 [(assoc aboves item last-item)
                  (assoc belows last-item item)
                  item])
               [{} {} (-> layer :items first)])
       (take 2)))


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
  (let [{:keys [layers succs preds marked crossings characters]} cm-graph
        cm-layers (vec (map OrderedLayer->FlatLayer layers))
        [aboves belows] (apply map merge (map neighborify cm-layers))
        [top-idxs bot-idxs] (apply map merge (map indexify cm-layers))]
    (map->FlatGraph {:layers cm-layers
                     :succs succs
                     :preds preds
                     :aboves aboves
                     :belows belows
                     :top-idxs top-idxs
                     :bot-idxs bot-idxs
                     :marked marked
                     :crossings crossings
                     :characters characters})))


(defn check-alignment
  "Checks whether a predecessor is a valid alignment candidate"
  [pred last-idx marked]
  (when (and (< last-idx (:idx pred))
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
    (conj (vec segs) src)))


(defn blockify-layer
  "Assign every node in a layer to an already-existing block, or begin a new
  block with it"
  [layer pred-layer roots blocks all-preds top-idxs marked]
  (->> (:items layer)
       (filter #(instance? Node %))
       (reduce
        (fn [[roots blocks last-idx] node]
          (let [pred-layer-id (:id pred-layer)
                preds (->> (get all-preds node)
                           (map #(if (= pred-layer-id (-> % :dest :layer-id))
                                   {:edge % :item (:dest %)}
                                   {:edge %
                                    :item (Edge->Segment % pred-layer-id)}))
                           (map #(assoc % :idx (get top-idxs (:item %))))
                           (sort-by :idx)
                           (mapcat #(repeat (-> % :edge :weight) %)))
                num-preds (count preds)
                median (quot (dec num-preds) 2)
                aligned (and (pos? num-preds)
                             (or (check-alignment (nth preds median)
                                                  last-idx marked)
                                 ;; If we kind of have two medians because
                                 ;; there are an even number of predecessors,
                                 ;; then we allow the "second" median to be
                                 ;; used if the first isn't available
                                 (and (even? num-preds)
                                      (check-alignment
                                       (nth preds (inc median))
                                       last-idx marked))))
                [root items idx] (if aligned
                                   ;; If we have an alignment, then we have
                                   ;; the same root, and add this node to that
                                   ;; root's block
                                   [(get roots (-> aligned :edge :dest))
                                    (pred->segs+src (:edge aligned))
                                    (:idx aligned)]
                                   ;; No aligned nodes means it's a new block
                                   ;; root
                                   [node [node] last-idx])
                rs (reduce #(assoc-in %1 [%2] root) roots items)
                bs (update-in blocks [root] (fnil rrb/catvec []) items)]
            [rs bs idx]))
        [roots blocks -1])
       (take 2)))


(defn blockify
  "Associate nodes with their median-ly positioned parent if it exists, hasn't been
  aligned with by another node, and we haven't aligned with nodes of greater index
  in the past. Continue finding these alignments until we've assigned every node
  into a horizontally aligned block"
  [flat-graph]
  (take 2 (reduce (fn [[roots blocks pred-layer] layer]
                    (let [[rs bs] (blockify-layer layer pred-layer roots blocks
                                                  (:preds flat-graph)
                                                  (:top-idxs flat-graph)
                                                  (:marked flat-graph))]
                      [rs bs layer]))
                  [{} {} nil]
                  (:layers flat-graph))))


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
  between their constituent Nodes and Segments in each FlatLayer."
  [flat-graph]
  (let [[roots blocks] (blockify flat-graph)
        edge->set #(update-in %1 [(:src %2)] (fnil conj #{}) %2)
        [succs preds]
        (reduce (fn [[succs preds] block-item]
                  (let [block (second block-item) ; we want the value
                        b-succs (->> (map #(get-in flat-graph [:aboves %])
                                          block)
                                     (remove nil?)
                                     (group-by #(get roots %))
                                     (map (fn [[above-root above-nodes]]
                                            (BlockEdge.
                                             (get blocks above-root)
                                             block
                                             (apply max (map :weight
                                                             above-nodes))))))
                        b-preds (map rev b-succs)]
                    [(reduce edge->set succs b-succs)
                     (reduce edge->set preds b-preds)]))
                [{} {}]
                blocks)
        simple-succs (->> (map (fn [[src edges]]
                                 [src (set (map :dest edges))])
                               succs)
                          (into {}))
        block-set (set (vals blocks))
        long-block (first (filter #(> (count %) 1) block-set))
        layer-id-compare (if (< (-> long-block first :layer-id)
                                (-> long-block second :layer-id))
                           < >)
        sources (->> (set/difference block-set (keys preds))
                     (sort-by #(:layer-id (get % 0))
                              layer-id-compare))
        topo-blocks (topo-sort sources simple-succs)]
    (map->BlockGraph {:blocks topo-blocks :succs succs
                      :preds preds :sources sources})))


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
  (reduce (fn [classes root-block]
            (let [proto-class (classify-source root-block (:succs block-graph))
                  class-set (apply set/difference proto-class (vals classes))
                  class (vec (filter #(contains? class-set %)
                                     (:blocks block-graph)))]
              (reduce #(assoc %1 %2 class) classes class)))
          {}
          (:sources block-graph)))


(defn BlockGraph->ClassGraph
  "Given a BlockGraph, organizes the blocks into classes and then constructs a
  ClassGraph, where the nodes are classes and the edges are BlockEdges that span
  classes. This means there can be multiple edges per class pair."
  [block-graph]
  (let [block-classes (classify block-graph)
        classes (set (vals block-classes))
        edge->set #(update-in %1 [(get block-classes (:src %2))]
                              (fnil conj #{}) %2)
        [succs preds]
        (reduce (fn [[succs preds] class]
                  (let [class-set (set class)
                        block-succs (->> class
                                         (mapcat #(-> block-graph :succs
                                                      (get %)))
                                         (remove #(contains? class-set
                                                             (:dest %)))
                                         set)
                        block-preds (map rev block-succs)]
                    [(reduce edge->set succs block-succs)
                     (reduce edge->set preds block-preds)]))
                [{} {}]
                classes)
        simple-succs (->> succs
                          (map (fn [[src edges]]
                                 [src
                                  (set (map #(get-in
                                              block-classes [(:dest %)])
                                            edges))]))
                          (into {}))
        sources (set/difference classes (keys preds))
        sinks (set/difference classes (keys succs))
        topo-classes (topo-sort sources simple-succs)]
    (map->ClassGraph {:classes topo-classes
                      :succs succs :preds preds
                      :sources sources :sinks sinks
                      :block-succs (:succs block-graph)
                      :block-preds (:preds block-graph)})))


(defn calc-rel-ys
  "An abstraction for processing a topologically-sorted seq of nodes
  (blocks or classes in our case) and calcluating y-values for each.
  Higher order; takes a function that generates a function for mapping,
  and returns a function that returns a map of y-values"
  [map-gen-fn get-preds]
  (fn [items preds node-sep char-sep]
    (let [filter-set (set items)]
      (reduce (fn [ys item]
                (let [pred-ys (map (map-gen-fn ys char-sep)
                                   (get-preds preds item filter-set))
                      item-y (if (empty? pred-ys)
                               0
                               (+ node-sep (apply max pred-ys)))]
                  (assoc ys item item-y)))
              {}
              items))))


(def get-rel-ys
  "Takes topologically-sorted blocks from a single class, plus preds and minimum
  separation between nodes and subnodes, and returns a map of blocks to relative
  y-positions within the class"
  (calc-rel-ys
   (fn [ys char-sep]
     #(+ (get ys (:dest %))
         (* char-sep (dec (:weight %)))))
   (fn [preds item filter-set]
     (filter #(contains? filter-set (:dest %)) (get preds item)))))


(defn gen-get-shift-ys
  "Takes a map of blocks to relative y-positions within classes, and returns
  a function that returns a map of classes to their relative y-positions"
  [rel-ys]
  (calc-rel-ys (fn [ys char-sep]
                 #(- (+ (get ys (:dest %))
                        (get rel-ys (:dest %))
                        (* char-sep (dec (:weight %))))
                     (get rel-ys (:src %))))
               (fn [preds item _] (get preds item))))


(defn ClassGraph->YPlottedClassGraph
  "Takes a class graph, minimum node separation, and minimum sub-node separation
  and returns a map of nodes to y-positions"
  [class-graph node-sep char-sep]
  (let [{:keys [classes preds block-preds]} class-graph
        rel-ys (reduce #(merge %1 (get-rel-ys %2 block-preds node-sep char-sep))
                       {}
                       classes)
        shift-ys (let [get-shift-ys (gen-get-shift-ys rel-ys)]
                   (get-shift-ys classes preds node-sep char-sep))
        block-shift-ys (into {} (mapcat (fn [[class shift]]
                                          (map #(-> [% shift]) class))
                                        shift-ys))
        block-ys (map (fn [[block rel-y]]
                        [block (+ rel-y (get block-shift-ys block))])
                      rel-ys)
        ys (into {} (mapcat (fn [[block y]]
                              (->> (filter #(instance? Node %) block)
                                   (map #(-> [% y]))))
                            block-ys))]
    (map->YPlottedClassGraph {:ys ys
                              :classes classes
                              :char-sep char-sep})))


(def FlatGraph->ClassGraph
  (comp BlockGraph->ClassGraph
        FlatGraph->BlockGraph))


(defn FlatGraph->y-graphs
  "Takes a FlatGraph and creates four variations of it by flipping
  and reversing, and then plots the y-positions for nodes in each
  of these variations, and returns them"
  [flat-graph node-sep char-sep]
  (let [flipped (flip flat-graph)
        reversed (rev flat-graph)
        flipped-reversed (flip reversed)]
    (->> [flat-graph flipped reversed flipped-reversed]
         (map FlatGraph->ClassGraph)
         (map-indexed #(if (odd? %1) (rev %2) %2))
         (map #(ClassGraph->YPlottedClassGraph % node-sep char-sep)))))


(defn avg-y-graphs
  "Takes four YPlottedClassGraphs, normalizes them to the narrowest one,
  and returns a map of nodes to the average median of the y-positions in
  the given graphs"
  [y-graphs]
  (let [narrowest (apply min-key width y-graphs)
        narrow-min-y (min-y narrowest)
        narrow-max-y (max-y narrowest)]
    (->> y-graphs
         (map-indexed #(let [delta (if (odd? %1)
                                     (- narrow-max-y (max-y %2))
                                     (- narrow-min-y (min-y %2)))]
                         (shift-y %2 delta)))
         (map :ys)
         (apply merge-with #(if (vector? %1) (conj %1 %2) [%1 %2]))
         (map (fn [[node ys]]
                (let [sort-ys (sort ys)]
                  [node (/ (+ (nth sort-ys 1) (nth sort-ys 2)) 2)])))
         (into {}))))


(defn FlatGraph->node-ys
  "Given a FlatGraph, generate four variations by flipping and reversing, then
  construct ClassGraphs for each, which are used to assign y-values for each
  node in each graph variation. The four y-positions for each node are then
  averaged together to give a final value.

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
  block graph. If you reflect the graph though, you'll see that we're working
  from the same block and getting the same result."
  [flat-graph node-sep char-sep]
  (let [y-graphs (FlatGraph->y-graphs flat-graph node-sep char-sep)]
    (avg-y-graphs y-graphs)))


(defn arc-distance
  "Calculates the distance traveled by an arc given the radius of that arc
  and the the radius slope, or the slope of the tangent to that radius"
  [radius slope]
  {:pre [(every? number? [radius slope])]}
  (/ radius (.sqrt js/Math (+ 1 (* slope slope)))))


(defn arc-x-distance
  "Calculates the x-distance covered by an arc given that arc's radius
  and slope"
  [radius radius-slope]
  {:pre [(every? number? [radius radius-slope])]}
  (arc-distance radius radius-slope))


(defn arc-y-distance
  "Calculates the y-distance covered by an arc given that arc's radius
  and slope"
  [radius radius-slope]
  {:pre [(every? number? [radius radius-slope])]}
  (arc-distance radius (/ 1 radius-slope)))


(defn layer-x-distance
  "Came up with this formula in the following manner: we have the y-coordinates
  of two circles and the slope of the line that is internally tangent to both.
  We want to find the horizontal distance between the circle centers for this
  system. This distance can be divided up into three parts: the x-distance
  between the center of the first circle and its tangent point, the x-distance
  between the tangent points on each circle, and the x-distance between the
  second tangent point and the center of the second circle.

  The central insight here is that because we know the slope of the tangent
  (max-slope, or m), we also know that the slope of the radius between the center
  of each circle and their respective tangent points is -1/m due to the radius
  and the tangent being perpendicular. I've just used 1/m because the term ends up
  getting squared anyway. Put another way, since we're only interested in x values,
  it doesn't matter whether the slope of the tangent is positive or negative as
  long as it rises or falls at the same rate. Given this, we can solve this system of
  equations to get formulas for the tangent point in terms of the slope and radius:
  y - y_1 = (1/m) (x - x_1)         ; the point-slope forumla
  (x - x_1)^2 + (y - y_1)^2 = r^2     ; formula for a circle
  where (x_1, y_1) is the center of a circle and r is the length of the radius.

  You solve the point-slope formula in terms of x and y (separately) and substitute
  in the circle formula to get the x-value of the tangent point.
  Solving point-slope for y and substituting gets you the y-value.

  m(y - y_1) = x - x_1
  x = m(y - y_1) + x_1

  substituting into the circle formula:

  (m(y - y_1) + x_1 - x_1)^2 + (y - y_1)^2 = r^2
  m^2(y - y_1)^2 + (y - y_1)^2 = r^2
  (y - y_1)^2 (m^2 + 1) = r^2
  (y - y_1)^2 = r^2 / (m^2 + 1)
  y - y_1 = ±r / √(m^2 + 1)
  y = y_1 ± r / √(m^2 + 1)

  Solving point-slope for x is almost exactly the same, and substituting gives you
  x = x_1 ± r / √((1 / m^2) + 1)

  Since we're really only interested in the relative horizontal distances, and we
  know that the contribution from the arcs is positive for both circles, we can
  simplify to:
  x = r / √((1 / m^2) + 1)
  y = y_1 ± r / √(m^2 + 1)

  Then since we want the values for both circles, we do this:

  arcs dx = (r_1 / √((1 / m^2) + 1)) + (r_2 / √((1 / m^2) + 1))
  arcs dx = (r_1 + r_2) / √((1 / m^2) + 1)

  Now that we have the contribution from the arcs, we only need to figure out the
  horizontal distance that the tangent itself covers. This is pretty easy; for a
  straight line, we know that dx = dy / m, where dx is the horizontal distance
  and dy is the vertical distance.  We also know that the vertical distance of
  the tangent line is equal to the distance between the circle centers, plus the
  vertical distance from each center to the respective tangent point. Following
  the same logic as above, we can derive that:

  arcs dy = (r_1 + r_2) / √(m^2 + 1)

  Then all we need to do is add the distance between centers and divide the whole
  thing by m:

  tangent dx = (|y_1 - y_2| + (r_1 + r_2) / √(m^2 + 1)) / m.

  Putting it all together:

  total dx = ((r_1 + r_2) / √((1 / m^2) + 1)) + ((|y_1 - y_2| + (r_1 + r_2) / √(m^2 + 1)) / m)"
  [max-slope sum-radii centers-y-dist]
  {:pre [(every? number? [max-slope sum-radii centers-y-dist])]}
  (let [radius-slope (/ 1 max-slope)
        arcs-x-dist (arc-x-distance sum-radii radius-slope)
        arcs-y-dist (arc-y-distance sum-radii radius-slope)
        tangent-y-dist (+ arcs-y-dist centers-y-dist)
        tangent-x-dist (/ tangent-y-dist max-slope)]
    (+ arcs-x-dist tangent-x-dist)))


(defn arc-center-up
  "Given node info map (must have :node-y) and a min-arc-radius, returns
  returns the center for the upward arcs on that node"
  [{:keys [node-y]} min-arc-radius]
  {:pre [(vector? node-y) (number? min-arc-radius)]}
  (- (node-y 0) min-arc-radius))


(defn arc-center-down
  "Given node info map (must have :node-y) and a min-arc-radius, returns
  returns the center for the downward arcs on that node"
  [{:keys [node-y]} min-arc-radius]
  {:pre [(vector? node-y) (number? min-arc-radius)]}
  (+ (node-y 1) min-arc-radius))


(defn arc-radius-up
  "Given node info map (must have :order) a min-arc-radius and the number of
  pixels characters should be separated by, and returns the radius of the arc
  for that node for that character"
  [{:keys [order]} min-arc-radius char-sep]
  {:pre [(integer? order) (every? number? [min-arc-radius char-sep])]}
  (+ min-arc-radius
     (* order char-sep)))


(defn arc-radius-down
  "Given node info map (must have :order and :node, which in turn must have :weight)
  a min-arc-radius and the number of pixels characters should be separated by,
  and returns the radius of the arc for that node for that character"
  [{:keys [order] {weight :weight} :node} min-arc-radius char-sep]
  {:pre [(every? integer? [order weight])
         (every? number? [min-arc-radius char-sep])]}
  (+ min-arc-radius
     (* (- (dec weight) order)
        char-sep)))


(defn arc-y-info
  "Returns the y values of the arc centers, as well as the radii for each arc for
  a pair of character nodes on adjacent layers"
  [src dest dir min-arc-radius char-sep]
  {:pre [(contains? #{:up :down} dir)]}
  (if (= dir :up)
    {:src-arc-y (arc-center-up src min-arc-radius)
     :src-arc-radius (arc-radius-up src min-arc-radius char-sep)
     :dest-arc-y (arc-center-down dest min-arc-radius)
     :dest-arc-radius (arc-radius-down dest min-arc-radius char-sep)}
    {:src-arc-y (arc-center-down src min-arc-radius)
     :src-arc-radius (arc-radius-down src min-arc-radius char-sep)
     :dest-arc-y (arc-center-up dest min-arc-radius)
     :dest-arc-radius (arc-radius-up dest min-arc-radius char-sep)}))


(defn add-y-info
  "Adds :arc-y and :arc-radius information to character path pairs, plus
  :total-arc-radius, for convenience"
  [pair-maps min-arc-radius char-sep]
  (map (fn [{:keys [dir], [src dest] :pair, :as info}]
         (if (= dir :level)
           info
           (let [{:keys [src-arc-y dest-arc-y
                         src-arc-radius dest-arc-radius]}
                 (arc-y-info src dest dir min-arc-radius char-sep)]
             (-> info
                 (assoc :total-arc-radius (+ src-arc-radius dest-arc-radius))
                 (assoc-in [:pair 0 :arc-y] src-arc-y)
                 (assoc-in [:pair 0 :arc-radius] src-arc-radius)
                 (assoc-in [:pair 1 :arc-y] dest-arc-y)
                 (assoc-in [:pair 1 :arc-radius] dest-arc-radius)))))
       pair-maps))


(defn add-x-info
  "Adds :xs to character path pairs, and if the pairs are not :level, also
  adds :arc-x for each pair"
  [pair-maps layer-xs]
  (map (fn [{:keys [dir], [src dest] :pair, :as info}]
         (let [src-xs (-> src :node :layer-id layer-xs)
               dest-xs (-> dest :node :layer-id layer-xs)
               info' (-> info
                         (assoc-in [:pair 0 :xs] src-xs)
                         (assoc-in [:pair 1 :xs] dest-xs))]
           (if (= dir :level)
             info'
             (-> info'
                 (assoc-in [:pair 0 :arc-x] (src-xs 1))
                 (assoc-in [:pair 1 :arc-x] (dest-xs 0))))))
       pair-maps))


(defn pair-up
  "Given a list of character positions, pairs each with the one following for
  a representation of transistions between layers. Also adds information about
  the direction this transition goes."
  [infos]
  (->> (map vector infos (drop 1 infos))
       (reduce (fn [pairs [src dest]]
                 (let [src-y (:y src)
                       dest-y (:y dest)]
                   (conj pairs
                         {:dir (if (= src-y dest-y)
                                 :level
                                 (if (> src-y dest-y)
                                   :up
                                   :down))
                          :pair [src dest]})))
               [])))


(defn relative-layer-xs
  "Generates a map of layers to the pixel distance to the preceding layer"
  [path-info layers max-slope layer-sep]
  (->> (map second path-info)
       (mapcat #(remove (fn [{:keys [dir]}] (= dir :level)) %))
       (reduce (fn [min-seps {:keys [dir total-arc-radius],
                              [src dest] :pair}]
                 (let [layer (get layers (-> dest
                                             :node
                                             :layer-id))
                       src-arc-y (:arc-y src)
                       dest-arc-y (:arc-y dest)
                       centers-y-dist (if (= dir :up)
                                        (- src-arc-y dest-arc-y)
                                        (- dest-arc-y src-arc-y))
                       x-dist (layer-x-distance max-slope
                                                total-arc-radius
                                                centers-y-dist)]
                   (update-in min-seps [layer]
                              (fnil conj #{})
                              x-dist)))
               {})
       (map (fn [[layer x-ds]]
              [layer (apply max layer-sep x-ds)]))
       (into {})))


(defn absolute-layer-xs
  "Generates a map of layers to their absolute x-values, including duration"
  [path-info layers max-slope layer-sep]
  (let [x-seps (relative-layer-xs path-info layers max-slope layer-sep)]
    (into [] (reductions (fn [[_ last-x] layer]
                           (let [start-x (+ last-x (get x-seps layer))
                                 end-x (+ start-x (:duration layer))]
                             [start-x end-x]))
                         [0 (:duration (first layers))]
                         (rest layers)))))


(defn arcs-and-tangent
  "Given a pair of character positions that are not :level, generates plotting
  information for the arcs and the line tangent to them required to draw the
  transition"
  [{:keys [dir total-arc-radius],
    [{src-x :arc-x, src-y :arc-y, src-radius :arc-radius}
     {dest-x :arc-x, dest-y :arc-y, dest-radius :arc-radius}] :pair}]
  {:pre [(every? number? [total-arc-radius src-x src-y src-radius
                          dest-x dest-y dest-radius])
         (contains? #{:up :down} dir)]}
  (let [;; (intersect-x, intersect-y) is the point at which the line between
        ;; centers crosses the tangent
        intersect-x (/ (+ (* src-radius dest-x)
                          (* dest-radius src-x))
                       total-arc-radius)
        intersect-y (/ (+ (* src-radius dest-y)
                          (* dest-radius src-y))
                       total-arc-radius)
        src-radius2 (* src-radius src-radius)
        src-center-intersect-dx (- intersect-x src-x)
        src-center-intersect-dx2 (* src-center-intersect-dx
                                    src-center-intersect-dx)
        src-center-intersect-dy (- intersect-y src-y)
        src-center-intersect-dy2 (* src-center-intersect-dy
                                    src-center-intersect-dy)
        src-center-intersect-dx2-dy2 (+ src-center-intersect-dx2
                                        src-center-intersect-dy2)
        ;; In the triangle formed by arc center, tangent point, and intersect,
        ;; the src-altitude-foot is the foot that belongs to the tangent point
        src-altitude-foot-x (+ src-x
                               (/ (* src-radius2
                                     src-center-intersect-dx)
                                  src-center-intersect-dx2-dy2))
        src-altitude-foot-y (+ src-y
                               (/ (* src-radius2
                                     src-center-intersect-dy)
                                  src-center-intersect-dx2-dy2))
        src-radius-ratio (/ (.sqrt js/Math (- src-center-intersect-dx2-dy2
                                              src-radius2))
                            src-radius)
        ;; The case where (= src-y dest-y) can be treated as either
        ;; :up or :down
        center-line-dir (if (> src-y dest-y) :up :down)
        x-op (if (= dir center-line-dir) + -)
        [src-y-op dest-y-op] (if (= dir :up) [+ -] [- +])
        src-tangent-x (x-op src-altitude-foot-x
                            (* src-radius-ratio
                               (- src-altitude-foot-y src-y)))
        src-tangent-y (src-y-op src-altitude-foot-y
                                (* src-radius-ratio
                                   (- src-x src-altitude-foot-x)))
        radius-slope (/ (- src-tangent-y src-y)
                        (- src-tangent-x src-x))
        dest-tangent-x (- dest-x
                          (arc-x-distance dest-radius radius-slope))
        dest-tangent-y (dest-y-op dest-y
                                  (arc-y-distance dest-radius radius-slope))
        [src-sweep dest-sweep] (if (= dir :up) [0 1] [1 0])]
    [{:type :a, :radius src-radius, :sweep src-sweep
      :x src-tangent-x, :y src-tangent-y}
     {:type :l, :x dest-tangent-x, :y dest-tangent-y}
     {:type :a, :radius dest-radius, :sweep dest-sweep
      :x dest-x, :y dest-y}]))


(defn extend-h
  "Extends a terminal :h plot to a new x value"
  [plots x]
  {:pre [(= (:type (peek plots)) :h) (number? x)]}
  (assoc-in plots [(dec (count plots)) :x] x))


(defn h-to
  "Draw a horizontal line to x"
  [x]
  {:pre [(number? x)]}
  {:type :h, :x x})


(defn plot-duration
  "Plots a layer duration after a non-:level transition"
  [plots start-x end-x]
  {:pre [(number? start-x) (number? end-x)]}
  (if (= start-x end-x)
    plots
    (conj plots (h-to end-x))))


(defn move-to
  "Starts a path by moving to its beginning and drawing the initial layer's
  duration, if applicable"
  [{:keys [y], [start-x end-x] :xs}]
  {:pre [(every? number? [y start-x end-x])]}
  (let [plots [{:type :m
                :x start-x
                :y y}]]
    (plot-duration plots start-x end-x)))


(defn char-plots
  "Generates coordinates a single character's path"
  [plots {:keys [dir pair], [src dest] :pair, :as pair-map}]
  ;; TODO checking empty? on every step seems silly; once we have
  ;; multipart paths, potentially with mid-path M's, we should just
  ;; use (move-to src) as the seed value in the reduce
  (let [plots' (if (empty? plots)
                 (into plots (move-to src))
                 plots)
        last-type (:type (peek plots'))
        [dest-start-x dest-end-x] (:xs dest)]
    (match [last-type dir]
           [:h :level] (extend-h plots' dest-end-x)
           [_ :level] (conj plots' (h-to dest-end-x))
           :else (plot-duration (into plots' (arcs-and-tangent pair-map))
                                dest-start-x dest-end-x))))


(defn plot
  "Generates coordinates all paths in a FlatGraph"
  [flat-graph max-slope min-arc-radius layer-sep node-sep char-sep]
  (let [{:keys [layers characters]} flat-graph
        node-ys (FlatGraph->node-ys flat-graph node-sep char-sep)
        paths-y (->> (nodes flat-graph)
                     (mapcat (fn [node]
                               (map-indexed (fn [i c]
                                              {:node node
                                               :character c
                                               :order i})
                                            (get characters node))))
                     (map (fn [info]
                            (let [node (:node info)
                                  node-top-y (get node-ys node)]
                              (assoc info
                                     :node-y [node-top-y
                                              (+ node-top-y
                                                 (* char-sep
                                                    (dec (:weight node))))]
                                     :y (+ node-top-y (* (:order info)
                                                         char-sep))))))
                     (group-by :character)
                     (map (fn [[character infos]]
                            [character (pair-up infos)]))
                     (map (fn [[character pair-maps]]
                            [character (add-y-info pair-maps min-arc-radius
                                                   char-sep)])))
        layer-xs (absolute-layer-xs paths-y layers max-slope layer-sep)]
    (->> paths-y
         (map (fn [[character pair-maps]]
                [character (add-x-info pair-maps layer-xs)]))
         (map (fn [[character pair-maps]]
                [character (reduce char-plots [] pair-maps)]))
         (into {}))))
