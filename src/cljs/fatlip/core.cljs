(ns fatlip.core
  (:require [clojure.core.rrb-vector :as rrb]
            [clojure.set :as set]
            [clojure.string :as s]))


(defrecord Node [id layer-id characters weight])
(defrecord Edge [src dest characters weight])
(defrecord Layer [id duration nodes])
(defrecord AccumulatorNode [weight node-edges is-seg-c])


(defn- add-edge [graph last-node node characters]
  "Creates an edge and adds it to the graph as a whole and to each participating node"
  (let [weight (count characters)
        forward-edge (Edge. last-node node characters weight)
        ;; We record the "true" forward edge here, for marking edges that
        ;; cross segment containers. The ordering algorithm works through
        ;; the graph backwards on every second pass, and the cross counting
        ;; algorithm switches the order of the layers depending on which
        ;; is smaller, but we always layout and draw the graph forward, so the
        ;; marked edges need to be the forward ones.
        edge-1 (with-meta forward-edge {:forward-edge forward-edge})
        edge-2 (with-meta (Edge. node last-node characters weight)
                 {:forward-edge forward-edge})]
    (-> graph
        (update-in [:succs last-node] (fnil conj #{}) edge-1)
        (update-in [:preds node] (fnil conj #{}) edge-2))))


(defn- make-node [graph layer-id input]
  "Utility function that was abstracted to avoid mutually recursing back to add-node
  when creating helper (p, q, and r) nodes. In particular, helper nodes don't require
  character group processing or bookkeeping metadata"
  (let [node-num (count (-> graph :layers (get layer-id) :nodes))
        node-id (keyword (s/join "-" [layer-id node-num]))
        weight (-> input :characters count)
        node (map->Node (assoc input :id node-id :layer-id layer-id :weight weight))
        g (update-in graph [:layers layer-id :nodes] conj node)]
    [g node]))


(defn- add-p-q-nodes [graph last-node node characters]
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


(defn- add-r-node [graph last-node node characters]
  "R nodes are created to assist in drawing the graph, when an edge skips over a layer.
  In that case, the r node is placed in the intervening layer, and an edge is created
  from last-node to r node"
  (let [r-layer-id (inc (:layer-id last-node))
        [r-g r-node] (make-node graph r-layer-id {:characters characters})
        g (-> (update-in r-g [:rs] conj r-node)
              (add-edge last-node r-node characters))]
    [g r-node]))


(defn- process-edge-characters [graph last-node node characters]
  "Create edges and possibly new helper nodes for a set of characters that
  remain together from last-node to node"
  (let [span (- (:layer-id node) (:layer-id last-node))
        [g last-n] (cond
                    (= span 2) (add-r-node graph last-node node characters)
                    (> span 2) (add-p-q-nodes graph last-node node characters)
                    :else [graph last-node])]
    (add-edge g last-n node characters)))


(defn- process-characters
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
        layer (Layer. layer-id
                      (input-layer :duration)
                      [])]
    (loop [g (update-in graph [:layers] conj layer)
           input-groups (input-layer :groups)]
      (if (empty? input-groups)
        g
        (recur (add-node g layer-id (first input-groups))
               (rest input-groups))))))


(defn make-sparse-graph
  "Creates a sparse graph, as defined in ESK"
  ([input]
     (make-sparse-graph input
                        {:layers []
                         :succs {}
                         :preds {}
                         :aboves {}
                         :belows {}
                         :ps #{}
                         :qs #{}
                         :rs #{}
                         :marked #{}
                         :last-nodes-by-character {}
                         :last-nodes-by-node {}}))
  ([input graph]
     (if (empty? input)
       graph
       (let [i (first input)
             dup-character (->> (mapcat #(:characters %) (:groups i))
                                frequencies
                                (filter (fn [[_ c]] (> c 1)))
                                first)]
         (if dup-character
           (throw (js/Error. (str (get dup-character 0) " is specified "
                                  (get dup-character 1) " times in one layer")))
           (recur (rest input) (add-layer graph i)))))))


(defn replace-ps
  "Step 1 of ESK - replace all p nodes with edges and merge segment containers"
  [graph layer]
  (->> (:ordered layer)
       (map #(if (contains? (:ps graph) %)
               ;; p nodes always have only one successor
               [(-> (:succs graph) (get %) first)]
               %))
       (reduce #(if (and (vector? (peek %1))
                         (vector? %2))
                  (update-in %1 [(dec (count %1))] rrb/catvec %2)
                  (conj %1 %2))
               [])
       (assoc layer :minus-ps)))


(defn- set-positions
  "Step 2a of ESK. Positions in an ordered layer are used to calculate the order of the
  next layer. ESK's description of the position algorithm is almost willfully circuitous
  and obtuse, so here's a simplified description: An item's position in an ordered layer
  is the sum of the size of all previous items, plus 1, where the size of a segment
  container is the number of edges it contains, and the size of a node is 1.

  Also, it doesn't really matter what seed you choose for the initial value of the sum. I
  chose -1, which is implied by the description in ESK."
  [layer]
  (loop [minus-ps (:minus-ps layer)
         current-position -1
         positions {}]
    (if (empty? minus-ps)
      (assoc layer :positions positions)
      (let [item (first minus-ps)
            c-p (if (vector? item)
                  (+ current-position (count item))
                  (inc current-position))]
        (recur (rest minus-ps) c-p (assoc positions item (inc current-position)))))))


(defn- set-qs-non-qs
  "ESK requires a layer's nodes to be split into lists of q-nodes and non-q-nodes"
  [graph layer]
  (->> ((juxt filter remove) #(contains? (:qs graph) %) (:nodes layer))
       (map set)
       (interleave [:qs :non-qs])
       (apply (partial assoc layer))))


(defn- get-measure
  "Calculate the average weighted position of a node's predecessors"
  [graph node pred-positions]
  (if-let [preds (get-in graph [:preds node])]
    (apply / (->> preds
                  (map #(let [weight (:weight %)
                              pos (get pred-positions (:dest %))]
                          [(* weight pos) weight]))
                  (apply map +)))
    0))


(defn set-measures
  "Step 2b of ESK - Use nodes' predecessors to calculate a 'measure' for the nodes
  and containers in a layer, which is used for ordering"
  [graph layer next-layer]
  (let [positions (:positions layer)
        non-qs (:non-qs next-layer)
        measures (reduce #(assoc %1 %2 (get-measure graph %2 positions)) {} non-qs)]
    (assoc next-layer :measures measures)))


(defn order-next-layer
  "Step 3 of ESK - Considers a layer as two lists, one of nodes and the other of segment
  containers. The items in these lists have 'measures' (for segment containers, this is
  equivalent to the position in the previous layer, so we just use that), and we merge
  the two lists into a single ordering based on these measures."
  [layer next-layer]
  (let [positions (:positions layer)
        measures (:measures next-layer)
        ns (sort-by #(get measures %) (:non-qs next-layer))
        ss (->> (:minus-ps layer)
                (filter vector?)
                (sort-by #(get positions %)))
        minus-qs (loop [nodes ns
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
                                      (into ord [s-1 node-1])))))))]
    (assoc next-layer :minus-qs minus-qs)))


(defn add-qs
  "Step 4 of ESK - takes the results of step 3, which doesn't include the q-nodes, and adds them,
  splitting their segment containers in the process"
  [next-layer]
  (let [qs (:qs next-layer)
        flat (->> (:minus-qs next-layer)
                  (mapcat #(if (vector? %)
                             %
                             [%]))
                  (map #(if (and (instance? Edge %)
                                 (contains? qs (:dest %)))
                          (:dest %)
                          %)))
        ordered (loop [f flat
                       seg-c []
                       layer []]
                  (if (empty? f)
                    (if (empty? seg-c)
                      layer
                      (conj layer seg-c))
                    (let [item (first f)]
                      (if (instance? Edge item)
                        (recur (rest f) (conj seg-c item) layer)
                        (if (empty? seg-c)
                          (recur (rest f) seg-c (conj layer item))
                          (recur (rest f) [] (conj layer seg-c item)))))))]
    (assoc next-layer :flat flat :ordered ordered)))


(defn- sorted-edge-order
  "Sorts edges between two ordered layers first by their index in the source
  layer, and then by their index in the destination layer. Then, returns seq
  of [order edge] pairs of the edge targets in the destination layer, using
  this edge ordering"
  [ordered next-ordered graph-edges]
  (let [next-order-map (->> (map-indexed #(-> [%2 %1]) next-ordered)
                            (into {}))
        ;; Edges between segment containers need to be counted as well
        ;; but they change with each new ordering, so we just temporarily
        ;; merge the current segment edges with the never-changing
        ;; node -> node edges
        edges (->> (filter vector? next-ordered)
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


(defn- next-power-of-2
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


(defn- single-edge-super-crossings
  "Counts the number of crossings that result from adding an edge, in order,
  to the accumulator tree. If the index is even, meaning it's a right
  child of its parent, we increment its value. If the index is
  odd (left child), we add the value of its right sibling times the
  current weight to the cross count, as the right sibling represents
  weight of the edges that were added ahead of this one, and
  therefore, crossings. Then we walk up the tree to the root,
  incrementing and adding right siblings, for a total count of edges
  that cross this one"
  [graph tree orig-index edge]
  (let [weight (:weight edge)
        is-seg-c (vector? (:dest edge))]
    (loop [graph graph
           tree tree
           index orig-index
           crossings 0]
      (if (zero? index)
        [graph tree crossings]
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
                  marked (if is-seg-c
                           (:node-edges right-sib)
                           (if (:is-seg-c right-sib)
                             #{(-> edge meta :forward-edge)}
                             #{}))
                  g (update-in graph [:marked] set/union marked)]
              (recur g tree parent-index c))
            (let [t (-> (update-in tree [real-right-index :weight] + weight)
                        (cond->
                         is-seg-c (assoc-in [real-right-index :is-seg-c] true)
                         (not is-seg-c) (update-in [real-right-index :node-edges]
                                                   conj (-> edge meta :forward-edge))))]
              (recur graph t parent-index crossings))))))))


(defn count-super-crossings
  "Counts crossings between separate nodes from one layer to the next"
  [graph layer next-layer]
  (let [minus-ps (:minus-ps layer)
        minus-qs (:minus-qs next-layer)
        [layer-1 layer-2 edges] (if (< (count minus-ps) (count minus-qs))
                                  [minus-qs minus-ps (:preds graph)]
                                  [minus-ps minus-qs (:succs graph)])
        num-acc-leaves (next-power-of-2 (count layer-2))
        first-leaf (dec num-acc-leaves)
        ;; Our compact representation has the same size as the index
        ;; of the first leaf in the mental model of the tree
        tree-size first-leaf]
    (loop [graph graph
           tree (vec (repeat tree-size (AccumulatorNode. 0 #{} false)))
           crossings 0
           order (sorted-edge-order layer-1 layer-2 edges)]
      (if (empty? order)
        [graph crossings]
        (let [[ord edge] (first order)
              index (+ first-leaf ord)
              [g t c] (single-edge-super-crossings graph tree index edge)]
          (recur g t (+ crossings c) (rest order)))))))


(defn single-edge-sub-crossings
  "Counts the sub-crossings that result from adding a single sub-edge
  to the accumulator tree"
  ([tree index]
     (single-edge-sub-crossings tree index 0))
  ([tree index crossings]
     (if (zero? index)
       [tree crossings]
       (let [parent-index (.floor js/Math (/ (dec index) 2))
             real-right-index parent-index]
         (if (odd? index)
           (recur tree parent-index (+ crossings (get tree real-right-index)))
           (recur (update-in tree [real-right-index] inc) parent-index crossings))))))


(defn count-sub-crossings-single-node
  "Counts sub-crossings for a node; short circuits if there are fewer
  than two successor nodes"
  [graph node next-layer]
  (let [succs (-> graph
                  :succs
                  (get node)
                  (->> (map #(:dest %))))
        num-succs (count succs)]
    (if (< num-succs 2)
      0
      (let [measures (:measures next-layer)
            order-map (->> (sort-by #(get measures %) succs)
                           (map-indexed (fn [idx n]
                                          (map #(-> [% idx])
                                               (:characters n))))
                           (apply concat)
                           (into {}))
            num-acc-leaves (next-power-of-2 num-succs)
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
  [graph layer next-layer]
  (loop [nodes (filter #(instance? Node %) (:minus-ps layer))
         crossings 0]
    (if (empty? nodes)
      crossings
      (let [node (first nodes)
            c (count-sub-crossings-single-node graph node next-layer)]
        (recur (rest nodes) (+ crossings c))))))


(defn count-crossings
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
  [graph layer next-layer]
  (let [[g sup-crossings] (count-super-crossings graph layer next-layer)
        sub-crossings (count-sub-crossings graph layer next-layer)]
    [g (+ sup-crossings sub-crossings)]))


(defn reverse-graph
  "Reverse a graph by reversing its layers and the direction of its edges"
  [graph]
  (assoc graph
    :succs (:preds graph)
    :preds (:succs graph)
    :ps (:qs graph)
    :qs (:ps graph)
    :layers (vec (rseq (:layers graph)))))


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


(defn neighborify
  "Maps things in a layer (nodes and edges) to the things that are directly
  above or below"
  [graph layer]
  (let [layer-id (:id layer)]
    (loop [grph graph
           o (-> layer :flat first)
           os (-> layer :flat rest)]
      (if (empty? os)
        grph
        (let [next-o (first os)
              g (-> grph
                    (assoc-in [:belows layer-id o] next-o)
                    (assoc-in [:aboves layer-id next-o] o))]
          (recur g next-o (rest os)))))))


(defn indexify
  [graph layer]
  (let [layer-id (:id layer)
        flat (:flat layer)
        len (count flat)
        top-idxs (->> flat
                      (map-indexed #(-> [%2 %1]))
                      (into {}))
        bot-idxs (->> top-idxs
                      (map (fn [[o idx]]
                             [o (- len idx 1)]))
                      (into {}))]
    (-> graph
        (assoc-in [:top-idxs layer-id] top-idxs)
        (assoc-in [:bot-idxs layer-id] bot-idxs))))


(defn order-graph-once
  "Performs one layer-by-layer sweep of the graph using ESK's algorithm"
  [sparse-graph]
  (loop [graph sparse-graph
         layer (-> graph :layers first)
         layers (-> graph :layers rest)]
    (if (empty? layers)
      graph
      (let [l (-> (replace-ps graph layer)
                  set-positions)
            next-l (->> (first layers)
                        (set-qs-non-qs graph)
                        (set-measures graph l)
                        (order-next-layer l)
                        add-qs)
            [marked-graph crossings] (count-crossings graph l next-l)
            g (-> marked-graph
                  (update-in [:crossings] + crossings)
                  (update-in [:layers] assoc (:id l) l)
                  (update-in [:layers] assoc (:id next-l) next-l)
                  (neighborify next-l)
                  (indexify next-l))]
        (recur g next-l (rest layers))))))


(defn seed-graph
  "Set up the attributes the first layer in a sweep needs in order to serve
  as a basis for ordering the following layer"
  [graph seed-order]
  (let [layer (-> graph :layers
                  (get 0)
                  (assoc :flat seed-order :ordered seed-order))]
    (-> (assoc-in graph [:layers 0] layer)
        (neighborify layer)
        (indexify layer))))


(defn order-graph
  "Implements the 2-layer crossing minimization algorithm on a sparse graph found in
  'An Efficient Implementation of Sugiyama’s Algorithm for Layered Graph Drawing',
  a paper by Markus Eiglsperger, Martin Sieberhaller, and Michael Kaufmann (ESK)

  I've made the following modifications:
  - Removed the concept of alternating layers, which don't help much, and hurt a bit
  - Added weights to nodes and edges"
  [sparse-graph]
  ;; seed the first layer with initial ordered layer
  (loop [seed-order (-> sparse-graph :layers first :nodes)
         orderings []]
    (let [c (count orderings)]
      (if (= c 20)
        orderings
        (let [reverse? (odd? c)
              ordered-graph (-> (if reverse?
                                  (reverse-graph sparse-graph)
                                  sparse-graph)
                                (seed-graph seed-order)
                                order-graph-once)]
          (recur (-> ordered-graph :layers peek :ordered)
                 (conj orderings (if reverse? (reverse-graph ordered-graph) ordered-graph))))))))
