(ns fatlip.order
  (:require [clojure.core.rrb-vector :as rrb]
            [clojure.set :as set]
            [clojure.string :as s]
            [fatlip.protocols :refer [Layered Sparse Directed Reversible
                                      Nodey Node Edge Edge->Segment rev]]))


(defrecord OrderedGraph [layers succs preds ps qs rs
                         minus-ps minus-qs characters]
  Layered
  (layers [_] layers)

  Sparse
  (ps [_] ps)
  (qs [_] qs)
  (rs [_] rs)

  Directed
  (succs [_] succs)
  (preds [_] preds)

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

(defrecord FlatGraph [layers succs preds aboves belows top-idxs bot-idxs
                      crossings marked characters]
  Nodey
  (nodes [_]
    (mapcat (fn [layer]
              (filter #(instance? Node %) (:items layer)))
            layers))

  Directed
  (succs [_] succs)
  (preds [_] preds)

  Reversible
  (rev [this]
    (assoc this
           :succs preds
           :preds succs
           :layers (vec (rseq layers)))))

(defrecord FlatLayer [id duration items])

(defrecord AccumulatorNode [weight node-edges is-seg-c])


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
  "Step 2a of ESK. Positions in an ordered layer are used to calculate the
  order of the next layer. ESK's description of the position algorithm is
  almost willfully circuitous and obtuse, so here's a simplified description:
  An item's position in an ordered layer is the sum of the size of all previous
  items, plus 1, where the size of a segment container is the number of edges
  it contains, and the size of a node is 1.

  Also, it doesn't really matter what seed you choose for the initial value of
  the sum. I chose 0, which is implied by the description in ESK."
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
  "Step 2b of ESK - Use nodes' predecessors to calculate a 'measure' for the
  nodes and containers in a layer, which is used for ordering"
  [non-qs preds positions]
  (into {} (map #(-> [% (get-measure % (get preds %) positions)]) non-qs)))


(defn merge-layer
  "Step 3 of ESK - Considers a layer as two lists, one of nodes and the other
  of segment containers. The items in these lists have 'measures' (for segment
  containers, this is equivalent to the position in the previous layer, so we
  just use that), and we merge the two lists into a single ordering based on
  these measures."
  [minus-ps positions non-qs measures]
  (let [ns (sort-by #(get measures %) non-qs)
        ss (->> (filter vector? minus-ps)
                (sort-by #(get positions %)))]
    (loop [nodes ns
           segments ss
           pos positions
           ord []]
      (if (or (empty? nodes) (empty? segments))
        ;; ESK's algorithm doesn't specify what to do with leftover things I
        ;; think this is because it doesn't take into account nodes that don't
        ;; have parents in layers > 0. In any case, at most one of nodes or
        ;; segments will be non-empty
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
                         (cons s-2 (rest segments))
                         (assoc pos s-2 (+ (get pos seg-1) 1))
                         (into ord [s-1 node-1])))))))))


(defn add-qs
  "Step 4 of ESK - takes the results of step 3, which doesn't include the
  q-nodes, and adds them, splitting their segment containers in the process"
  [minus-qs qs]
  (->> (flatten minus-qs)
       (map #(if (contains? qs (:dest %))
               (:dest %)
               %))
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
        ;; Edges between segment containers need to be counted as well but they
        ;; change with each new ordering, so we just temporarily merge the
        ;; current segment edges with the never-changing node -> node edges
        next-segs-to-seg-cs (->> (filter vector? next-ordered)
                                 (mapcat (fn [seg-c]
                                           (map (fn [seg]
                                                  [seg seg-c])
                                                seg-c)))
                                 (into {}))
        seg->Edge (fn [seg-c]
                    (fn [seg]
                      (let [dest (get next-segs-to-seg-cs seg)
                            seg-chars (set (mapcat :characters seg))
                            dest-chars (set (mapcat :characters dest))
                            edge-characters (set/intersection
                                             seg-chars
                                             dest-chars)]
                        (Edge. seg-c dest
                               edge-characters
                               (count edge-characters)))))
        edges (->> (filter vector? ordered)
                   (map
                    (fn [seg-c]
                      [seg-c (set (map (seg->Edge seg-c) seg-c))]))
                   (into {})
                   (merge graph-edges))]
    (->> ordered
         (mapcat (fn [item]
                   (sort-by #(get next-order-map (:dest %))
                            (get edges item))))
         (map #(-> [(get next-order-map (:dest %)) %])))))


(defn next-power-of-2
  "A helper for cross counting; the number of leaf nodes in the accumulator
  tree needs to be the first power of 2 greater than the number of nodes in one
  layer.  This function sets all the bits to the right of the first bit set in
  a 32 bit number, and then increments, which is the same thing"
  [x]
  (inc (reduce (fn [num exp]
                 (let [shifted (bit-shift-right num (.pow js/Math 2 exp))]
                   (bit-or num shifted)))
               x
               (range 5))))


(defn single-edge-super-crossings
  "Counts the number of crossings that result from adding an edge, in order,
  to the accumulator tree. If the index is even, meaning it's a right child of
  its parent, we increment its value. If the index is odd (left child), we add
  the value of its right sibling times the current weight to the cross count,
  as the right sibling represents weight of the edges that were added ahead of
  this one, and therefore, crossings. Then we walk up the tree to the root,
  incrementing and adding right siblings, for a total count of edges that cross
  this one"
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
              ;; mental model of the accumulator tree is the same as that
              ;; node's index (if it's a right child), or its sibling's index
              ;; (if it's a left child) in the compact representation of the
              ;; tree
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
                            is-seg-c (assoc-in
                                      [real-right-index :is-seg-c]
                                      true)
                            (not is-seg-c) (update-in
                                            [real-right-index :node-edges]
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
  "Counts the sub-crossings that result from adding a single sub-edge to the
  accumulator tree"
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
  "Counts sub-crossings for a node; short circuits if there are fewer than two
  successor nodes"
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
  "A modification of ESK. In our scheme, each node in the graph represents a
  set of characters, and those characters have an order within their node. If
  those characters then diverge to different successor nodes, then depending on
  their relative positions and the positions of the successors, their lines may
  cross in a way that is not detected by the coarser-grained node-by-node cross
  counting. We apply the same methodology here, but without needing to deal
  with crossing segments, or with edge weight"
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


(defn count-and-mark-crossings-layer
  "Step 5 of ESK, counts the number of crossings that result from a bi-layer
  ordering. Implements the algorithm found in Bilayer Cross Counting, by
  Wilhelm Barth, Petra Mutzel and Michael Jünger

  I've made the following modifications:

  - Counting inter-node crossings and intra-node crossings. For inter-node
  crossings, edges that originate from separate nodes in one layer and arrive
  at separate nodes in the next layer may cross each other. These is what is
  normally counted in layered graph drawing. However, for this application,
  those coarse edges may be composed of several finer sub-edges, and the
  sub-edges may cross each other as well, depending on their initial ordering
  and the ordering of their destination nodes. We can use basically the same
  algorithm at a smaller scale to count these crossings as well

  - Added weights to the inter-node edges

  - Marking inter-node edges that cross segment containers, which helps in the
  layout process later, by:

  - Storing accumulated node -> node edges in the accumulator tree

  - Storing a flag for whether a node has seen a segment container in the
  accumulator tree

  - Only storing information on right siblings, as the information on the root
  or left siblings is never accessed

  - Due to not storing information on the root or left siblings, we can store
  everything in a compact representation of the tree that is one less than half
  the size of the mental model of the tree, by only accounting for right
  siblings. This leads to a few coincidences, namely, the size of the compact
  representation is the same as the index of the first leaf in the expanded
  tree, and the index of a right sibling in the compact representation is the
  same as that node's parent in the expanded tree"
  [minus-ps minus-qs preds succs characters]
  (let [[sup-crossings marked] (count-and-mark-super-crossings
                                minus-ps minus-qs preds succs)
        sub-crossings (count-sub-crossings minus-ps minus-qs succs characters)]
    [(+ sup-crossings sub-crossings) marked]))


(defn OrderedGraph->OrderedGraph
  "Performs one layer-by-layer sweep of the graph using ESK's algorithm"
  [ordered-graph]
  (let [{:keys [ps qs rs preds succs characters layers]} ordered-graph
        first-layer (first layers)
        [ordered-layers minus-ps minus-qs]
        (reduce (fn [[layers minus-ps minus-qs prev-layer] layer]
                  (let [minus-p (replace-ps (:items prev-layer) ps succs)
                        positions (set-positions minus-p)
                        [qs non-qs] (map set
                                         ((juxt filter remove)
                                          #(contains? qs %)
                                          (remove vector? (:items layer))))
                        measures (set-measures non-qs preds positions)
                        minus-q (merge-layer minus-p positions
                                             non-qs measures)
                        items (add-qs minus-q qs)
                        ordered-layer (OrderedLayer. (:id layer)
                                                     (:duration layer)
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
                        :rs rs
                        :characters characters})))


(defn get-ordered
  "Orders characters for a single node based on predecessors"
  [node preds characters prev-l-characters]
  (let [pred-nodes (get preds node)
        node-characters (:characters node)
        ordered (condp = (count pred-nodes)
                  0 (get characters node)
                  1 (filterv #(contains? node-characters %)
                             (get characters (:dest (first pred-nodes))))
                  (filterv #(contains? node-characters %)
                           prev-l-characters))]
    (if (= (count ordered) (count node-characters))
      ordered
      (into ordered (set/difference node-characters (set ordered))))))


(defn order-subnodes
  "Orders all characters in all nodes based on their predecessors"
  [graph]
  (let [{:keys [layers characters preds]} graph
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
    (assoc graph :characters sorted)))


(defn SparseGraph->OrderedGraph
  [sparse-graph]
  (let [{:keys [ps qs rs preds succs characters layers]} sparse-graph
        ordered-layers (map (fn [layer]
                              (map->OrderedLayer
                               (-> layer
                                   (dissoc :nodes)
                                   (assoc :items (:nodes layer)))))
                            layers)]
    (map->OrderedGraph {:layers ordered-layers
                        :minus-ps []
                        :minus-qs []
                        :succs succs
                        :preds preds
                        :ps ps
                        :qs qs
                        :rs rs
                        :characters characters})))


(defn SparseGraph->ordered-graphs
  "Implements the 2-layer crossing minimization algorithm on a sparse graph
  found in 'An Efficient Implementation of Sugiyama’s Algorithm for Layered
  Graph Drawing', a paper by Markus Eiglsperger, Martin Sieberhaller, and
  Michael Kaufmann (ESK)

  I've made the following modifications:

  - Removed the concept of alternating layers, which don't help much, and hurt
  a bit

  - Added weights to nodes and edges

  - Added short-circuiting if we've seen a seed layer before

  - Now that short-circuiting doesn't rely on crossing counts, I pulled
  counting crossings and marking edges out of the ordering algorithm. This
  allows the counting and marking to be parallelized, whereas the ordering is
  inherently serial."
  [sparse-graph & {:keys [max-sweeps] :or {max-sweeps 20}}]
  (let [last-layer-idx (dec (count (:layers sparse-graph)))]
    (->>
     (loop [orderings []
            ordering-set #{}
            input-graph (SparseGraph->OrderedGraph sparse-graph)]
       (let [graph (OrderedGraph->OrderedGraph input-graph)
             layers (:layers graph)]
         ;; If we see a graph that we've seen before, this is the
         ;; beginning of a cycle, and we can stop processing
         (if (or (contains? ordering-set graph)
                 (= max-sweeps (inc (count orderings))))
           orderings
           ;; The last layer of the current ordering is the first
           ;; layer of the next
           (recur (conj orderings graph)
                  (conj ordering-set graph)
                  (rev graph)))))
     ;; This is sort of ugly, but once we order the nodes, we still have to
     ;; come up with a good subnode ordering. Conceptually, this belongs in
     ;; OrderedGraph->OrderedGraph, but it's also conceptually cromulent to have
     ;; OrderedGraph->OrderedGraph know nothing about the forward/reverse dance
     ;; that we do here. However, subnode ordering is dependent on
     ;; direction. My choice then, is to make OrderedGraph->OrderedGraph
     ;; direction-aware, or pull the subnode ordering out and put it here,
     ;; where we're aware of the direction.  For now, I've chosen the latter.
     (map-indexed (fn [idx graph]
                    (let [backward-graph (if (odd? idx)
                                           graph
                                           (rev graph))]
                      (-> backward-graph
                          order-subnodes
                          rev
                          order-subnodes)))))))


(def count-and-mark-crossings
  ^{:doc "Takes an OrderedGraph, counts edge crossings and marks edges that should not
         be drawn straight. Memoized."}
  (memoize
   (fn [ordered-graph]
     (let [{:keys [minus-ps minus-qs preds succs characters]} ordered-graph
           [crossings marked] (->> (map #(count-and-mark-crossings-layer
                                          %1 %2 preds succs characters)
                                        minus-ps minus-qs)
                                   (apply map vector))]
       {:crossings (reduce + crossings)
        :marked (reduce set/union marked)}))))


(defn best-ordering
  "Chooses the graph with the fewest crossings from a collection of OrderedGraphs"
  [ordered-graphs]
  (->> ordered-graphs
       (sort-by (fn [og]
                  (let [{:keys [crossings marked]} (count-and-mark-crossings og)]
                    [crossings (reduce #(+ %1 (:weight %2)) 0 marked)])))
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


(defn OrderedGraph->FlatGraph
  "Transforms each OrderedLayer into a FlatLayer, and calculates attributes
  that are useful for organizing Nodes and Segments into horizontally-aligned
  blocks"
  [ordered-graph]
  (let [{:keys [layers succs preds characters]} ordered-graph
        {:keys [crossings marked]} (count-and-mark-crossings ordered-graph)
        flat-layers (mapv OrderedLayer->FlatLayer layers)
        [aboves belows] (apply map merge (map neighborify flat-layers))
        [top-idxs bot-idxs] (apply map merge (map indexify flat-layers))]
    (map->FlatGraph {:layers flat-layers
                     :succs succs
                     :preds preds
                     :aboves aboves
                     :belows belows
                     :top-idxs top-idxs
                     :bot-idxs bot-idxs
                     :marked marked
                     :crossings crossings
                     :characters characters})))


(def SparseGraph->FlatGraph (comp OrderedGraph->FlatGraph
                                  best-ordering
                                  SparseGraph->ordered-graphs))
