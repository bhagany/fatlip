(ns fatlip.core
  (:require [clojure.set :as set]
            [clojure.string :as s]))


(defprotocol Reversible
  (rev [this] "It... reverses"))

(defprotocol Orderable
  (order [this]))

(defprotocol Plottable
  (plot [this]))

(defprotocol Drawable
  (draw [this]))

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


(defn Edge->Segment
  "Does what it says on the tin; Segments represent the portion of an Edge that
  crosses a particular layer"
  [edge layer-id]
  (map->Segment {:endpoints #{(:src edge) (:dest edge)}
                 :layer-id layer-id
                 :characters (:characters edge)
                 :weight (:weight edge)}))


(defn add-edge
  "Creates an edge and adds it to the graph as a whole and to each
  participating node"
  [graph last-node node characters]
  (let [weight (count characters)
        edge (Edge. last-node node (set characters) weight)]
    (-> graph
        (update-in [:succs last-node] (fnil conj #{}) edge)
        (update-in [:preds node] (fnil conj #{}) (rev edge)))))


(defn make-node
  "Utility function that was abstracted to avoid mutually recursing back to
  add-node when creating helper (p, q, and r) nodes. In particular, helper
  nodes don't require character group processing or bookkeeping metadata"
  [graph layer-id input]
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


(defn add-p-q-nodes
  "When an edge would span more than 2 layers, two helper nodes are created.
  P nodes are placed on the layer following last-node's layer, and q nodes are
  placed on the layer preceding node's layer. Edges are then drawn from
  last-node -> p node and from p node to q node, regardless of the span between
  the p node and the q node"
  [graph last-node node characters]
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


(defn add-r-node
  "R nodes are created to assist in drawing the graph, when an edge skips over
  a layer. In that case, the r node is placed in the intervening layer, and an
  edge is created from last-node to r node"
  [graph last-node node characters]
  (let [r-layer-id (inc (:layer-id last-node))
        [r-g r-node] (make-node graph r-layer-id {:characters characters})
        g (-> (update-in r-g [:rs] conj r-node)
              (add-edge last-node r-node characters))]
    [g r-node]))


(defn process-edge-characters
  "Create edges and possibly new helper nodes for a set of characters that
  remain together from last-node to node"
  [graph last-node node characters]
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
                          edge-characters (vec (set/intersection
                                                last-node-characters
                                                characters))
                          g (process-edge-characters graph last-node
                                                     node edge-characters)]
                      [g (set/difference characters last-node-characters)])
                    [graph (disj characters character)])]
        (recur g c)))))


(defn add-node
  "Create a new node, then deduce and create edges to that node from previous
  layers. Then, update the bookkeeping metadata for each character in a node
  for calculating future edges from this node"
  [graph layer-id input]
  (let [[grph node] (make-node graph layer-id input)]
    (reduce (fn [g character]
              (let [last-node (-> g :last-nodes-by-character character)
                    disappeared (contains? (-> node :path-mods character)
                                           :disappeared)]
                (cond-> g
                  disappeared (update-in [:last-nodes-by-character]
                                         dissoc character)
                  (not disappeared) (->
                                     (assoc-in
                                      [:last-nodes-by-character character]
                                      node)
                                     (update-in
                                      [:last-nodes-by-node (:id node)]
                                      (fnil conj #{}) character))
                  last-node (update-in [:last-nodes-by-node (:id last-node)]
                                       disj character))))
            (process-characters grph node)
            (-> grph :characters (get node)))))


(defn add-layer
  "Create the layer of our graph representation, and recursively create and add
  nodes from the groups in the input layer"
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
