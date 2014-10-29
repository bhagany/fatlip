(ns fatlip.core-test
  (:require-macros [cemerick.cljs.test :refer (is deftest are testing)])
  (:require [cemerick.cljs.test :as test]
            [fatlip.core :as f]))


(deftest test-graph-properties
  (let [input [{:groups [{:characters [:a :a]}]}]]
    (is (thrown? js/Error (f/make-sparse-graph input))
        "Duplicate characters in a layer throw an error"))

  (let [input [{:duration 10
                :groups [{:characters [:a :b :c]}
                         {:characters [:d :e :f]}
                         {:characters [:x :y :z]}]}
               {:duration 10
                :groups [{:characters [:a :d]}
                         {:characters [:b :y]}]}
               {:duration 10
                :groups [{:characters [:c :z]}]}
               {:duration 10
                :groups [{:characters [:e :f :x]}]}]
        graph (f/make-sparse-graph input)
        orderings (f/order-graph graph)
        ordered-graph (get orderings 0)]
    (is (= (count (:layers graph)) (count (:layers ordered-graph)) 4) "Number of layers")
    (is (= (count (-> graph :layers (get 0) :nodes))
           (count (-> ordered-graph :layers (get 0) :nodes))
           3)
        "Number of nodes in layer 0")
    (is (= (count (-> graph :layers (get 1) :nodes))
           (count (-> ordered-graph :layers (get 1) :nodes))
           6)
        "Number of nodes in layer 1")
    (is (= (count (-> graph :layers (get 2) :nodes))
           (count (-> ordered-graph :layers (get 2) :nodes))
           3)
        "Number of nodes in layer 2")
    (is (= (count (-> graph :layers (get 3) :nodes))
           (count (-> ordered-graph :layers (get 3) :nodes))
           1)
        "Number of nodes in layer 3")
    (is (= (count (:ps graph)) 2) "Number of p nodes")
    (is (= (count (:qs graph)) 2) "Number of q nodes")
    (is (= (count (:rs graph)) 2) "Number of r nodes")
    (is (= (count (mapcat (fn [[_ es]] es) (:succs graph)))
           (count (mapcat (fn [[_ es]] es) (:preds graph)))
           14)
        "Number of edges")

    ;; ESK ordering
    (is (= (count (:marked ordered-graph)) 1) "Number of marked edges")
    (is (= (:crossings ordered-graph) 9) "Number of crossings")
    (is (empty? (->> (:layers ordered-graph)
                     (mapcat #(:minus-ps %))
                     (filter #(contains? (:ps ordered-graph) %))))
        "No p nodes in :minus-ps")
    (is (empty? (->> (:layers ordered-graph)
                     (mapcat #(:minus-qs %))
                     (filter #(contains? (:qs ordered-graph) %))))
        "No q nodes in :minus-qs")))


;; Fine-grained ESK
(deftest test-replace-ps
  (let [p (f/Node. :0-0 0 [:a] 1)
        p-succ (f/Node. :1-0 0 [:a] 1)
        p-edge (f/Edge. p p-succ [:a] 1)
        not-p (f/Node. :0-1 0 [:b] 1)
        not-p-succ (f/Node. :1-0 0 [:b] 1)
        seg-c-edge (f/Edge. (f/Node. :-1:55 -1 [:c] 1) (f/Node. :3-99 3 [:c] 1) [:c] 1)
        seg-c [seg-c-edge]
        graph {:ps #{p}
               :succs {p #{p-edge}, not-p #{(f/Edge. not-p not-p-succ [:b] 1)}}}
        layer (-> (f/Layer. 0 0 [p seg-c not-p])
                  (assoc :ordered [p seg-c not-p]))]
    (is (= (f/replace-ps graph layer)
           (assoc layer :minus-ps [[p-edge seg-c-edge] not-p]))
        "P nodes get replaced by segment containers, and joined with adjacent segment containers")))


(deftest test-set-positions
  (let [seg-1 [(f/Edge. "src" "dest" [] 0)
               (f/Edge. "src" "dest" [] 0)]
        node-1 (f/Node. :0-0 0 [] 0)
        node-2 (f/Node. :0-1 0 [] 0)
        seg-2 [(f/Edge. "src" "dest" [] 0)
               (f/Edge. "src" "dest" [] 0)
               (f/Edge. "src" "dest" [] 0)]
        node-3 (f/Node. :0-2 0 [] 0)
        seg-3 [(f/Edge. "src" "dest" [] 0)]
        node-4 (f/Node. :0-2 0 [] 0)
        layer (-> (f/Layer. 0 0 [])
                  (assoc :minus-ps [seg-1 node-1 node-2 seg-2 node-3 seg-3 node-4]))]
    (is (= (f/set-positions layer) (assoc layer :positions {seg-1 0, node-1 2, node-2 3
                                                            seg-2 4, node-3 7, seg-3 8
                                                            node-4 9}))
        "Positions are set correctly")))


(deftest test-set-qs-non-qs
  (let [q (f/Node. :0-0 0 [] 0)
        not-q (f/Node. :0-1 0 [] 0)
        graph {:qs #{q}}
        layer (f/Layer. 0 0 [q not-q])]
    (is (= (f/set-qs-non-qs graph layer) (assoc layer :qs #{q} :non-qs #{not-q}))
        "Q nodes and non-q nodes are distinguised correctly")))


(deftest test-get-measure
  (let [node (f/Node. :1-0 1 [:a :b :c] 3)
        pred-1 (f/Node. :0-0 0 [:a :b] 2)
        pred-2 (f/Node. :0-1 0 [:c] 1)
        edge-1 (f/Edge. node pred-1 [:a :b] 2)
        edge-2 (f/Edge. node pred-2[:c] 1)
        graph {:preds {node #{edge-1 edge-2}}}
        pred-positions {pred-1 1, pred-2 2}]
    (is (= (f/get-measure graph node pred-positions) (/ 4 3))
        "The measure of a node is calculated correctly")))


(deftest test-set-measures
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        l-node-2 (f/Node. :0-1 0 [:d] 1)
        l-node-3 (f/Node. :0-2 0 [:e] 1)
        nl-node-1 (f/Node. :1-0 1 [:c] 1)
        nl-node-2 (f/Node. :1-1 1 [:b :e] 2)
        nl-node-3 (f/Node. :1-2 1 [:a] 1)
        nl-node-4 (f/Node. :1-3 1 [:d] 1)
        edge-1 (f/Edge. nl-node-1 l-node-1 [:c] 1)
        edge-2 (f/Edge. nl-node-2 l-node-1 [:b] 1)
        edge-3 (f/Edge. nl-node-2 l-node-3 [:e] 1)
        edge-4 (f/Edge. nl-node-3 l-node-1 [:a] 1)
        edge-5 (f/Edge. nl-node-4 l-node-2 [:d] 1)
        layer (-> (f/Layer. 0 0 [l-node-1 l-node-2 l-node-3])
                  (assoc :positions {l-node-1 0
                                     l-node-2 1
                                     l-node-3 2}))
        next-layer (-> (f/Layer. 1 0 [])
                       (assoc :non-qs [nl-node-1 nl-node-2 nl-node-3 nl-node-4]))
        graph {:preds {nl-node-1 #{edge-1}
                       nl-node-2 #{edge-2 edge-3}
                       nl-node-3 #{edge-4}
                       nl-node-4 #{edge-5}}}]
    (is (= (f/set-measures graph layer next-layer)
           (assoc next-layer :measures {nl-node-1 0
                                        nl-node-2 1
                                        nl-node-3 0
                                        nl-node-4 1}))
        "Measures are set correctly")))


(deftest test-order-next-layer
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        l-node-2 (f/Node. :0-1 0 [:d] 1)
        l-node-3 (f/Node. :0-2 0 [:e] 1)
        nl-node-1 (f/Node. :1-0 1 [:c] 1)
        nl-node-2 (f/Node. :1-1 1 [:b :e] 2)
        nl-node-3 (f/Node. :1-2 1 [:a] 1)
        nl-node-4 (f/Node. :1-3 1 [:d] 1)
        nl-node-5 (f/Node. :1-4 1 [:f :g] 2)
        l-seg-1 [(f/Edge. "whatever" "somewhere else" [:x] 1)]
        l-seg-2 [(f/Edge. "sup" "elsewhere" [:y] 1)
                 (f/Edge. "unclear" nl-node-5 [:f :g] 2)]
        edge-1 (f/Edge. nl-node-1 l-node-1 [:c] 1)
        edge-2 (f/Edge. nl-node-2 l-node-1 [:b] 1)
        edge-3 (f/Edge. nl-node-2 l-node-3 [:e] 1)
        edge-4 (f/Edge. nl-node-3 l-node-1 [:a] 1)
        edge-5 (f/Edge. nl-node-4 l-node-2 [:d] 1)
        layer (-> (f/Layer. 0 0 [l-node-1 l-node-2 l-node-3])
                  (assoc :positions {l-node-1 0
                                     l-seg-1 1
                                     l-node-2 2
                                     l-seg-2 3
                                     l-node-3 5}
                         :minus-ps [l-node-1 l-seg-1 l-node-2 l-seg-2 l-node-3]))
        next-layer (-> (f/Layer. 1 0 [])
                       (assoc :non-qs [nl-node-1 nl-node-2 nl-node-3 nl-node-4]
                              :measures {nl-node-1 0
                                         nl-node-2 2.5
                                         nl-node-3 0
                                         nl-node-4 2}))
        graph {:preds {nl-node-1 #{edge-1}
                       nl-node-2 #{edge-2 edge-3}
                       nl-node-3 #{edge-4}
                       nl-node-4 #{edge-5}}}]
    (is (= (f/order-next-layer layer next-layer)
           (assoc next-layer :minus-qs [nl-node-1 nl-node-3 l-seg-1 nl-node-4 nl-node-2 l-seg-2]))
        "Nodes and segment containers are merged correctly")))


(deftest test-add-qs
  (let [node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        node-2 (f/Node. :0-1 0 [:d] 1)
        node-3 (f/Node. :0-2 0 [:e] 1)
        edge-1 (f/Edge. "whatever" "somewhere else" [:x] 1)
        edge-2 (f/Edge. "sup" "elsewhere" [:y] 1)
        edge-3 (f/Edge. "unclear" node-2 [:f :g] 2)
        edge-4 (f/Edge. "doesn't" "matter" [:m] 1)
        seg-1 [edge-1]
        seg-2 [edge-2 edge-3 edge-4]
        layer (-> (f/Layer. 0 0 [node-1 node-2 node-3])
                  (assoc :qs #{node-2}
                         :minus-qs [node-1 seg-1 node-3 seg-2]))]
    (is (= (f/add-qs layer)
           (assoc layer
             :ordered [node-1 seg-1 node-3 [edge-2] node-2 [edge-4]]
             :flat [node-1 edge-1 node-3 edge-2 node-2 edge-4]))
        "Q nodes are placed correctly, splitting the segment containers they were in")))


(deftest test-sorted-edge-order
  (let [node-1 (f/Node. :0-0 0 [:a :d :e] 3)
        node-2 (f/Node. :0-1 0 [:c :g] 2)
        nl-node-1 (f/Node. :1-0 1 [:a :c] 2)
        nl-node-2 (f/Node. :1-1 1 [:d :g] 2)
        nl-node-3 (f/Node. :1-1 1 [:e] 1)
        seg [(f/Edge. "mmm" "hmm" [:b :f] 1)]
        ordered [node-1 seg node-2]
        next-ordered [nl-node-1 seg nl-node-2 nl-node-3]
        edge-1 (f/Edge. node-1 nl-node-1 [:a] 1)
        edge-2 (f/Edge. node-1 nl-node-2 [:d] 1)
        edge-3 (f/Edge. node-1 nl-node-3 [:e] 1)
        edge-4 (f/Edge. seg seg [:b :f] 2)
        edge-5 (f/Edge. node-2 nl-node-1 [:c] 1)
        edge-6 (f/Edge. node-2 nl-node-2 [:g] 1)
        edges {node-1 #{edge-1 edge-2 edge-3}
               node-2 #{edge-5 edge-6}}]
    (is (= (f/sorted-edge-order ordered next-ordered edges)
           [[0 edge-1] [2 edge-2] [3 edge-3] [1 edge-4] [0 edge-5] [2 edge-6]])
        (str "Edges between two orderings are sorted by position in "
             "layer and then position in next layer"))))


(deftest test-next-power-of-2
  (testing "Finding the next highest power of 2"
    (is (= (f/next-power-of-2 3) 4))
    (is (= (f/next-power-of-2 4) 8))
    (is (= (f/next-power-of-2 17) 32))
    (is (= (f/next-power-of-2 4094) 4096))))


(deftest test-single-edge-super-crossings
  ;; add edge that crosses segment x
  ;; add edge that doesn't cross segment x
  ;; add segment that crosses edge
  (let [graph {:marked #{}}
        fedge (f/Edge. (f/Node. :0-3 0 [:b] 1)
                       (f/Node. :1-3 1 [:b] 1)
                       [:b] 1)
        edge (with-meta fedge {:forward-edge fedge})
        seg [(f/Edge. "mmm" "hmm" [:b :h] 2)]
        sedge (f/Edge. seg seg [:b :h] 2)
        crossed-edge (f/Edge. (f/Node. :0-2 0 [:f] 1)
                              (f/Node. :1-2 1 [:f] 1)
                              [:f] 1)
        tree [(f/AccumulatorNode. 2
                                  #{(f/Edge. (f/Node. :0-0 0 [:a :d :e] 3)
                                             (f/Node. :1-0 1 [:a :d :e] 3)
                                             [:a :d :e] 3)
                                    (f/Edge. (f/Node. :0-1 0 [:c :g] 2)
                                             (f/Node. :1-1 1 [:c :g] 2)
                                             [:c :g] 2)}
                                  false)
              (f/AccumulatorNode. 1 #{} true)
              (f/AccumulatorNode. 1
                                  #{crossed-edge}
                                  false)]]
    (is (= (f/single-edge-super-crossings graph tree 3 edge)
           [(update-in graph [:marked] conj fedge) tree 3])
        "Correctly sum up and mark an edge that crosses a segment")
    (is (= (f/single-edge-super-crossings graph tree 5 edge)
           [graph
            (-> tree
                (update-in [0 :node-edges] conj fedge)
                (update-in [0 :weight] inc))
            1])
        "Correctly sum up and don't mark and edge that crosses a segment")
    (is (= (f/single-edge-super-crossings graph tree 5 sedge)
           [(update-in graph [:marked] conj crossed-edge)
            (-> tree
                (assoc-in [0 :is-seg-c] true)
                (update-in [0 :weight] + 2))
            2])
        "Correctly sum up crossings and markings when adding a segment")))


(deftest test-count-super-crossings
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        l-node-2 (f/Node. :0-1 0 [:d] 1)
        l-node-3 (f/Node. :0-2 0 [:e] 1)
        nl-node-1 (f/Node. :1-0 1 [:c] 2)
        nl-node-2 (f/Node. :1-1 1 [:b :e] 2)
        nl-node-3 (f/Node. :1-2 1 [:a] 1)
        nl-node-4 (f/Node. :1-3 1 [:d] 1)
        nl-node-5 (f/Node. :1-4 1 [:f :g] 2)
        l-seg-1 [(f/Edge. "whatever" "somewhere else" [:x] 1)]
        l-seg-2 [(f/Edge. "sup" "elsewhere" [:y] 1)
                 (f/Edge. "unclear" nl-node-5 [:f :g] 2)]
        fedge-1 (f/Edge. nl-node-1 l-node-1 [:c] 1)
        fedge-2 (f/Edge. nl-node-2 l-node-1 [:b] 1)
        fedge-3 (f/Edge. nl-node-2 l-node-3 [:e] 1)
        fedge-4 (f/Edge. nl-node-3 l-node-1 [:a] 1)
        fedge-5 (f/Edge. nl-node-4 l-node-2 [:d] 1)
        edge-1 (with-meta fedge-1 {:forward-edge fedge-1})
        edge-2 (with-meta fedge-2 {:forward-edge fedge-2})
        edge-3 (with-meta fedge-3 {:forward-edge fedge-3})
        edge-4 (with-meta fedge-4 {:forward-edge fedge-4})
        edge-5 (with-meta fedge-5 {:forward-edge fedge-5})
        layer (-> (f/Layer. 0 0 [l-node-1 l-node-2 l-node-3])
                  (assoc :minus-ps [l-node-1 l-seg-1 l-node-2 l-seg-2 l-node-3]))
        next-layer (-> (f/Layer. 1 0 [])
                       (assoc :minus-qs [nl-node-1 nl-node-3 l-seg-1 nl-node-4 nl-node-2 l-seg-2]))
        graph {:preds {nl-node-1 #{edge-1}
                       nl-node-2 #{edge-2 edge-3}
                       nl-node-3 #{edge-4}
                       nl-node-4 #{edge-5}}}]
    (is (= (f/count-super-crossings graph layer next-layer)
           [(assoc graph :marked #{fedge-2 fedge-3}) 5])
        "Super crossings are counted correctly")))


(deftest test-single-edge-sub-crossings
  (let [tree [2 1 1]]
    (is (= (f/single-edge-sub-crossings tree 3) [[2 1 1] 3])
        "Correctly sum up crossings from leaf 1")
    (is (= (f/single-edge-sub-crossings tree 4) [[2 2 1] 2])
        "Correctly sum up crossings from leaf 2")
    (is (= (f/single-edge-sub-crossings tree 5) [[3 1 1] 1])
        "Correctly sum up crossings from leaf 3")
    (is (= (f/single-edge-sub-crossings tree 6) [[3 1 2] 0])
        "Correctly sum up crossings from leaf 4")))


(deftest test-count-sub-crossings-single-node
  (let [node (f/Node. :0-0 0 [:a] 1)
        graph {:succs {}}
        next-layer (f/Layer. 0 0 [])]
    (is (= (f/count-sub-crossings-single-node graph node next-layer) 0)
        "A node with 0 successors has 0 sub-crossings"))
  (let [node (f/Node. :0-0 0 [:a] 1)
        succ (f/Node. :1-0 1 [:a] 1)
        graph {:succs {node #{(f/Edge. node succ [:a] 1)}}}
        next-layer (f/Layer. 1 0 [succ])]
    (is (= (f/count-sub-crossings-single-node graph node next-layer) 0)
        "A node with 1 successor has 0 sub-crossings"))
  (let [node (f/Node. :0-0 0 [:a :b] 2)
        succ-1 (f/Node. :1-0 1 [:a] 1)
        succ-2 (f/Node. :1-1 1 [:b] 1)
        graph {:succs {node (set (map #(f/Edge. node % (:characters %) 1)
                                      [succ-1 succ-2]))}}
        next-layer (-> (f/Layer. 1 0 [succ-1 succ-2])
                       (assoc :measures {succ-2 0 succ-1 1}))]
    (is (= (f/count-sub-crossings-single-node graph node next-layer) 1)
        "A node with multiple successors has its sub-crossings counted")))


(deftest test-count-sub-crossings
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        l-node-2 (f/Node. :0-1 0 [:d] 1)
        l-node-3 (f/Node. :0-2 0 [:e] 1)
        nl-node-1 (f/Node. :1-0 1 [:c] 1)
        nl-node-2 (f/Node. :1-1 1 [:b] 1)
        nl-node-3 (f/Node. :1-2 1 [:a] 1)
        nl-node-4 (f/Node. :1-3 1 [:d] 1)
        layer (-> (f/Layer. 0 0 [l-node-1 l-node-2 l-node-3])
                  (assoc :minus-ps [l-node-1 l-node-2 l-node-3]))
        next-layer (-> (f/Layer. 1 0 [nl-node-1 nl-node-2 nl-node-3 nl-node-4])
                       (assoc :measures {nl-node-1 0, nl-node-2 1,
                                         nl-node-3 2, nl-node-4 3}))
        graph {:succs {l-node-1 (set (map #(f/Edge. l-node-1 % (:characters %) (:weight %))
                                          [nl-node-1 nl-node-2 nl-node-3]))
                       l-node-2 (set (map #(f/Edge. l-node-2 % (:characters %) (:weight %))
                                          [nl-node-4]))}}]
    (is (= (f/count-sub-crossings graph layer next-layer) 3)
        "Sub crossings are counted correctly")))


(deftest test-neighborify
  (let [node-1 (f/Node. :0-0 0 [:a] 1)
        node-2 (f/Node. :0-1 0 [:b] 1)
        node-3 (f/Node. :0-2 0 [:c] 1)
        edge-1 (f/Edge. "meh" "bleh" [:d] 1)
        edge-2 (f/Edge. "feh" "geh" [:e] 1)
        layer (-> (f/Layer. 0 0 [node-1 node-2 node-3])
                  (assoc :flat [node-1 edge-1 node-2 edge-2 node-3]))
        graph {:belows {} :aboves {}}]
    (is (= (f/neighborify graph layer)
           (assoc graph
             :belows {0 {node-1 edge-1
                         edge-1 node-2
                         node-2 edge-2
                         edge-2 node-3}}
             :aboves {0 {node-3 edge-2
                         edge-2 node-2
                         node-2 edge-1
                         edge-1 node-1}}))
        "Mapping nodes and edges that are above or below each thing works")))


(deftest test-indexify
  (let [node-1 (f/Node. :0-0 0 [:a] 1)
        node-2 (f/Node. :0-1 0 [:b] 1)
        node-3 (f/Node. :0-2 0 [:c] 1)
        edge-1 (f/Edge. "meh" "bleh" [:d] 1)
        edge-2 (f/Edge. "feh" "geh" [:e] 1)
        layer (-> (f/Layer. 0 0 [node-1 node-2 node-3])
                  (assoc :flat [node-1 edge-1 node-2 edge-2 node-3])
                  (assoc :ordered [node-1 [edge-1] node-2 [edge-2] node-3]))
        graph {:top-idxs {} :bot-idxs {} :node-orders {} :rev-node-orders {}}]
    (is (= (f/indexify graph layer)
           (assoc graph
             :top-idxs {0 {node-1 0
                           edge-1 1
                           node-2 2
                           edge-2 3
                           node-3 4}}
             :bot-idxs {0 {node-3 0
                           edge-2 1
                           node-2 2
                           edge-1 3
                           node-1 4}}
             :node-orders {0 [node-1 node-2 node-3]}
             :rev-node-orders {0 [node-3 node-2 node-1]}))
        "Mapping nodes and edges to indexes from the top and bottom of the layer")))


(deftest test-seed-graph
  (let [node-1 (f/Node. :0-0 0 [:a] 1)
        node-2 (f/Node. :0-1 0 [:b] 1)
        node-3 (f/Node. :0-2 0 [:c] 1)
        layer (-> (f/Layer. 0 0 [node-1 node-2 node-3])
                  (assoc :ordered [node-1 node-2 node-3]))
        graph {:layers [layer] :belows {} :aboves {} :top-idxs {} :bot-idxs {}}
        seed-order [node-1 node-2 node-3]]
    (is (= (f/seed-graph graph seed-order)
           (assoc graph
             :layers [(assoc layer
                        :flat seed-order
                        :ordered seed-order)]
             :belows {0 {node-1 node-2
                         node-2 node-3}}
             :aboves {0 {node-3 node-2
                         node-2 node-1}}
             :top-idxs {0 {node-1 0
                           node-2 1
                           node-3 2}}
             :bot-idxs {0 {node-3 0
                           node-2 1
                           node-1 2}}
             :node-orders {0 [node-1 node-2 node-3]}
             :rev-node-orders {0 [node-3 node-2 node-1]}))
        "Seeding the graph ordering with the initial layer")))


(deftest test-check-alignment
  (let [pred-edge (f/Edge. nil nil [:a] 1)
        pred {:idx 2 :edge pred-edge :item nil}
        marked {:marked #{pred-edge}}
        unmarked {:marked #{}}]
    (is (nil? (f/check-alignment marked pred 1))
        "Marked edge returns nil with last-idx < idx")
    (is (nil? (f/check-alignment marked pred 2))
        "Marked edge returns nil with last-idx = idx")
    (is (nil? (f/check-alignment marked pred 3))
        "Marked edge returns nil with last-idx > idx")
    (is (= (f/check-alignment unmarked pred 1) pred)
        "Unmarked edge returns the pred with last-idx < idx")
    (is (nil? (f/check-alignment unmarked pred 2))
        "Unmarked edge returns nil with last-idx = idx")
    (is (nil? (f/check-alignment unmarked pred 3))
        "Unmarked edge returns nil with last-idx > idx")))


(deftest test-blockify
  (let [;; Even predecessors
        node-1 (f/Node. :0-0 0 [:a] 1)
        node-2 (f/Node. :0-1 0 [:b] 1)
        ;; Odd predecessors
        node-3 (f/Node. :0-2 0 [:c] 1)
        node-4 (f/Node. :0-3 0 [:d] 1)
        node-5 (f/Node. :0-4 0 [:e] 1)
        ;; weighted median
        node-6 (f/Node. :0-5 0 [:f :g :h :i] 4)
        node-7 (f/Node. :0-6 0 [:j] 1)
        node-8 (f/Node. :0-7 0 [:k] 1)

        node-9 (f/Node. :1-0 1 [:a :b] 2)
        edge-1 (f/Edge. node-9 node-1 [:a] 1)
        edge-2 (f/Edge. node-9 node-2 [:b] 1)
        node-10 (f/Node. :1-1 1 [:c :d :e] 1)
        edge-3 (f/Edge. node-10 node-3 [:c] 1)
        edge-4 (f/Edge. node-10 node-4 [:d] 1)
        edge-5 (f/Edge. node-10 node-5 [:e] 1)
        node-11 (f/Node. :1-2 1 [:f :g :h :i :j :k] 6)
        edge-6 (f/Edge. node-11 node-6 [:f :g :h :i] 4)
        edge-7 (f/Edge. node-11 node-7 [:j] 1)
        edge-8 (f/Edge. node-11 node-8 [:k] 1)
        layer-1 (f/Layer. 0 0 [node-1 node-2 node-3 node-4 node-5 node-6 node-7 node-8])
        layer-2 (f/Layer. 1 0 [node-9 node-10 node-11])
        graph {:layers [layer-1 layer-2]
               :preds {node-9 #{edge-1 edge-2}
                       node-10 #{edge-3 edge-4 edge-5}
                       node-11 #{edge-6 edge-7 edge-8}}
               :top-idxs {0 {node-1 0
                             node-2 1
                             node-3 2
                             node-4 3
                             node-5 4
                             node-6 5
                             node-7 6
                             node-8 7}}
               :roots {node-1 node-1
                       node-2 node-2
                       node-3 node-3
                       node-4 node-4
                       node-5 node-5
                       node-6 node-6
                       node-7 node-7
                       node-8 node-8}
               :blocks {node-1 [node-1]
                        node-2 [node-2]
                        node-3 [node-3]
                        node-4 [node-4]
                        node-5 [node-5]
                        node-6 [node-6]
                        node-7 [node-7]
                        node-8 [node-8]}
               :node-orders {0 [node-1 node-2 node-3 node-4
                                node-5 node-6 node-7 node-8]
                             1 [node-9 node-10 node-11]}}]
    (is (= (f/blockify graph)
           (-> graph
               (update-in [:roots] assoc
                          node-9 node-1
                          node-10 node-4
                          node-11 node-6)
               (update-in [:blocks node-1] conj node-9)
               (update-in [:blocks node-4] conj node-10)
               (update-in [:blocks node-6] conj node-11)))
        "Base case for even preds, odd preds, and weighted edges work right")
    (let [g (assoc graph :marked #{edge-1})]
      (is (= (f/blockify g)
             (-> g
                 (update-in [:roots] assoc
                            node-9 node-2
                            node-10 node-4
                            node-11 node-6)
                 (update-in [:blocks node-2] conj node-9)
                 (update-in [:blocks node-4] conj node-10)
                 (update-in [:blocks node-6] conj node-11)))
          "Marked first medians are skipped"))
    (let [g (-> (assoc-in graph [:top-idxs 0 node-1] 1)
                (assoc-in [:top-idxs 0 node-2] 0))]
      (is (= (f/blockify g)
             (-> g
                 (update-in [:roots] assoc
                            node-9 node-2
                            node-10 node-4
                            node-11 node-6)
                 (update-in [:blocks node-2] conj node-9)
                 (update-in [:blocks node-4] conj node-10)
                 (update-in [:blocks node-6] conj node-11)))
          "First medians that have already been crossed are skipped"))
    (let [g (assoc graph :marked #{edge-1 edge-2 edge-3 edge-4 edge-5})]
      (is (= (f/blockify g)
             (-> g
                 (update-in [:roots] assoc
                            node-9 node-9
                            node-10 node-10
                            node-11 node-6)
                 (assoc-in [:blocks node-9] [node-9])
                 (assoc-in [:blocks node-10] [node-10])
                 (update-in [:blocks node-6] conj node-11)))
          "Nodes without valid medians start their own blocks"))))
