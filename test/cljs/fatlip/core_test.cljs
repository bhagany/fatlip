(ns fatlip.core-test
  (:require-macros [cemerick.cljs.test :refer (is deftest)])
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
    (is (= (count (:p graph)) 2) "Number of p nodes")
    (is (= (count (:q graph)) 2) "Number of q nodes")
    (is (= (count (:r graph)) 2) "Number of r nodes")
    (is (= (count (mapcat (fn [[_ es]] es) (:succs graph)))
           (count (mapcat (fn [[_ es]] es) (:preds graph)))
           14)
        "Number of edges")

    ;; ESK ordering
    (is (= (count (:marked ordered-graph)) 1) "Number of marked edges")
    (is (= (:crossings ordered-graph) 9) "Number of crossings")
    (is (empty? (->> (:layers ordered-graph)
                     (mapcat #(:minus-ps %))
                     (filter #(contains? (:p ordered-graph) %))))
        "No p nodes in :minus-ps")
    (is (empty? (->> (:layers ordered-graph)
                     (mapcat #(:minus-qs %))
                     (filter #(contains? (:q ordered-graph) %))))
        "No q nodes in :minus-qs")))


;; Fine-grained ESK
(deftest test-replace-ps
  (let [p (f/Node. :0-0 0 [:a])
        p-succ (f/Node. :1-0 0 [:a])
        p-edge (f/Edge. p p-succ [:a])
        not-p (f/Node. :0-1 0 [:b])
        not-p-succ (f/Node. :1-0 0 [:b])
        seg-c-edge (f/Edge. (f/Node. :-1:55 -1 [:c]) (f/Node. :3-99 3 [:c]) [:c])
        seg-c (f/SegmentContainer. [seg-c-edge])
        graph {:p #{p}
               :succs {p #{p-edge}, not-p #{(f/Edge. not-p not-p-succ [:b])}}}
        layer (-> (f/Layer. 0 0 [p seg-c not-p])
                  (assoc :ordered [p seg-c not-p]))]
    (is (= (f/replace-ps graph layer)
           (assoc layer :minus-ps [(f/SegmentContainer. [p-edge seg-c-edge]) not-p]))
        "P nodes get replaced by segment containers, and joined with adjacent segment containers")))


(deftest test-set-positions
  (let [seg-1 (f/SegmentContainer. [(f/Edge. "src" "dest" [])
                                    (f/Edge. "src" "dest" [])])
        node-1 (f/Node. :0-0 0 [])
        node-2 (f/Node. :0-1 0 [])
        seg-2 (f/SegmentContainer. [(f/Edge. "src" "dest" [])
                                    (f/Edge. "src" "dest" [])
                                    (f/Edge. "src" "dest" [])])
        node-3 (f/Node. :0-2 0 [])
        seg-3 (f/SegmentContainer. [(f/Edge. "src" "dest" [])])
        node-4 (f/Node. :0-2 0 [])
        layer (-> (f/Layer. 0 0 [])
                  (assoc :minus-ps [seg-1 node-1 node-2 seg-2 node-3 seg-3 node-4]))]
    (is (= (f/set-positions layer) (assoc layer :positions {seg-1 0, node-1 2, node-2 3
                                                            seg-2 4, node-3 7, seg-3 8
                                                            node-4 9}))
        "Positions are set correctly")))


(deftest test-set-qs-non-qs
  (let [q (f/Node. :0-0 0 [])
        not-q (f/Node. :0-1 0 [])
        graph {:q #{q}}
        layer (f/Layer. 0 0 [q not-q])]
    (is (= (f/set-qs-non-qs graph layer) (assoc layer :qs #{q} :non-qs #{not-q}))
        "Q nodes and non-q nodes are distinguised correctly")))


(deftest test-get-measure
  (let [node (f/Node. :1-0 1 [:a :b :c])
        pred-1 (f/Node. :0-0 0 [:a :b])
        pred-2 (f/Node. :0-1 0 [:c])
        edge-1 (f/Edge. node pred-1 [:a :b])
        edge-2 (f/Edge. node pred-2[:c])
        graph {:preds {node #{edge-1 edge-2}}}
        pred-positions {pred-1 1, pred-2 2}]
    (is (= (f/get-measure graph node pred-positions) (/ 4 3))
        "The measure of a node is calculated correctly")))


(deftest test-set-measures
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c])
        l-node-2 (f/Node. :0-1 0 [:d])
        l-node-3 (f/Node. :0-2 0 [:e])
        nl-node-1 (f/Node. :1-0 1 [:c])
        nl-node-2 (f/Node. :1-1 1 [:b :e])
        nl-node-3 (f/Node. :1-2 1 [:a])
        nl-node-4 (f/Node. :1-3 1 [:d])
        edge-1 (f/Edge. nl-node-1 l-node-1 [:c])
        edge-2 (f/Edge. nl-node-2 l-node-1 [:b])
        edge-3 (f/Edge. nl-node-2 l-node-3 [:e])
        edge-4 (f/Edge. nl-node-3 l-node-1 [:a])
        edge-5 (f/Edge. nl-node-4 l-node-2 [:d])
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
  (let [node (f/Node. :0-0 0 [:a])
        graph {:succs {}}
        next-layer (f/Layer. 0 0 [])]
    (is (= (f/count-sub-crossings-single-node graph node next-layer) 0)
        "A node with 0 successors has 0 sub-crossings"))
  (let [node (f/Node. :0-0 0 [:a])
        succ (f/Node. :1-0 1 [:a])
        graph {:succs {node #{(f/Edge. node succ [:a])}}}
        next-layer (f/Layer. 1 0 [succ])]
    (is (= (f/count-sub-crossings-single-node graph node next-layer) 0)
        "A node with 1 successor has 0 sub-crossings"))
  (let [node (f/Node. :0-0 0 [:a :b])
        succ-1 (f/Node. :1-0 1 [:a])
        succ-2 (f/Node. :1-1 1 [:b])
        graph {:succs {node (set (map #(f/Edge. node % (:characters %))
                                      [succ-1 succ-2]))}}
        next-layer (-> (f/Layer. 1 0 [succ-1 succ-2])
                       (assoc :measures {succ-2 0 succ-1 1}))]
    (is (= (f/count-sub-crossings-single-node graph node next-layer) 1)
        "A node with multiple successors has its sub-crossings counted")))


(deftest test-count-sub-crossings
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c])
        l-node-2 (f/Node. :0-1 0 [:d])
        l-node-3 (f/Node. :0-2 0 [:e])
        nl-node-1 (f/Node. :1-0 1 [:c])
        nl-node-2 (f/Node. :1-1 1 [:b])
        nl-node-3 (f/Node. :1-2 1 [:a])
        nl-node-4 (f/Node. :1-3 1 [:d])
        layer (-> (f/Layer. 0 0 [l-node-1 l-node-2 l-node-3])
                  (assoc :minus-ps [l-node-1 l-node-2 l-node-3]))
        next-layer (-> (f/Layer. 1 0 [nl-node-1 nl-node-2 nl-node-3 nl-node-4])
                       (assoc :measures {nl-node-1 0, nl-node-2 1,
                                         nl-node-3 2, nl-node-4 3}))
        graph {:succs {l-node-1 (set (map #(f/Edge. l-node-1 % (:characters %))
                                          [nl-node-1 nl-node-2 nl-node-3]))
                       l-node-2 (set (map #(f/Edge. l-node-2 % (:characters %))
                                          [nl-node-4]))}}]
    (is (= (f/count-sub-crossings graph layer next-layer) 3)
        "Sub crossings are counted correctly")))
