(ns fatlip.core-test
  (:require-macros [cemerick.cljs.test :refer (is deftest)])
  (:require [cemerick.cljs.test :as test]
            [fatlip.core :as f]))


(deftest graph-properties
  (let [input [{:groups [{:characters [:a :a]}]}]]
    (is (thrown? js/Error (f/make-sparse-graph input))))

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
    (is (= (:crossings ordered-graph) 5) "Number of crossings")
    (is (empty? (->> (:layers ordered-graph)
                     (mapcat #(:minus-ps %))
                     (filter #(contains? (:p ordered-graph) %))))
        "No p nodes in :minus-ps")
    (is (empty? (->> (:layers ordered-graph)
                     (mapcat #(:minus-qs %))
                     (filter #(contains? (:q ordered-graph) %))))
        "No q nodes in :minus-qs")))


;; Fine-grained ESK
(deftest replace-ps
  (let [p (f/Node. :0-0 0 #{:a})
        not-p (f/Node. :0-1 0 #{:b})
        p-succ (f/Node. :1-0 0 #{:a})
        not-p-succ (f/Node. :1-0 0 #{:b})
        seg-c-edge (f/Edge. (f/Node. :-1:55 -1 #{:c}) (f/Node. :3-99 3 #{:c}) #{:c})
        seg-c (f/SegmentContainer. [seg-c-edge])
        p-edge (f/Edge. p p-succ #{:a})
        graph {:p #{p}
               :succs {p #{p-edge}, not-p #{(f/Edge. not-p not-p-succ #{:b})}}}
        layer (-> (f/Layer. 0 0 [p seg-c not-p])
                  (assoc :ordered [p seg-c not-p]))]
    (is (= (f/replace-ps graph layer)
           (assoc layer :minus-ps [(f/SegmentContainer. [p-edge seg-c-edge]) not-p]))
        "P nodes get replaced by segment containers, and joined with adjacent segment containers")))


(deftest set-positions
  (let [seg-1 (f/SegmentContainer. [(f/Edge. "src" "dest" #{})
                                    (f/Edge. "src" "dest" #{})])
        node-1 (f/Node. :0-0 0 #{})
        node-2 (f/Node. :0-1 0 #{})
        seg-2 (f/SegmentContainer. [(f/Edge. "src" "dest" #{})
                                    (f/Edge. "src" "dest" #{})
                                    (f/Edge. "src" "dest" #{})])
        node-3 (f/Node. :0-2 0 #{})
        seg-3 (f/SegmentContainer. [(f/Edge. "src" "dest" #{})])
        node-4 (f/Node. :0-2 0 #{})
        layer (-> (f/Layer. 0 0 [])
                  (assoc :minus-ps [seg-1 node-1 node-2 seg-2 node-3 seg-3 node-4]))]
    (is (= (f/set-positions layer) (assoc layer :positions {seg-1 0, node-1 2, node-2 3
                                                            seg-2 4, node-3 7, seg-3 8
                                                            node-4 9}))
        "Positions are set correctly")))


(deftest set-qs-non-qs
  (let [q (f/Node. :0-0 0 #{})
        not-q (f/Node. :0-1 0 #{})
        graph {:q #{q}}
        layer (f/Layer. 0 0 [q not-q])]
    (is (= (f/set-qs-non-qs graph layer) (assoc layer :qs #{q} :non-qs #{not-q}))
        "Q nodes and non-q nodes are distinguised correctly")))
