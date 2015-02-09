(ns fatlip.sparse-test
  (:require-macros [cemerick.cljs.test :refer (is deftest are testing)])
  (:require [cemerick.cljs.test :as test]
            [fatlip.sparse :as f]))


(deftest test-graph-properties
  (let [input [{:groups [{:characters [:a :a]}]}]]
    (is (thrown? js/Error (f/input->SparseGraph input))
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
        graph (f/input->SparseGraph input)]
    (is (= (count (:layers graph))
           4)
        "Number of layers")
    (is (= (-> graph :layers (get 0) :nodes count)
           3)
        "Number of nodes in layer 0")
    (is (= (-> graph :layers (get 1) :nodes count)
           6)
        "Number of nodes in layer 1")
    (is (= (-> graph :layers (get 2) :nodes count)
           3)
        "Number of nodes in layer 2")
    (is (= (-> graph :layers (get 3) :nodes count)
           1)
        "Number of nodes in layer 3")
    (is (= (count (:ps graph)) 2) "Number of p nodes")
    (is (= (count (:qs graph)) 2) "Number of q nodes")
    (is (= (count (:rs graph)) 2) "Number of r nodes")
    (is (= (reduce + (map count (vals (:succs graph))))
           (reduce + (map count (vals (:preds graph))))
           14)
        "Number of edges")))
