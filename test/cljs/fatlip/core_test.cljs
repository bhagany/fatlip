(ns fatlip.core-test
  (:require-macros [cemerick.cljs.test :refer (is deftest)])
  (:require [cemerick.cljs.test :as test]
            [fatlip.core :as f]))


(deftest input-parsing
  (let [input [{:duration 10
                :groups [{:characters #{:a :b :c}}
                         {:characters #{:d :e :f}}
                         {:characters #{:x :y :z}}]}
               {:duration 10
                :groups [{:characters #{:a :d}}
                         {:characters #{:b :y}}]}
               {:duration 10
                :groups [{:characters #{:c :z}}]}
               {:duration 10
                :groups [{:characters #{:e :f :x}}]}]
        graph (f/make-sparse-graph input)
        ordered-graph (get (f/order-graph graph) 19)
        ;; _ (println (:succs graph))
        ]
    (is (= (count (:layers graph)) (count (:layers ordered-graph)) 4) "Number of layers")
    (is (= (count (-> graph :layers (get 0) :nodes))
           (count (-> ordered-graph :layers (get 0) :nodes))
           3)
        "Number of nodes in layer 1")
    (is (= (count (-> graph :layers (get 1) :nodes))
           (count (-> ordered-graph :layers (get 1) :nodes))
           6)
        "Number of nodes in layer 2")
    (is (= (count (-> graph :layers (get 2) :nodes))
           (count (-> ordered-graph :layers (get 2) :nodes))
           3)
        "Number of nodes in layer 3")
    (is (= (count (-> graph :layers (get 3) :nodes))
           (count (-> ordered-graph :layers (get 3) :nodes))
           1)
        "Number of nodes in layer 4")
    (is (= (count (:p graph)) 2) "Number of p nodes")
    (is (= (count (:q graph)) 2) "Number of q nodes")
    (is (= (count (:r graph)) 2) "Number of r nodes")
    (is (= (count (mapcat (fn [[n es]] es) (:succs graph)))
           (count (mapcat (fn [[n es]] es) (:preds graph)))
           14)
        "Number of edges")
  ))
