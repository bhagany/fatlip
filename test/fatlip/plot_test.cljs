(ns fatlip.plot-test
  (:require [cljs.test :refer-macros [is deftest are testing]]
            [fatlip.protocols :refer [Node Edge Segment Edge->Segment]]
            [fatlip.core :as fc]
            [fatlip.order :as fo]
            [fatlip.plot :as f]))


(deftest test-check-alignment
  (let [pred-edge (Edge. nil nil #{:a} 1)
        pred {:idx 2 :edge pred-edge :item nil}
        marked #{pred-edge}
        unmarked #{}]
    (is (nil? (f/check-alignment pred 1 marked))
        "Marked edge returns nil with last-idx < idx")
    (is (nil? (f/check-alignment pred 2 marked))
        "Marked edge returns nil with last-idx = idx")
    (is (nil? (f/check-alignment pred 3 marked))
        "Marked edge returns nil with last-idx > idx")
    (is (= (f/check-alignment pred 1 unmarked) pred)
        "Unmarked edge returns the pred with last-idx < idx")
    (is (nil? (f/check-alignment pred 2 unmarked))
        "Unmarked edge returns nil with last-idx = idx")
    (is (nil? (f/check-alignment pred 3 unmarked))
        "Unmarked edge returns nil with last-idx > idx")))


(deftest test-pred->segs+src
  (let [node-1 (Node. :5-0 5 #{:a} 1)
        node-2 (Node. :0-0 0 #{:a} 1)
        pred (Edge. node-1 node-2 #{:a} 1)
        rev-pred (Edge. node-2 node-1 #{:a} 1)]
    (is (= (f/pred->segs+src pred)
           [(Segment. #{node-1 node-2} 1 #{:a} 1)
            (Segment. #{node-1 node-2} 2 #{:a} 1)
            (Segment. #{node-1 node-2} 3 #{:a} 1)
            (Segment. #{node-1 node-2} 4 #{:a} 1)
            node-1])
        "Predecessors are processed correctly when layers are increasing")
    (is (= (f/pred->segs+src rev-pred)
           [(Segment. #{node-1 node-2} 4 #{:a} 1)
            (Segment. #{node-1 node-2} 3 #{:a} 1)
            (Segment. #{node-1 node-2} 2 #{:a} 1)
            (Segment. #{node-1 node-2} 1 #{:a} 1)
            node-2])
        "Predecessors are processed correctly when layers are decreasing")))


(deftest test-blockify
  (let [;; Even predecessors
        node-1 (Node. :0-0 0 #{:a} 1)
        node-2 (Node. :0-1 0 #{:b} 1)
        ;; Odd predecessors
        node-3 (Node. :0-2 0 #{:c} 1)
        node-4 (Node. :0-3 0 #{:d} 1)
        node-5 (Node. :0-4 0 #{:e} 1)
        ;; weighted median
        node-6 (Node. :0-5 0 #{:f :g :h :i} 4)
        node-7 (Node. :0-6 0 #{:j} 1)
        node-8 (Node. :0-7 0 #{:k} 1)

        node-9 (Node. :1-0 1 #{:a :b} 2)
        edge-1 (Edge. node-9 node-1 #{:a} 1)
        edge-2 (Edge. node-9 node-2 #{:b} 1)
        node-10 (Node. :1-1 1 #{:c :d :e} 1)
        edge-3 (Edge. node-10 node-3 #{:c} 1)
        edge-4 (Edge. node-10 node-4 #{:d} 1)
        edge-5 (Edge. node-10 node-5 #{:e} 1)
        node-11 (Node. :1-2 1 #{:f :g :h :i :j :k} 6)
        edge-6 (Edge. node-11 node-6 #{:f :g :h :i} 4)
        edge-7 (Edge. node-11 node-7 #{:j} 1)
        edge-8 (Edge. node-11 node-8 #{:k} 1)
        layer-1 (fo/FlatLayer. 0 0 [node-1 node-2 node-3 node-4 node-5 node-6 node-7 node-8])
        layer-2 (fo/FlatLayer. 1 0 [node-9 node-10 node-11])
        graph (fo/map->FlatGraph {:layers [layer-1 layer-2]
                                 :succs {}
                                 :preds {node-9 #{edge-1 edge-2}
                                         node-10 #{edge-3 edge-4 edge-5}
                                         node-11 #{edge-6 edge-7 edge-8}}
                                 :aboves {}
                                 :belows {}
                                 :top-idxs {node-1 0
                                            node-2 1
                                            node-3 2
                                            node-4 3
                                            node-5 4
                                            node-6 5
                                            node-7 6
                                            node-8 7}
                                 :bot-idxs {}})
        base-roots {node-1 node-1
                    node-2 node-2
                    node-3 node-3
                    node-4 node-4
                    node-5 node-5
                    node-6 node-6
                    node-7 node-7
                    node-8 node-8}
        base-blocks {node-1 [node-1]
                     node-2 [node-2]
                     node-3 [node-3]
                     node-4 [node-4]
                     node-5 [node-5]
                     node-6 [node-6]
                     node-7 [node-7]
                     node-8 [node-8]}]
    (is (= (f/blockify graph)
           [(assoc base-roots
               node-9 node-1
               node-10 node-4
               node-11 node-6)
            (-> base-blocks
                (update-in [node-1] conj node-9)
                (update-in [node-4] conj node-10)
                (update-in [node-6] conj node-11))])
        "Base case for even preds, odd preds, and weighted edges work right")
    (let [g (assoc graph :marked #{edge-1})]
      (is (= (f/blockify g)
             [(assoc base-roots
                node-9 node-2
                node-10 node-4
                node-11 node-6)
              (-> base-blocks
                  (update-in [node-2] conj node-9)
                  (update-in [node-4] conj node-10)
                  (update-in [node-6] conj node-11))])
          "Marked first medians are skipped"))
    (let [g (-> (assoc-in graph [:top-idxs node-1] 1)
                (assoc-in [:top-idxs node-2] 0))]
      (is (= (f/blockify g)
             [(assoc base-roots
                 node-9 node-2
                 node-10 node-4
                 node-11 node-6)
              (-> base-blocks
                  (update-in [node-2] conj node-9)
                  (update-in [node-4] conj node-10)
                  (update-in [node-6] conj node-11))])
          "First medians that have already been crossed are skipped"))
    (let [g (assoc graph :marked #{edge-1 edge-2 edge-3 edge-4 edge-5})]
      (is (= (f/blockify g)
             [(assoc base-roots
                 node-9 node-9
                 node-10 node-10
                 node-11 node-6)
              (-> base-blocks
                  (assoc-in [node-9] [node-9])
                  (assoc-in [node-10] [node-10])
                  (update-in [node-6] conj node-11))])
          "Nodes without valid medians start their own blocks"))))


(let [node-0-0 (Node. :0-0 0 #{:a :b :c :d :e :f
                                 :g :h :i :j :k} 11)
      node-1-0 (Node. :1-0 1 #{:a :b :c :d :e} 5)
      node-1-1 (Node. :1-1 1 #{:f :g :h :i :j :k} 6)
      node-2-0 (Node. :2-0 2 #{:a :b :c :d} 4)
      node-2-1 (Node. :2-1 2 #{:f :g :h} 3)
      node-2-2 (Node. :2-2 2 #{:i :j} 2)
      node-2-3 (Node. :2-3 2 #{:k} 1)
      node-3-0 (Node. :3-0 3 #{:l :m :n :o :p :q :r :s} 8)
      node-3-1 (Node. :3-1 3 #{:f :g :h} 3)
      node-3-2 (Node. :3-2 3 #{:k} 1)
      node-4-0 (Node. :4-0 4 #{:l :m :n :o :p :q :r :s} 8)
      node-4-1 (Node. :4-1 4 #{:t :u :v :w :x} 5)
      node-4-2 (Node. :4-2 4 #{:i :j} 2)
      node-5-0 (Node. :5-0 5 #{:t :u :v :w :x} 5)
      node-6-0 (Node. :6-0 6 #{:y :z} 2)
      node-6-1 (Node. :6-1 6 #{:t :u :v :w :x} 5)
      node-7-0 (Node. :7-0 7 #{:1 :2 :3} 3)
      node-7-1 (Node. :7-1 7 #{:y :z} 2)
      node-8-0 (Node. :8-0 8 #{:1 :2 :3} 3)
      pred-1 (Edge. node-1-0 node-0-0 #{:a :b :c :d :e} 5)
      pred-2 (Edge. node-1-1 node-0-0 #{:f :g :h :i :j :k} 6)
      pred-3 (Edge. node-2-0 node-1-0 #{:a :b :c :d} 4)
      pred-4 (Edge. node-2-1 node-1-1 #{:f :g :h} 3)
      pred-5 (Edge. node-2-2 node-1-1 #{:i :j} 2)
      pred-6 (Edge. node-2-3 node-1-1 #{:k} 1)
      pred-7 (Edge. node-3-1 node-2-1 #{:f :g :h} 3)
      pred-8 (Edge. node-4-2 node-2-2 #{:i :j} 2)
      pred-9 (Edge. node-3-2 node-2-3 #{:k} 1)
      pred-10 (Edge. node-4-0 node-3-0 #{:l :m :n :o :p :q :r :s} 8)
      pred-11 (Edge. node-5-0 node-4-1 #{:t :u :v :w :x} 5)
      pred-12 (Edge. node-6-1 node-5-0 #{:t :u :v :w :x} 5)
      pred-13 (Edge. node-7-1 node-6-0 #{:y :z} 2)
      pred-14 (Edge. node-8-0 node-7-0 #{:1 :2 :3} 3)
      succ-1 (Edge. node-0-0 node-1-0 #{:a :b :c :d :e} 5)
      succ-2 (Edge. node-0-0 node-1-1 #{:f :g :h :i :j :k} 6)
      succ-3 (Edge. node-1-0 node-2-0 #{:a :b :c :d} 4)
      succ-4 (Edge. node-1-1 node-2-1 #{:f :g :h} 3)
      succ-5 (Edge. node-1-1 node-2-2 #{:i :j} 2)
      succ-6 (Edge. node-1-1 node-2-3 #{:k} 1)
      succ-7 (Edge. node-2-1 node-3-1 #{:f :g :h} 3)
      succ-8 (Edge. node-2-2 node-4-2 #{:i :j} 2)
      succ-9 (Edge. node-2-3 node-3-2 #{:k} 1)
      succ-10 (Edge. node-3-0 node-4-0 #{:l :m :n :o :p :q :r :s} 8)
      succ-11 (Edge. node-4-1 node-5-0 #{:t :u :v :w :x} 5)
      succ-12 (Edge. node-5-0 node-6-1 #{:t :u :v :w :x} 5)
      succ-13 (Edge. node-6-0 node-7-1 #{:y :z} 2)
      succ-14 (Edge. node-7-0 node-8-0 #{:1 :2 :3} 3)
      layer-0 (fo/FlatLayer. 0 0 [node-0-0])
      layer-1 (fo/FlatLayer. 1 0 [node-1-0 node-1-1])
      layer-2 (fo/FlatLayer. 2 0 [node-2-0 node-2-1 node-2-2 node-2-3])
      layer-3 (fo/FlatLayer. 3 0 [node-3-0 node-3-1 node-3-2])
      layer-4 (fo/FlatLayer. 4 0 [node-4-0 node-4-1 node-4-2])
      layer-5 (fo/FlatLayer. 5 0 [node-5-0])
      layer-6 (fo/FlatLayer. 6 0 [node-6-0 node-6-1])
      layer-7 (fo/FlatLayer. 7 0 [node-7-0 node-7-1])
      layer-8 (fo/FlatLayer. 8 0 [node-8-0])
      seg (Segment. #{node-2-2 node-4-2} 3 #{:i :j} 2)
      graph (fo/map->FlatGraph {:layers [layer-0 layer-1 layer-2 layer-3 layer-4
                                         layer-5 layer-6 layer-7 layer-8]
                                :preds {node-1-0 #{pred-1}
                                        node-1-1 #{pred-2}
                                        node-2-0 #{pred-3}
                                        node-2-1 #{pred-4}
                                        node-2-2 #{pred-5}
                                        node-2-3 #{pred-6}
                                        node-3-1 #{pred-7}
                                        node-4-2 #{pred-8}
                                        node-3-2 #{pred-9}
                                        node-4-0 #{pred-10}
                                        node-5-0 #{pred-11}
                                        node-6-1 #{pred-12}
                                        node-7-1 #{pred-13}
                                        node-8-0 #{pred-14}}
                                :succs {node-0-0 #{succ-1 succ-2}
                                        node-1-0 #{succ-3}
                                        node-1-1 #{succ-4 succ-5 succ-6}
                                        node-2-1 #{succ-7}
                                        node-2-2 #{succ-8}
                                        node-2-3 #{succ-9}
                                        node-3-0 #{succ-10}
                                        node-4-1 #{succ-11}
                                        node-5-0 #{succ-12}
                                        node-6-0 #{succ-13}
                                        node-7-0 #{succ-14}}
                                :aboves {node-1-1 node-1-0
                                         node-2-1 node-2-0
                                         node-2-2 node-2-1
                                         node-2-3 node-2-2
                                         node-3-1 node-3-0
                                         seg node-3-1
                                         node-3-2 seg
                                         node-4-1 node-4-0
                                         node-4-2 node-4-1
                                         node-6-1 node-6-0
                                         node-7-1 node-7-0}
                                :belows {}
                                :top-idxs {node-0-0 0
                                           node-1-0 0
                                           node-1-1 1
                                           node-2-0 0
                                           node-2-1 1
                                           node-2-2 2
                                           node-2-3 3
                                           node-3-0 0
                                           node-3-1 1
                                           seg 2
                                           node-3-2 3
                                           node-4-0 0
                                           node-4-1 1
                                           node-4-2 2
                                           node-5-0 0
                                           node-6-0 0
                                           node-6-1 1
                                           node-7-0 0
                                           node-7-1 1
                                           node-8-0 0}
                                :bot-idxs {}
                                :characters {node-0-0 [:a :b :c :d :e :f
                                                       :g :h :i :j :k]
                                             node-1-0 [:a :b :c :d :e]
                                             node-1-1 [:f :g :h :i :j :k]
                                             node-2-0 [:a :b :c :d]
                                             node-2-1 [:f :g :h]
                                             node-2-2 [:i :j]
                                             node-2-3 [:k]
                                             node-3-0 [:l :m :n :o :p :q :r :s]
                                             node-3-1 [:f :g :h]
                                             node-3-2 [:k]
                                             node-4-0 [:l :m :n :o :p :q :r :s]
                                             node-4-1 [:t :u :v :w :x]
                                             node-4-2 [:i :j]
                                             node-5-0 [:t :u :v :w :x]
                                             node-6-0 [:y :z]
                                             node-6-1 [:t :u :v :w :x]
                                             node-7-0 [:1 :2 :3]
                                             node-7-1 [:y :z]
                                             node-8-0 [:1 :2 :3]}})
      block-1 [node-0-0 node-1-0 node-2-0]
      block-2 [node-1-1 node-2-1 node-3-1]
      block-3 [node-2-2 seg node-4-2]
      block-4 [node-2-3 node-3-2]
      block-5 [node-3-0 node-4-0]
      block-6 [node-4-1 node-5-0 node-6-1]
      block-7 [node-6-0 node-7-1]
      block-8 [node-7-0 node-8-0]
      block-succs {block-1 #{(f/BlockEdge. block-1 block-2 5)}
                   block-2 #{(f/BlockEdge. block-2 block-3 3)}
                   block-3 #{(f/BlockEdge. block-3 block-4 2)}
                   block-5 #{(f/BlockEdge. block-5 block-2 8)
                             (f/BlockEdge. block-5 block-6 8)}
                   block-6 #{(f/BlockEdge. block-6 block-3 5)}
                   block-7 #{(f/BlockEdge. block-7 block-6 2)}
                   block-8 #{(f/BlockEdge. block-8 block-7 3)}}
      block-preds {block-2 #{(f/BlockEdge. block-2 block-1 5)
                             (f/BlockEdge. block-2 block-5 8)}
                   block-3 #{(f/BlockEdge. block-3 block-2 3)
                             (f/BlockEdge. block-3 block-6 5)}
                   block-4 #{(f/BlockEdge. block-4 block-3 2)}
                   block-6 #{(f/BlockEdge. block-6 block-5 8)
                             (f/BlockEdge. block-6 block-7 2)}
                   block-7 #{(f/BlockEdge. block-7 block-8 3)}}
      simple-succs {block-1 #{block-2}
                    block-2 #{block-3}
                    block-3 #{block-4}
                    block-5 #{block-2 block-6}
                    block-6 #{block-3}
                    block-7 #{block-6}
                    block-8 #{block-7}}
      class-1 [block-1 block-2 block-3 block-4]
      class-2 [block-5 block-6]
      class-3 [block-8 block-7]
      block-classes {block-1 class-1
                     block-2 class-1
                     block-3 class-1
                     block-4 class-1
                     block-5 class-2
                     block-6 class-2
                     block-7 class-3
                     block-8 class-3}
      classes [class-3 class-2 class-1]
      block-succs {class-2 #{(f/BlockEdge. block-5 block-2 8)
                             (f/BlockEdge. block-6 block-3 5)}
                   class-3 #{(f/BlockEdge. block-7 block-6 2)}}
      class-graph (f/map->ClassGraph {:classes classes
                                      :succs block-succs
                                      :preds {class-1 #{(f/BlockEdge. block-2 block-5 8)
                                                        (f/BlockEdge. block-3 block-6 5)}
                                              class-2 #{(f/BlockEdge. block-6 block-7 2)}}
                                      :block-succs block-succs
                                      :block-preds block-preds
                                      :sources #{class-3}
                                      :sinks #{class-1}})]
  (deftest test-classify-start
    (is (= (f/classify-start block-1 simple-succs) (into #{} class-1))
        "Descendents of a source block are correctly categorized"))
  (deftest test-classify
    (is (= (f/classify [block-1 block-2 block-3 block-4
                        block-5 block-6 block-8 block-7]
                       simple-succs
                       [block-1 block-5 block-8])
           block-classes)
        "Classes are calculated from BlockGraphs"))
  (deftest test-class-topo-sort
    (is (= (f/topo-sort [class-3]
                        {class-3 #{class-2} class-2 #{class-1}})
           [class-3 class-2 class-1])))
  (deftest test-block-topo-sort
    (is (= (f/topo-sort [block-1 block-5 block-8] {block-1 #{block-2}
                                                   block-2 #{block-3}
                                                   block-3 #{block-4}
                                                   block-5 #{block-2 block-6}
                                                   block-6 #{block-3}
                                                   block-7 #{block-6}
                                                   block-8 #{block-7}})
           [block-1 block-5 block-2 block-8
            block-7 block-6 block-3 block-4])))
  (deftest test-get-block-rel-ys
    (is (= (f/get-block-rel-ys class-1 block-preds 50 15 :up)
           {block-1 0
            block-2 110
            block-3 190
            block-4 255})
        "Relative y-positions within a class are calculated"))
  #_(deftest test-ClassGraph->YPlottedClassGraph
    (is (= (f/ClassGraph->YPlottedClassGraph class-graph 50 15)
           (f/YPlottedClassGraph. classes
                                  {node-0-0 75
                                   node-1-0 75
                                   node-1-1 185
                                   node-2-0 75
                                   node-2-1 185
                                   node-2-2 265
                                   node-2-3 330
                                   node-3-0 -10
                                   node-3-1 185
                                   node-3-2 330
                                   node-4-0 -10
                                   node-4-1 145
                                   node-4-2 265
                                   node-5-0 145
                                   node-6-0 80
                                   node-6-1 145
                                   node-7-0 0
                                   node-7-1 80
                                   node-8-0 0}
                                  15))
        "ClassGraph y-positions are correctly calculated"))
  #_(deftest test-FlatGraph->node-ys
    (is (= (f/FlatGraph->node-ys graph 50 15)
           {node-0-0 0
            node-1-0 0
            node-1-1 55
            node-2-0 0
            node-2-1 47.5
            node-2-2 87.5
            node-2-3 120
            node-3-0 0
            node-3-1 77.5
            node-3-2 170
            node-4-0 0
            node-4-1 77.5
            node-4-2 132.5
            node-5-0 0
            node-6-0 0
            node-6-1 32.5
            node-7-0 0
            node-7-1 40
            node-8-0 0})
        "Final y-positions are correctly calculated"))
  (deftest test-plot
    (is (= (update-in (f/plot graph 8 15 50 60 10) [:plots] set)
           {:min-x 0
            :max-x 429.68386622447827
            :min-y -160
            :max-y 160
            :plots #{{:character :a,
                      :plots [{:type :m, :x 0, :y -40}
                              {:type :a, :radius 15, :sweep 0, :x 11.860320721904968, :y -45.816711244137444,
                               :arc-x 0, :arc-y -55}
                              {:type :l, :x 29.55691265062744, :y -68.67205877149604}
                              {:type :a, :radius 55, :sweep 1, :x 73.04475529761231, :y -90,
                               :arc-x 73.04475529761231, :arc-y -35}
                              {:type :h, :x 129.68386622447827}]}
                     {:character :b,
                      :plots [{:type :m, :x 0, :y -30}
                              {:type :a, :radius 25, :sweep 0, :x 19.767201203174942, :y -39.694518740229064,
                               :arc-x 0, :arc-y -55}
                              {:type :l, :x 37.463793131897425, :y -62.549866267587674}
                              {:type :a, :radius 45, :sweep 1, :x 73.04475529761231, :y -80,
                               :arc-x 73.04475529761231, :arc-y -35}
                              {:type :h, :x 129.68386622447827}]}
                     {:character :c,
                      :plots [{:type :m, :x 0, :y -20}
                              {:type :a, :radius 35, :sweep 0, :x 27.674081684444918, :y -33.572326236320706,
                               :arc-x 0, :arc-y -55}
                              {:type :l, :x 45.3706736131674, :y -56.427673763679294}
                              {:type :a, :radius 35, :sweep 1, :x 73.04475529761231, :y -70,
                               :arc-x 73.04475529761231, :arc-y -35}
                              {:type :h, :x 129.68386622447827}]}
                     {:character :d,
                      :plots [{:type :m, :x 0, :y -10}
                              {:type :a, :radius 45, :sweep 0, :x 35.580962165714915, :y -27.450133732412343,
                               :arc-x 0, :arc-y -55}
                              {:type :l, :x 53.27755409443736, :y -50.305481259770914}
                              {:type :a, :radius 25, :sweep 1, :x 73.04475529761231, :y -60,
                               :arc-x 73.04475529761231, :arc-y -35}
                              {:type :h, :x 129.68386622447827}]}
                     {:character :e,
                      :plots [{:type :m, :x 0, :y 0}
                              {:type :a, :radius 55, :sweep 0, :x 43.48784264698487, :y -21.327941228503956,
                               :arc-x 0, :arc-y -55}
                              {:type :l, :x 61.18443457570734, :y -44.183288755862556}
                              {:type :a, :radius 15, :sweep 1, :x 73.04475529761231, :y -50,
                               :arc-x 73.04475529761231, :arc-y -35}]}
                     {:character :f,
                      :plots [{:type :m, :x 0, :y 10} {:type :h, :x 179.68386622447827}]}
                     {:character :g,
                      :plots [{:type :m, :x 0, :y 20} {:type :h, :x 179.68386622447827}]}
                     {:character :h,
                      :plots [{:type :m, :x 0, :y 30} {:type :h, :x 179.68386622447827}]}
                     {:character :i,
                      :plots [{:type :m, :x 0, :y 40}
                              {:type :h, :x 73.04475529761231}
                              {:type :a, :radius 35, :sweep 1, :x 103.94213294641845, :y 58.557614089583765,
                               :arc-x 73.04475529761231, :arc-y 75}
                              {:type :l, :x 116.4421329464185, :y 82.04673681874982}
                              {:type :a, :radius 15, :sweep 0, :x 129.68386622447827, :y 90,
                               :arc-x 129.68386622447827, :arc-y 75}
                              {:type :h, :x 229.68386622447827}]}
                     {:character :j,
                      :plots [{:type :m, :x 0, :y 50}
                              {:type :h, :x 73.04475529761231}
                              {:type :a, :radius 25, :sweep 1, :x 95.11431076104527, :y 63.25543863541697,
                               :arc-x 73.04475529761231, :arc-y 75}
                              {:type :l, :x 107.6143107610453, :y 86.74456136458302}
                              {:type :a, :radius 25, :sweep 0, :x 129.68386622447827, :y 100,
                               :arc-x 129.68386622447827, :arc-y 75}
                              {:type :h, :x 229.68386622447827}]}
                     {:character :k,
                      :plots [{:type :m, :x 0, :y 60}
                              {:type :h, :x 73.04475529761231}
                              {:type :a, :radius 15, :sweep 1, :x 87.92892344831732, :y 73.13947898116186,
                               :arc-x 73.04475529761231, :arc-y 75}
                              {:type :l, :x 94.9541405394999, :y 129.34121571062232}
                              {:type :a, :radius 35, :sweep 0, :x 129.68386622447827, :y 160,
                               :arc-x 129.68386622447827, :arc-y 125}
                              {:type :h, :x 179.68386622447827}]}
                     {:character :l,
                      :plots [{:type :m, :x 179.68386622447827, :y -140} {:type :h, :x 229.68386622447827}]}
                     {:character :m,
                      :plots [{:type :m, :x 179.68386622447827, :y -130} {:type :h, :x 229.68386622447827}]}
                     {:character :n,
                      :plots [{:type :m, :x 179.68386622447827, :y -120} {:type :h, :x 229.68386622447827}]}
                     {:character :o,
                      :plots [{:type :m, :x 179.68386622447827, :y -110} {:type :h, :x 229.68386622447827}]}
                     {:character :p,
                      :plots [{:type :m, :x 179.68386622447827, :y -100} {:type :h, :x 229.68386622447827}]}
                     {:character :q,
                      :plots [{:type :m, :x 179.68386622447827, :y -90} {:type :h, :x 229.68386622447827}]}
                     {:character :r,
                      :plots [{:type :m, :x 179.68386622447827, :y -80} {:type :h, :x 229.68386622447827}]}
                     {:character :s,
                      :plots [{:type :m, :x 179.68386622447827, :y -70} {:type :h, :x 229.68386622447827}]}
                     {:character :t,
                      :plots [{:type :m, :x 229.68386622447827, :y -10} {:type :h, :x 329.68386622447827}]}
                     {:character :u,
                      :plots [{:type :m, :x 229.68386622447827, :y 0} {:type :h, :x 329.68386622447827}]}
                     {:character :v,
                      :plots [{:type :m, :x 229.68386622447827, :y 10} {:type :h, :x 329.68386622447827}]}
                     {:character :w,
                      :plots [{:type :m, :x 229.68386622447827, :y 20} {:type :h, :x 329.68386622447827}]}
                     {:character :x,
                      :plots [{:type :m, :x 229.68386622447827, :y 30} {:type :h, :x 329.68386622447827}]}
                     {:character :y,
                      :plots [{:type :m, :x 329.68386622447827, :y -80}
                              {:type :h, :x 379.68386622447827}]}
                     {:character :z,
                      :plots [{:type :m, :x 329.68386622447827, :y -70} {:type :h, :x 379.68386622447827}]}
                     {:character :1,
                      :plots [{:type :m, :x 379.68386622447827, :y -160} {:type :h, :x 429.68386622447827}]}
                     {:character :2,
                      :plots [{:type :m, :x 379.68386622447827, :y -150} {:type :h, :x 429.68386622447827}]}
                     {:character :3,
                      :plots [{:type :m, :x 379.68386622447827, :y -140} {:type :h, :x 429.68386622447827}]}}}))))


(deftest test-arc-distance
  (is (= (f/arc-distance 5 (/ 3 4))) 4)
  (is (= (f/arc-distance 5 (/ 4 3))) 3)
  (is (= (f/arc-x-distance 5 (/ 3 4)) 4))
  (is (= (f/arc-x-distance 5 (/ 4 3)) 3))
  (is (= (f/arc-y-distance 5 (/ 3 4)) 3))
  (is (= (f/arc-y-distance 5 (/ 4 3)) 4)))


(deftest test-layer-x-distance
  (is (= (f/layer-x-distance (/ 4 3) 10 110) 95))
  (is (= (f/layer-x-distance (/ 3 4) 10 100) 150)))

(deftest test-arc-radius
  (is (= (f/arc-radius {:order 5} 15 20) 115)))

(deftest test-arc-y-info
  (is (= (f/arc-y-info {:order 5 :node {:weight 21} :node-y [56 456]}
                       {:order 2 :node {:weight 4} :node-y [62 122]}
                       :up 15 20)
         {:src-arc-radius 115
          :dest-arc-radius 55
          :src-arc-y -115
          :dest-arc-y 55}))
  (is (= (f/arc-y-info {:order 2 :node {:weight 4} :node-y [62 122] :y 102}
                       {:order 5 :node {:weight 21} :node-y [56 456] :y 156}
                       :down 15 20)
         {:src-arc-radius 55
          :dest-arc-radius 115
          :src-arc-y 157
          :dest-arc-y 41})))

(deftest test-add-y-info
  (is (= (f/add-y-info [{:src {:order 5 :node {:weight 21} :node-y [56 456] :y 156}
                         :dest {:order 2 :node {:weight 4} :node-y [62 122] :y 102}
                         :dir :up}
                        {:src {:order 2 :node {:weight 4} :node-y [62 122] :y 102}
                         :dest {:order 5 :node {:weight 21} :node-y [56 456] :y 156}
                         :dir :down}]
                       15 20)
         [{:src {:order 5 :node {:weight 21} :node-y [56 456] :y 156
                 :arc-y 41 :arc-radius 115}
           :dest {:order 2 :node {:weight 4} :node-y [62 122] :y 102
                  :arc-y 157 :arc-radius 55}
           :dir :up
           :total-arc-radius 170}
          {:src {:order 2 :node {:weight 4} :node-y [62 122] :y 102
                 :arc-y 157 :arc-radius 55}
           :dest {:order 5 :node {:weight 21} :node-y [56 456] :y 156
                  :arc-y 41 :arc-radius 115}
           :dir :down
           :total-arc-radius 170}])))

(deftest test-add-x-info
  (is (= (f/add-x-info [{:src {:node {:layer-id 0}}
                         :dest {:node {:layer-id 1}}}
                        {:src {:node {:layer-id 1}}
                         :dest {:node {:layer-id 2}}}]
                       [[15 27] [78 92] [156 200]])
         [{:src {:node {:layer-id 0} :arc-x 27 :xs [15 27]}
           :dest {:node {:layer-id 1} :arc-x 78 :xs [78 92]}}
          {:src {:node {:layer-id 1} :arc-x 92 :xs [78 92]}
           :dest {:node {:layer-id 2} :arc-x 156 :xs [156 200]}}])))

(deftest test-pair-up
  (is (= (f/pair-up [{:y 15 :character :a} {:y 15 :character :a}
                     {:y 25 :character :a} {:y 5 :character :a}])
         [{:dir :level, :src {:y 15 :character :a}, :dest {:y 15 :character :a}, :character :a}
          {:dir :down, :src {:y 15 :character :a}, :dest {:y 25 :character :a}, :character :a}
          {:dir :up, :src {:y 25 :character :a}, :dest {:y 5 :character :a}, :character :a}])))

(let [node-1 (Node. :0-0 0 #{:a :b :c} 3)
      node-2 (Node. :0-1 0 #{:d :e} 2)
      node-3 (Node. :0-2 0 #{:f} 1)
      node-4 (Node. :1-0 1 #{:a :b} 2)
      node-5 (Node. :1-1 1 #{:c :d :e} 3)
      node-6 (Node. :1-2 1 #{:f} 1)
      node-7 (Node. :2-0 2 #{:a :c} 2)
      node-8 (Node. :2-1 2 #{:b :e} 2)
      node-9 (Node. :2-2 2 #{:d :f} 2)
      path-info {:a [{:dir :down,
                      :src {:node node-1
                            :node-y [100 140], :y 100, :order 0, :arc-y 155, :arc-radius 55
                            :xs [0 15] :arc-x 15}
                      :dest {:node node-4
                             :node-y [100 120], :y 120, :order 1, :arc-y 85, :arc-radius 35
                             :xs [275 302] :arc-x 275},
                      :total-arc-radius 90}
                     {:dir :level,
                      :src {:node node-4
                            :node-y [100 120], :y 120, :order 1 :xs [275 302]}
                      :dest {:node node-7
                             :node-y [100 120], :y 120, :order 1 :xs [602 680]}}],
                 :b [{:dir :up,
                      :src {:node node-1
                            :node-y [100 140], :y 120, :order 1, :arc-y 85, :arc-radius 35
                            :xs [0 15] :arc-x 15}
                      :dest {:node node-4
                             :node-y [100 120], :y 100, :order 0, :arc-y 135, :arc-radius 35
                             :xs [275 302] :arc-x 275},
                      :total-arc-radius 70}
                     {:dir :down,
                      :src {:node node-4
                            :node-y [100 120], :y 100, :order 0, :arc-y 135, :arc-radius 35
                            :xs [275 302] :arc-x 302}
                      :dest {:node node-8
                             :node-y [400 420], :y 400, :order 0, :arc-y 385, :arc-radius 15
                             :xs [602 680] :arc-x 602},
                      :total-arc-radius 50}],
                 :c [{:dir :down,
                      :src {:node node-1
                            :node-y [100 140], :y 140, :order 2, :arc-y 155, :arc-radius 15
                            :xs [0 15] :arc-x 15}
                      :dest {:node node-5
                             :node-y [400 440], :y 440, :order 2, :arc-y 385, :arc-radius 55
                             :xs [275 302] :arc-x 275},
                      :total-arc-radius 70}
                     {:dir :up,
                      :src {:node node-5
                            :node-y [400 440], :y 440, :order 2, :arc-y 385, :arc-radius 55
                            :xs [275 302] :arc-x 302}
                      :dest {:node node-7
                             :node-y [100 120], :y 100, :order 0, :arc-y 135, :arc-radius 35
                             :xs [602 680] :arc-x 602},
                      :total-arc-radius 90}],
                 :e [{:dir :down,
                      :src {:node node-2
                            :node-y [400 420], :y 400, :order 0, :arc-y 435, :arc-radius 35
                            :xs [0 15] :arc-x 15}
                      :dest {:node node-5
                             :node-y [400 440], :y 420, :order 1, :arc-y 385, :arc-radius 35
                             :xs [275 302] :arc-x 275},
                      :total-arc-radius 70}
                     {:dir :level,
                      :src {:node node-5
                            :node-y [400 440], :y 420, :order 1 :xs [275 302]}
                      :dest {:node node-8
                             :node-y [400 420], :y 420, :order 1 :xs [602 680]}}],
                 :d [{:dir :up,
                      :src {:node node-2
                            :node-y [400 420], :y 420, :order 1, :arc-y 385, :arc-radius 35
                            :xs [0 15] :arc-x 15}
                      :dest {:node node-5
                             :node-y [400 440], :y 400, :order 0, :arc-y 455, :arc-radius 55
                             :xs [275 302] :arc-x 275},
                      :total-arc-radius 90}
                     {:dir :down,
                      :src {:node node-5
                            :node-y [400 440], :y 400, :order 0, :arc-y 455, :arc-radius 55
                            :xs [275 302] :arc-x 302}
                      :dest {:node node-9
                             :node-y [700 720], :y 720, :order 1, :arc-y 685, :arc-radius 35
                             :xs [602 680] :arc-x 602},
                      :total-arc-radius 90}],
                 :f [{:dir :level,
                      :src {:node node-3
                            :node-y [700 700], :y 700, :order 0 :xs [0 15]}
                      :dest {:node node-6
                             :node-y [700 700], :y 700, :order 0 :xs [275 302]}}
                     {:dir :level,
                      :src {:node node-6
                            :node-y [700 700], :y 700, :order 0 :xs [275 302]}
                      :dest {:node node-9
                             :node-y [700 720], :y 700, :order 0 :xs [602 680]}}]}]

  (deftest test-relative-layer-xs
    (is (= (f/relative-layer-xs path-info [{:id 0} {:id 1} {:id 2}] (/ 4 3) 50)
           {{:id 1} 260, {:id 2} 300})))

  (deftest test-absolute-layer-xs
    (is (= (f/absolute-layer-xs path-info [{:id 0 :duration 15}
                                           {:id 1 :duration 27}
                                           {:id 2 :duration 78}] (/ 4 3) 50)
           [[0 15]
            [275 302]
            [602 680]])))

  (deftest test-arcs-and-tangent
    (let [pair-map (-> (get (:a path-info) 0)
                       (assoc-in [:src :arc-x] 15)
                       (assoc-in [:dest :arc-x] 275))]
      (is (= (f/arcs-and-tangent pair-map)
             [{:type :a, :radius 55, :sweep 1, :x 19.27557971566049, :y 100.1664389438819, :arc-x 15, :arc-y 155}
              {:type :l, :x 272.2791765445797, :y 119.89408430843879}
              {:type :a, :radius 35, :sweep 0, :x 275, :y 120, :arc-x 275, :arc-y 85}]))))

  (deftest test-char-plots
    (let [up-info (-> (get (:b path-info) 0))
          up-plot [{:type :a, :radius 35, :sweep 0, :x 17.712552257943955, :y 119.89472825869149, :arc-x 15, :arc-y 85}
                   {:type :l, :x 272.28744774205603, :y 100.10527174130851}
                   {:type :a, :radius 35, :sweep 1, :x 275, :y 100, :arc-x 275, :arc-y 135}
                   {:type :h, :x 302}]
          down-info (-> (get (:a path-info) 0))
          down-plot [{:type :a, :radius 55, :sweep 1, :x 19.27557971566049, :y 100.1664389438819, :arc-x 15, :arc-y 155}
                     {:type :l, :x 272.2791765445797, :y 119.89408430843879}
                     {:type :a, :radius 35, :sweep 0, :x 275, :y 120, :arc-x 275, :arc-y 85}
                     {:type :h, :x 302}]
          level-info (-> (get (:a path-info) 1))
          level-plot [{:type :h, :x 680}]
          h [{:type :h, :x 78}]
          ala [{:type :a, :radius 55, :sweep 1, :x 46.22786856020158 :y 200.2749403664629}
               {:type :l, :x 255.12772000714446 :y 113.81132568774913}
               {:type :a, :radius 35, :sweep 0, :x 255.12772000714446 :y 113.81132568774913}]]
      (is (= (f/char-plots [] up-info) (into [{:type :m, :x 0, :y 120}
                                              {:type :h, :x 15}]
                                             up-plot)))
      (is (= (f/char-plots [] down-info) (into [{:type :m, :x 0, :y 100}
                                                {:type :h, :x 15}]
                                               down-plot)))
      (is (= (f/char-plots [] level-info) (into [{:type :m, :x 275, :y 120}]
                                                level-plot)))
      (is (= (f/char-plots h up-info) (into h up-plot)))
      (is (= (f/char-plots h down-info) (into h down-plot)))
      (is (= (f/char-plots h level-info) level-plot))
      (is (= (f/char-plots ala up-info) (into ala up-plot)))
      (is (= (f/char-plots ala down-info) (into ala down-plot)))
      (is (= (f/char-plots ala level-info) (into ala level-plot))))))

(deftest test-extend-h
  (is (= (f/extend-h [{:type :h, :x 14}] 27) [{:type :h, :x 27}])))

(deftest test-h-to
  (is (= (f/h-to 78) {:type :h, :x 78})))

(deftest plot-duration
  (is (= (f/plot-duration [{:type :m :x 67 :y 178}] 67 90)
         [{:type :m :x 67 :y 178} {:type :h :x 90}])))

(deftest move-to
  (is (= (f/move-to {:y 38 :xs [47 80]}) [{:type :m :x 47 :y 38} {:type :h :x 80}])))
