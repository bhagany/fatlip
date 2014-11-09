(ns fatlip.core-test
  (:require-macros [cemerick.cljs.test :refer (is deftest are testing)])
  (:require [cemerick.cljs.test :as test]
            [fatlip.core :as f]))


(deftest test-graph-properties
  (let [input [{:groups [{:characters [:a :a]}]}]]
    (is (thrown? js/Error (f/inp->SparseGraph input))
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
        graph (f/inp->SparseGraph input)
        orderings (f/orderings graph)
        ordered-graph (get orderings 0)]
    (is (= (count (:layers graph)) (count (:layers ordered-graph)) 4) "Number of layers")
    (is (= (count (-> graph :layers (get 0) :nodes))
           (count (-> ordered-graph :layers (get 0) :items))
           3)
        "Number of nodes in layer 0")
    (is (= (count (-> graph :layers (get 1) :nodes))
           (count (-> ordered-graph :layers (get 1) :items))
           6)
        "Number of nodes in layer 1")
    (is (= (count (-> graph :layers (get 2) :nodes))
           (count (-> ordered-graph :layers (get 2) :items))
           3)
        "Number of nodes in layer 2")
    (is (= (count (-> graph :layers (get 3) :nodes))
           (count (-> ordered-graph :layers (get 3) :items))
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
        p-edge (f/Edge. (f/Node. :1-0 0 [:a] 1) [:a] 1)
        not-p (f/Node. :0-1 0 [:b] 1)
        seg-c-edge (f/Edge. (f/Node. :3-99 3 [:c] 1) [:c] 1)
        seg-c [seg-c-edge]
        items [p seg-c not-p]
        ps #{p}
        succs {p #{p-edge}, not-p #{(f/Edge. (f/Node. :1-0 0 [:b] 1) [:b] 1)}}]
    (is (= (f/replace-ps items ps succs)
           [[p-edge seg-c-edge] not-p])
        "P nodes get replaced by segment containers, and joined with adjacent segment containers")))


(deftest test-set-positions
  (let [seg-1 [(f/Edge. "dest" [] 0)
               (f/Edge. "dest" [] 0)]
        node-1 (f/Node. :0-0 0 [] 0)
        node-2 (f/Node. :0-1 0 [] 0)
        seg-2 [(f/Edge. "dest" [] 0)
               (f/Edge. "dest" [] 0)
               (f/Edge. "dest" [] 0)]
        node-3 (f/Node. :0-2 0 [] 0)
        seg-3 [(f/Edge. "dest" [] 0)]
        node-4 (f/Node. :0-2 0 [] 0)
        minus-ps [seg-1 node-1 node-2 seg-2 node-3 seg-3 node-4]]
    (is (= (f/set-positions minus-ps) {seg-1 0, node-1 2, node-2 3
                                       seg-2 4, node-3 7, seg-3 8
                                       node-4 9})
        "Positions are set correctly")))


(deftest test-get-measure
  (let [node (f/Node. :1-0 1 [:a :b :c] 3)
        pr-node-1 (f/Node. :0-0 0 [:a :b] 2)
        pr-node-2 (f/Node. :0-1 0 [:c] 1)
        edge-1 (f/Edge. pr-node-1 [:a :b] 2)
        edge-2 (f/Edge. pr-node-2 [:c] 1)
        preds #{edge-1 edge-2}
        pred-positions {pr-node-1 1, pr-node-2 2}]
    (is (= (f/get-measure node preds pred-positions) (/ 4 3))
        "The measure of a node is calculated correctly")))


(deftest test-set-measures
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        l-node-2 (f/Node. :0-1 0 [:d] 1)
        l-node-3 (f/Node. :0-2 0 [:e] 1)
        nl-node-1 (f/Node. :1-0 1 [:c] 1)
        nl-node-2 (f/Node. :1-1 1 [:b :e] 2)
        nl-node-3 (f/Node. :1-2 1 [:a] 1)
        nl-node-4 (f/Node. :1-3 1 [:d] 1)
        edge-1 (f/Edge. l-node-1 [:c] 1)
        edge-2 (f/Edge. l-node-1 [:b] 1)
        edge-3 (f/Edge. l-node-3 [:e] 1)
        edge-4 (f/Edge. l-node-1 [:a] 1)
        edge-5 (f/Edge. l-node-2 [:d] 1)
        non-qs [nl-node-1 nl-node-2 nl-node-3 nl-node-4]
        preds {nl-node-1 #{edge-1}
               nl-node-2 #{edge-2 edge-3}
               nl-node-3 #{edge-4}
               nl-node-4 #{edge-5}}
        positions {l-node-1 0
                   l-node-2 1
                   l-node-3 2}]
    (is (= (f/set-measures non-qs preds positions)
           {nl-node-1 0
            nl-node-2 1
            nl-node-3 0
            nl-node-4 1})
        "Measures are set correctly")))


(deftest test-merge-layer
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        l-node-2 (f/Node. :0-1 0 [:d] 1)
        l-node-3 (f/Node. :0-2 0 [:e] 1)
        nl-node-1 (f/Node. :1-0 1 [:c] 1)
        nl-node-2 (f/Node. :1-1 1 [:b :e] 2)
        nl-node-3 (f/Node. :1-2 1 [:a] 1)
        nl-node-4 (f/Node. :1-3 1 [:d] 1)
        nl-node-5 (f/Node. :1-4 1 [:f :g] 2)
        l-seg-1 [(f/Edge. "somewhere else" [:x] 1)]
        l-seg-2 [(f/Edge. "elsewhere" [:y] 1)
                 (f/Edge. nl-node-5 [:f :g] 2)]
        edge-1 (f/Edge. l-node-1 [:c] 1)
        edge-2 (f/Edge. l-node-1 [:b] 1)
        edge-3 (f/Edge. l-node-3 [:e] 1)
        edge-4 (f/Edge. l-node-1 [:a] 1)
        edge-5 (f/Edge. l-node-2 [:d] 1)
        minus-ps [l-node-1 l-seg-1 l-node-2 l-seg-2 l-node-3]
        positions {l-node-1 0
                   l-seg-1 1
                   l-node-2 2
                   l-seg-2 3
                   l-node-3 5}
        non-qs [nl-node-1 nl-node-2 nl-node-3 nl-node-4]
        measures {nl-node-1 0
                  nl-node-2 2.5
                  nl-node-3 0
                  nl-node-4 2}]
    (is (= (f/merge-layer minus-ps positions non-qs measures)
           [nl-node-1 nl-node-3 l-seg-1 nl-node-4 nl-node-2 l-seg-2])
        "Nodes and segment containers are merged correctly")))


(deftest test-add-qs
  (let [node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        node-2 (f/Node. :0-1 0 [:d] 1)
        node-3 (f/Node. :0-2 0 [:e] 1)
        edge-1 (f/Edge. "somewhere else" [:x] 1)
        edge-2 (f/Edge. "elsewhere" [:y] 1)
        edge-3 (f/Edge. node-2 [:f :g] 2)
        edge-4 (f/Edge. "matter" [:m] 1)
        seg-c-1 [edge-1]
        seg-c-2 [edge-2 edge-3 edge-4]
        minus-qs [node-1 seg-c-1 node-3 seg-c-2]
        qs #{node-2}]
    (is (= (f/add-qs minus-qs qs)
           [node-1 seg-c-1 node-3 [edge-2] node-2 [edge-4]])
        "Q nodes are placed correctly, splitting the segment containers they were in")))


(deftest test-sorted-edge-order
  (let [node-1 (f/Node. :0-0 0 [:a :d :e] 3)
        node-2 (f/Node. :0-1 0 [:c :g] 2)
        nl-node-1 (f/Node. :1-0 1 [:a :c] 2)
        nl-node-2 (f/Node. :1-1 1 [:d :g] 2)
        nl-node-3 (f/Node. :1-1 1 [:e] 1)
        seg [(f/Edge. "hmm" [:b :f] 1)]
        ordered [node-1 seg node-2]
        next-ordered [nl-node-1 seg nl-node-2 nl-node-3]
        edge-1 (f/Edge. nl-node-1 [:a] 1)
        edge-2 (f/Edge. nl-node-2 [:d] 1)
        edge-3 (f/Edge. nl-node-3 [:e] 1)
        edge-4 (f/Edge. seg [:b :f] 2)
        edge-5 (f/Edge. nl-node-1 [:c] 1)
        edge-6 (f/Edge. nl-node-2 [:g] 1)
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
        fedge (f/Edge. (f/Node. :1-3 1 [:b] 1)
                       [:b] 1)
        edge (with-meta fedge {:forward-edge fedge})
        seg [(f/Edge. "hmm" [:b :h] 2)]
        sedge (f/Edge. seg [:b :h] 2)
        crossed-edge (f/Edge. (f/Node. :1-2 1 [:f] 1)
                              [:f] 1)
        tree [(f/AccumulatorNode. 2
                                  #{(f/Edge. (f/Node. :1-0 1 [:a :d :e] 3)
                                             [:a :d :e] 3)
                                    (f/Edge. (f/Node. :1-1 1 [:c :g] 2)
                                             [:c :g] 2)}
                                  false)
              (f/AccumulatorNode. 1 #{} true)
              (f/AccumulatorNode. 1
                                  #{crossed-edge}
                                  false)]]
    (is (= (f/single-edge-super-crossings tree 3 edge)
           [tree 3 #{fedge}])
        "Correctly sum up and mark an edge that crosses a segment")
    (is (= (f/single-edge-super-crossings tree 5 edge)
           [(-> tree
                (update-in [0 :node-edges] conj fedge)
                (update-in [0 :weight] inc))
            1 #{}])
        "Correctly sum up and don't mark and edge that crosses a segment")
    (is (= (f/single-edge-super-crossings tree 5 sedge)
           [(-> tree
                (assoc-in [0 :is-seg-c] true)
                (update-in [0 :weight] + 2))
            2
            #{crossed-edge}])
        "Correctly sum up crossings and markings when adding a segment")))


(deftest test-count-and-mark-super-crossings
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        l-node-2 (f/Node. :0-1 0 [:d] 1)
        l-node-3 (f/Node. :0-2 0 [:e] 1)
        nl-node-1 (f/Node. :1-0 1 [:c] 2)
        nl-node-2 (f/Node. :1-1 1 [:b :e] 2)
        nl-node-3 (f/Node. :1-2 1 [:a] 1)
        nl-node-4 (f/Node. :1-3 1 [:d] 1)
        nl-node-5 (f/Node. :1-4 1 [:f :g] 2)
        l-seg-1 [(f/Edge. "somewhere else" [:x] 1)]
        l-seg-2 [(f/Edge. "elsewhere" [:y] 1)
                 (f/Edge. nl-node-5 [:f :g] 2)]
        fedge-1 (f/Edge. l-node-1 [:c] 1)
        fedge-2 (f/Edge. l-node-1 [:b] 1)
        fedge-3 (f/Edge. l-node-3 [:e] 1)
        fedge-4 (f/Edge. l-node-1 [:a] 1)
        fedge-5 (f/Edge. l-node-2 [:d] 1)
        edge-1 (with-meta fedge-1 {:forward-edge fedge-1})
        edge-2 (with-meta fedge-2 {:forward-edge fedge-2})
        edge-3 (with-meta fedge-3 {:forward-edge fedge-3})
        edge-4 (with-meta fedge-4 {:forward-edge fedge-4})
        edge-5 (with-meta fedge-5 {:forward-edge fedge-5})
        minus-ps [l-node-1 l-seg-1 l-node-2 l-seg-2 l-node-3]
        minus-qs [nl-node-1 nl-node-3 l-seg-1 nl-node-4 nl-node-2 l-seg-2]
        preds {nl-node-1 #{edge-1}
               nl-node-2 #{edge-2 edge-3}
               nl-node-3 #{edge-4}
               nl-node-4 #{edge-5}}]
    (is (= (f/count-and-mark-super-crossings minus-ps minus-qs preds {})
           [5 #{fedge-2 fedge-3}])
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
        succs {}
        measures {}]
    (is (= (f/count-sub-crossings-single-node node succs measures) 0)
        "A node with 0 successors has 0 sub-crossings"))
  (let [node (f/Node. :0-0 0 [:a] 1)
        s-node (f/Node. :1-0 1 [:a] 1)
        succs {node #{(f/Edge. s-node [:a] 1)}}
        measures {s-node 1}]
    (is (= (f/count-sub-crossings-single-node node succs measures) 0)
        "A node with 1 successor has 0 sub-crossings"))
  (let [node (f/Node. :0-0 0 [:a :b] 2)
        succ-1 (f/Node. :1-0 1 [:a] 1)
        succ-2 (f/Node. :1-1 1 [:b] 1)
        succs {node (set (map #(f/Edge. % (:characters %) 1)
                              [succ-1 succ-2]))}
        measures {succ-2 0 succ-1 1}]
    (is (= (f/count-sub-crossings-single-node node succs measures) 1)
        "A node with multiple successors has its sub-crossings counted")))


(deftest test-count-sub-crossings
  (let [l-node-1 (f/Node. :0-0 0 [:a :b :c] 3)
        l-node-2 (f/Node. :0-1 0 [:d] 1)
        l-node-3 (f/Node. :0-2 0 [:e] 1)
        nl-node-1 (f/Node. :1-0 1 [:c] 1)
        nl-node-2 (f/Node. :1-1 1 [:b] 1)
        nl-node-3 (f/Node. :1-2 1 [:a] 1)
        nl-node-4 (f/Node. :1-3 1 [:d] 1)
        minus-ps [l-node-1 l-node-2 l-node-3]
        measures {nl-node-1 0, nl-node-2 1,
                  nl-node-3 2, nl-node-4 3}
        succs {l-node-1 (set (map #(f/Edge. % (:characters %) (:weight %))
                                  [nl-node-1 nl-node-2 nl-node-3]))
               l-node-2 (set (map #(f/Edge. % (:characters %) (:weight %))
                                  [nl-node-4]))}]
    (is (= (f/count-sub-crossings minus-ps succs measures) 3)
        "Sub crossings are counted correctly")))


(deftest test-neighborify
  (let [node-1 (f/Node. :0-0 0 [:a] 1)
        node-2 (f/Node. :0-1 0 [:b] 1)
        node-3 (f/Node. :0-2 0 [:c] 1)
        seg-1 (f/Segment. "bleh" 0 [:d] 1)
        seg-2 (f/Segment. "geh" 0 [:e] 1)
        layer (f/FlatLayer. 0 0 [node-1 seg-1 node-2 seg-2 node-3])]
    (is (= (f/neighborify layer)
           [{node-3 seg-2
             seg-2 node-2
             node-2 seg-1
             seg-1 node-1}
            {node-1 seg-1
             seg-1 node-2
             node-2 seg-2
             seg-2 node-3}])
        "Mapping nodes and edges that are above or below each thing works")))


(deftest test-indexify
  (let [node-1 (f/Node. :0-0 0 [:a] 1)
        node-2 (f/Node. :0-1 0 [:b] 1)
        node-3 (f/Node. :0-2 0 [:c] 1)
        seg-1 (f/Segment. "bleh" 0 [:d] 1)
        seg-2 (f/Segment. "geh" 0 [:e] 1)
        layer (f/FlatLayer. 0 0 [node-1 seg-1 node-2 seg-2 node-3])]
    (is (= (f/indexify layer)
           [{node-1 0
             seg-1 1
             node-2 2
             seg-2 3
             node-3 4}
            {node-3 0
             seg-2 1
             node-2 2
             seg-1 3
             node-1 4}])
        "Mapping nodes and edges to indexes from the top and bottom of the layer")))


(deftest test-ordered->flat-pred
  (let [src (f/Node. :14-3 14 [:a] 1)
        dest (f/Node. :13-3 13 [:a] 1)
        edge (f/Edge. dest [:a] 1)]
    (is (= (f/ordered->flat-pred src edge)
           [[src dest]])
        "Short edges are converted to [src dest] pairs"))
  (let [src (f/Node. :14-3 14 [:a] 1)
        dest (f/Node. :11-2 11 [:a] 1)
        edge (f/Edge. dest [:a] 1)
        seg-1 (f/Edge->Segment edge 13)
        seg-2 (f/Edge->Segment edge 12)]
    (is (= (f/ordered->flat-pred src edge)
           [[seg-2 dest]
            [seg-1 seg-2]
            [src seg-1]])
        "Long edges are converted into a series of [src seg] [seg seg] ... [seg dest] pairs")))


(deftest test-ordered->flat-preds
  (let [src (f/Node. :14-3 14 [:a :b] 1)
        dest-1 (f/Node. :13-3 13 [:a] 1)
        dest-2 (f/Node. :13-2 13 [:b] 1)
        edge-1 (f/Edge. dest-1 [:a] 1)
        edge-2 (f/Edge. dest-2 [:a] 1)]
    (is (= (f/ordered->flat-preds [src #{edge-1 edge-2}])
           {src #{dest-1 dest-2}})
        "Multiple short edges are collapsed into their destinations, mapped to the same source"))
  (let [src (f/Node. :14-3 14 [:a] 1)
        dest (f/Node. :11-2 11 [:a] 1)
        edge (f/Edge. dest [:a] 1)
        seg-1 (f/Edge->Segment edge 13)
        seg-2 (f/Edge->Segment edge 12)]
    (is (= (f/ordered->flat-preds [src #{edge}])
           {src #{seg-1}
            seg-1 #{seg-2}
            seg-2 #{dest}})
        "Long edges are converted into a series of [src seg] [seg seg] ... [seg dest] pairs")))


(deftest test-OrderedLayer->FlatLayer
  (let [node-1 (f/Node. :0-0 0 [:a] 1)
        node-2 (f/Node. :0-1 0 [:b] 1)
        edge-1 (f/Edge. "meh" [:c] 1)
        edge-2 (f/Edge. "bleh" [:d] 1)
        seg-c [edge-1 edge-2]
        ordered-layer (f/OrderedLayer. 0 0 [node-1 seg-c node-2])]
    (is (= (f/OrderedLayer->FlatLayer ordered-layer)
           (f/FlatLayer. 0 0 [node-1
                              (f/Edge->Segment edge-1 0)
                              (f/Edge->Segment edge-2 0)
                              node-2]))
        "Ordered layers get transformed into flat layers")))


(deftest test-OrderedGraph->FlatGraph
  (let [node-0-0 (f/Node. :0-0 0 [:a] 1)
        node-0-1 (f/Node. :0-1 0 [:b] 1)
        node-0-2 (f/Node. :0-2 0 [:c] 1)
        node-1-0 (f/Node. :1-0 1 [:a :c] 2)
        node-2-0 (f/Node. :2-0 2 [:a] 1)
        node-2-1 (f/Node. :2-1 2 [:b] 1)
        node-2-2 (f/Node. :2-2 2 [:c] 1)
        edge (f/Edge. node-0-1 [:b] 1)
        preds {node-1-0 #{(f/Edge. node-0-0 [:a] 1)
                          (f/Edge. node-0-2 [:c] 1)}
               node-2-0 #{(f/Edge. node-1-0 [:a] 1)}
               node-2-1 #{edge}
               node-2-2 #{(f/Edge. node-1-0 [:c] 1)}}
        layers [(f/OrderedLayer. 0 0 [node-0-0 node-0-1 node-0-2])
                (f/OrderedLayer. 1 0 [node-1-0 [edge]])
                (f/OrderedLayer. 2 0 [node-2-0 node-2-1 node-2-2])]
        ordered-graph (f/OrderedGraph. layers {} preds 0 #{})
        seg (f/Edge->Segment edge 1)]
    (is (= (f/OrderedGraph->FlatGraph ordered-graph)
           (f/map->FlatGraph {:layers [(f/FlatLayer. 0 0 [node-0-0 node-0-1 node-0-2])
                                       (f/FlatLayer. 1 0 [node-1-0 seg])
                                       (f/FlatLayer. 2 0 [node-2-0 node-2-1 node-2-2])]
                              :preds {node-1-0 #{node-0-0 node-0-2}
                                      node-2-0 #{node-1-0}
                                      node-2-1 #{seg}
                                      seg #{node-0-1}
                                      node-2-2 #{node-1-0}}
                              :aboves {node-0-1 node-0-0
                                       node-0-2 node-0-1
                                       seg node-1-0
                                       node-2-1 node-2-0
                                       node-2-2 node-2-1}
                              :belows {node-0-0 node-0-1
                                       node-0-1 node-0-2
                                       node-1-0 seg
                                       node-2-0 node-2-1
                                       node-2-1 node-2-2}
                              :top-idxs {node-0-0 0
                                         node-0-1 1
                                         node-0-2 2
                                         node-1-0 0
                                         seg 1
                                         node-2-0 0
                                         node-2-1 1
                                         node-2-2 2}
                              :bot-idxs {node-0-0 2
                                         node-0-1 1
                                         node-0-2 0
                                         node-1-0 1
                                         seg 0
                                         node-2-0 2
                                         node-2-1 1
                                         node-2-2 0}}))
        "Ordered graphs are converted flat graphs")))


(deftest test-check-alignment
  (let [pred-edge (f/Edge. nil [:a] 1)
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


#_(deftest test-blockify
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
