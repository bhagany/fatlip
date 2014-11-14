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
    (is (= (count (:marked ordered-graph)) 2) "Number of marked edges")
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
        p-edge (f/Edge. p (f/Node. :1-0 0 [:a] 1) [:a] 1)
        not-p (f/Node. :0-1 0 [:b] 1)
        seg-c-edge (f/Edge. not-p (f/Node. :3-99 3 [:c] 1) [:c] 1)
        seg-c [seg-c-edge]
        items [p seg-c not-p]
        ps #{p}
        succs {p #{p-edge}, not-p #{(f/Edge. not-p (f/Node. :1-0 0 [:b] 1) [:b] 1)}}]
    (is (= (f/replace-ps items ps succs)
           [[p-edge seg-c-edge] not-p])
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
        minus-ps [seg-1 node-1 node-2 seg-2 node-3 seg-3 node-4]]
    (is (= (f/set-positions minus-ps) {seg-1 0, node-1 2, node-2 3
                                       seg-2 4, node-3 7, seg-3 8
                                       node-4 9})
        "Positions are set correctly")))


(deftest test-get-measure
  (let [node (f/Node. :1-0 1 [:a :b :c] 3)
        pr-node-1 (f/Node. :0-0 0 [:a :b] 2)
        pr-node-2 (f/Node. :0-1 0 [:c] 1)
        edge-1 (f/Edge. node pr-node-1 [:a :b] 2)
        edge-2 (f/Edge. node pr-node-2 [:c] 1)
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
        edge-1 (f/Edge. nl-node-1 l-node-1 [:c] 1)
        edge-2 (f/Edge. nl-node-2 l-node-1 [:b] 1)
        edge-3 (f/Edge. nl-node-2 l-node-3 [:e] 1)
        edge-4 (f/Edge. nl-node-3 l-node-1 [:a] 1)
        edge-5 (f/Edge. nl-node-4 l-node-2 [:d] 1)
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
        l-seg-1 [(f/Edge. "meh" "somewhere else" [:x] 1)]
        l-seg-2 [(f/Edge. "bleh" "elsewhere" [:y] 1)
                 (f/Edge. "geh" nl-node-5 [:f :g] 2)]
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
        edge-1 (f/Edge. "meh"  "somewhere else" [:x] 1)
        edge-2 (f/Edge. "bleh" "elsewhere" [:y] 1)
        edge-3 (f/Edge. "feh" node-2 [:f :g] 2)
        edge-4 (f/Edge. "doesn't" "matter" [:m] 1)
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
        edge (f/Edge. (f/Node. :0-0 0 [:b] 1)
                      (f/Node. :1-3 1 [:b] 1)
                      [:b] 1)
        seg [(f/Edge. "mmm" "hmm" [:b :h] 2)]
        sedge (f/Edge. seg seg [:b :h] 2)
        crossed-edge (f/Edge. (f/Node. :0-1 1 [:f] 1)
                              (f/Node. :1-2 1 [:f] 1)
                              [:f] 1)
        edge-1 (f/Edge. (f/Node. :0-2 0 [:a :d :e] 3)
                        (f/Node. :1-0 1 [:a :d :e] 3)
                        [:a :d :e] 3)
        edge-2 (f/Edge. (f/Node. :0-3 0 [:c :g] 2)
                        (f/Node. :1-1 1 [:c :g] 2)
                        [:c :g] 2)
        tree [(f/AccumulatorNode. 2
                                  #{edge-1 (f/rev edge-1)
                                    edge-2 (f/rev edge-2)}
                                  false)
              (f/AccumulatorNode. 1 #{} true)
              (f/AccumulatorNode. 1
                                  #{crossed-edge (f/rev crossed-edge)}
                                  false)]]
    (is (= (f/single-edge-super-crossings tree 3 edge)
           [tree 3 #{edge (f/rev edge)}])
        "Correctly sum up and mark an edge that crosses a segment")
    (is (= (f/single-edge-super-crossings tree 5 edge)
           [(-> tree
                (update-in [0 :node-edges] conj edge (f/rev edge))
                (update-in [0 :weight] inc))
            1 #{}])
        "Correctly sum up and don't mark and edge that crosses a segment")
    (is (= (f/single-edge-super-crossings tree 5 sedge)
           [(-> tree
                (assoc-in [0 :is-seg-c] true)
                (update-in [0 :weight] + 2))
            2
            #{crossed-edge (f/rev crossed-edge)}])
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
        l-seg-1 [(f/Edge. "bleh" "somewhere else" [:x] 1)]
        l-seg-2 [(f/Edge. "meh" "elsewhere" [:y] 1)
                 (f/Edge. "geh" nl-node-5 [:f :g] 2)]
        edge-1 (f/Edge. nl-node-1 l-node-1 [:c] 1)
        edge-2 (f/Edge. nl-node-2 l-node-1 [:b] 1)
        edge-3 (f/Edge. nl-node-2 l-node-3 [:e] 1)
        edge-4 (f/Edge. nl-node-3 l-node-1  [:a] 1)
        edge-5 (f/Edge. nl-node-4 l-node-2 [:d] 1)
        minus-ps [l-node-1 l-seg-1 l-node-2 l-seg-2 l-node-3]
        minus-qs [nl-node-1 nl-node-3 l-seg-1 nl-node-4 nl-node-2 l-seg-2]
        preds {nl-node-1 #{edge-1}
               nl-node-2 #{edge-2 edge-3}
               nl-node-3 #{edge-4}
               nl-node-4 #{edge-5}}]
    (is (= (f/count-and-mark-super-crossings minus-ps minus-qs preds {})
           [5 #{edge-2 (f/rev edge-2) edge-3 (f/rev edge-3)}])
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
        succs {node #{(f/Edge. node s-node [:a] 1)}}
        measures {s-node 1}]
    (is (= (f/count-sub-crossings-single-node node succs measures) 0)
        "A node with 1 successor has 0 sub-crossings"))
  (let [node (f/Node. :0-0 0 [:a :b] 2)
        succ-1 (f/Node. :1-0 1 [:a] 1)
        succ-2 (f/Node. :1-1 1 [:b] 1)
        succs {node (set (map #(f/Edge. node % (:characters %) 1)
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
        succs {l-node-1 (set (map #(f/Edge. l-node-1 % (:characters %) (:weight %))
                                  [nl-node-1 nl-node-2 nl-node-3]))
               l-node-2 (set (map #(f/Edge. l-node-2 % (:characters %) (:weight %))
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


(deftest test-OrderedLayer->FlatLayer
  (let [node-1 (f/Node. :0-0 0 [:a] 1)
        node-2 (f/Node. :0-1 0 [:b] 1)
        edge-1 (f/Edge. node-1 "meh" [:c] 1)
        edge-2 (f/Edge. node-2 "bleh" [:d] 1)
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
        succ (f/Edge. node-0-1 node-2-1 [:b] 1)
        pred (f/Edge. node-2-1 node-0-1 [:b] 1)
        marked-1 (f/Edge. node-0-0 node-1-0 [:a] 1)
        marked-2 (f/Edge. node-1-0 node-0-0 [:a] 1)
        marked #{marked-1 marked-2}
        succs {node-0-0 #{marked-1}
               node-0-1 #{succ}
               node-0-2 #{(f/Edge. node-0-2 node-1-0 [:c] 1)}
               node-1-0 #{(f/Edge. node-1-0 node-2-0 [:a] 1)
                          (f/Edge. node-1-0 node-2-2 [:c] 1)}}
        preds {node-1-0 #{marked-2
                          (f/Edge. node-1-0 node-0-2 [:c] 1)}
               node-2-0 #{(f/Edge. node-2-0 node-1-0 [:a] 1)}
               node-2-1 #{pred}
               node-2-2 #{(f/Edge. node-2-2 node-1-0 [:c] 1)}}
        layers [(f/OrderedLayer. 0 0 [node-0-0 node-0-1 node-0-2])
                (f/OrderedLayer. 1 0 [node-1-0 [succ]])
                (f/OrderedLayer. 2 0 [node-2-0 node-2-1 node-2-2])]
        ordered-graph (f/OrderedGraph. layers succs preds 0 marked)
        succ-seg (f/Edge->Segment succ 1)
        pred-seg (f/Edge->Segment pred 1)]
    (is (= (f/OrderedGraph->FlatGraph ordered-graph)
           (f/map->FlatGraph {:layers [(f/FlatLayer. 0 0 [node-0-0 node-0-1 node-0-2])
                                       (f/FlatLayer. 1 0 [node-1-0 succ-seg])
                                       (f/FlatLayer. 2 0 [node-2-0 node-2-1 node-2-2])]
                              :succs succs
                              :preds preds
                              :marked marked
                              :aboves {node-0-1 node-0-0
                                       node-0-2 node-0-1
                                       succ-seg node-1-0
                                       node-2-1 node-2-0
                                       node-2-2 node-2-1}
                              :belows {node-0-0 node-0-1
                                       node-0-1 node-0-2
                                       node-1-0 succ-seg
                                       node-2-0 node-2-1
                                       node-2-1 node-2-2}
                              :top-idxs {node-0-0 0
                                         node-0-1 1
                                         node-0-2 2
                                         node-1-0 0
                                         succ-seg 1
                                         node-2-0 0
                                         node-2-1 1
                                         node-2-2 2}
                              :bot-idxs {node-0-0 2
                                         node-0-1 1
                                         node-0-2 0
                                         node-1-0 1
                                         succ-seg 0
                                         node-2-0 2
                                         node-2-1 1
                                         node-2-2 0}}))
        "Ordered graphs are converted to flat graphs")))


(deftest test-check-alignment
  (let [pred-edge (f/Edge. nil nil [:a] 1)
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
  (let [node-1 (f/Node. :5-0 5 [:a] 1)
        node-2 (f/Node. :0-0 0 [:a] 1)
        pred (f/Edge. node-1 node-2 [:a] 1)
        rev-pred (f/Edge. node-2 node-1 [:a] 1)]
    (is (= (f/pred->segs+src pred)
           [(f/Segment. #{node-1 node-2} 1 [:a] 1)
            (f/Segment. #{node-1 node-2} 2 [:a] 1)
            (f/Segment. #{node-1 node-2} 3 [:a] 1)
            (f/Segment. #{node-1 node-2} 4 [:a] 1)
            node-1])
        "Predecessors are processed correctly when layers are increasing")
    (is (= (f/pred->segs+src rev-pred)
           [(f/Segment. #{node-1 node-2} 4 [:a] 1)
            (f/Segment. #{node-1 node-2} 3 [:a] 1)
            (f/Segment. #{node-1 node-2} 2 [:a] 1)
            (f/Segment. #{node-1 node-2} 1 [:a] 1)
            node-2])
        "Predecessors are processed correctly when layers are decreasing")))


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
        layer-1 (f/FlatLayer. 0 0 [node-1 node-2 node-3 node-4 node-5 node-6 node-7 node-8])
        layer-2 (f/FlatLayer. 1 0 [node-9 node-10 node-11])
        graph (f/map->FlatGraph {:layers [layer-1 layer-2]
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


(let [node-0-0 (f/Node. :0-0 0 [:a :b :c :d :e :f
                                :g :h :i :j :k] 11)
      node-1-0 (f/Node. :1-0 1 [:a :b :c :d :e] 5)
      node-1-1 (f/Node. :1-1 1 [:f :g :h :i :j :k] 6)
      node-2-0 (f/Node. :2-0 2 [:a :b :c :d] 4)
      node-2-1 (f/Node. :2-1 2 [:f :g :h] 3)
      node-2-2 (f/Node. :2-2 2 [:i :j] 2)
      node-2-3 (f/Node. :2-3 2 [:k] 1)
      node-3-0 (f/Node. :3-0 3 [:l :m :n :o :p :q :r :s] 8)
      node-3-1 (f/Node. :3-1 3 [:f :g :h] 3)
      node-3-2 (f/Node. :3-2 3 [:k] 1)
      node-4-0 (f/Node. :4-0 4 [:l :m :n :o :p :q :r :s] 8)
      node-4-1 (f/Node. :4-1 4 [:t :u :v :w :x] 5)
      node-4-2 (f/Node. :4-2 4 [:i :j] 2)
      node-5-0 (f/Node. :5-0 5 [:t :u :v :w :x] 5)
      node-6-0 (f/Node. :6-0 6 [:y :z] 2)
      node-6-1 (f/Node. :6-1 6 [:t :u :v :w :x] 5)
      node-7-0 (f/Node. :7-0 7 [:1 :2 :3] 3)
      node-7-1 (f/Node. :7-1 7 [:y :z] 2)
      node-8-0 (f/Node. :8-0 8 [:1 :2 :3] 3)
      pred-1 (f/Edge. node-1-0 node-0-0 [:a :b :c :d :e] 5)
      pred-2 (f/Edge. node-1-1 node-0-0 [:f :g :h :i :j :k] 6)
      pred-3 (f/Edge. node-2-0 node-1-0 [:a :b :c :d] 4)
      pred-4 (f/Edge. node-2-1 node-1-1 [:f :g :h] 3)
      pred-5 (f/Edge. node-2-2 node-1-1 [:i :j] 2)
      pred-6 (f/Edge. node-2-3 node-1-1 [:k] 1)
      pred-7 (f/Edge. node-3-1 node-2-1 [:f :g :h] 3)
      pred-8 (f/Edge. node-4-2 node-2-2 [:i :j] 2)
      pred-9 (f/Edge. node-3-2 node-2-3 [:k] 1)
      pred-10 (f/Edge. node-4-0 node-3-0 [:l :m :n :o :p :q :r :s] 8)
      pred-11 (f/Edge. node-5-0 node-4-1 [:t :u :v :w :x] 5)
      pred-12 (f/Edge. node-6-1 node-5-0 [:t :u :v :w :x] 5)
      pred-13 (f/Edge. node-7-1 node-6-0 [:y :z] 2)
      pred-14 (f/Edge. node-8-0 node-7-0 [:1 :2 :3] 3)
      layer-0 (f/FlatLayer. 0 0 [node-0-0])
      layer-1 (f/FlatLayer. 1 0 [node-1-0 node-1-1])
      layer-2 (f/FlatLayer. 2 0 [node-2-0 node-2-1 node-2-2 node-2-3])
      layer-3 (f/FlatLayer. 3 0 [node-3-0 node-3-1 node-3-2])
      layer-4 (f/FlatLayer. 4 0 [node-4-0 node-4-1 node-4-2])
      layer-5 (f/FlatLayer. 5 0 [node-5-0])
      layer-6 (f/FlatLayer. 6 0 [node-6-0 node-6-1])
      layer-7 (f/FlatLayer. 7 0 [node-7-0 node-7-1])
      layer-8 (f/FlatLayer. 8 0 [node-8-0])
      seg (f/Segment. #{node-2-2 node-4-2} 3 [:i :j] 2)
      graph (f/map->FlatGraph {:layers [layer-0 layer-1 layer-2 layer-3 layer-4
                                        layer-5 layer-6 layer-7 layer-8]
                               :succs {}
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
                               :bot-idxs {}})
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
      block-graph (f/map->BlockGraph {:roots {node-0-0 node-0-0
                                              node-1-0 node-0-0
                                              node-1-1 node-1-1
                                              node-2-0 node-0-0
                                              node-2-1 node-1-1
                                              node-2-2 node-2-2
                                              node-2-3 node-2-3
                                              node-3-0 node-3-0
                                              node-3-1 node-1-1
                                              seg node-2-2
                                              node-3-2 node-2-3
                                              node-4-0 node-3-0
                                              node-4-1 node-4-1
                                              node-4-2 node-2-2
                                              node-5-0 node-4-1
                                              node-6-0 node-6-0
                                              node-6-1 node-4-1
                                              node-7-0 node-7-0
                                              node-7-1 node-6-0
                                              node-8-0 node-7-0}
                                      :blocks {node-0-0 block-1
                                               node-1-1 block-2
                                               node-2-2 block-3
                                               node-2-3 block-4
                                               node-3-0 block-5
                                               node-4-1 block-6
                                               node-6-0 block-7
                                               node-7-0 block-8}
                                      :succs block-succs
                                      :sources [block-1 block-5 block-8]})
      class-roots {block-1 node-0-0
                   block-2 node-0-0
                   block-3 node-0-0
                   block-4 node-0-0
                   block-5 node-3-0
                   block-6 node-3-0
                   block-7 node-7-0
                   block-8 node-7-0}
      class-1 #{block-1 block-2 block-3 block-4}
      class-2 #{block-5 block-6}
      class-3 #{block-7 block-8}
      classes {node-0-0 class-1
               node-3-0 class-2
               node-7-0 class-3}]
  (deftest test-FlatGraph->BlockGraph
    (is (= (f/FlatGraph->BlockGraph graph) block-graph)
        "FlatGraphs translate to BlockGraphs"))
  (deftest test-classify-source
    (is (= (f/classify-source block-1 block-succs) class-1)
        "Descendents of a source block are correctly categorized"))
  (deftest test-classify
    (is (= (f/classify block-graph) [class-roots classes])
        "Classes are calculated from BlockGraphs"))
  (deftest test-BlockGraph->ClassGraph
    (is (= (f/BlockGraph->ClassGraph block-graph)
           (f/map->ClassGraph {:roots class-roots
                               :classes classes
                               :succs {class-1 #{}
                                       class-2 #{(f/BlockEdge. block-5 block-2 8)
                                                 (f/BlockEdge. block-6 block-3 5)}
                                       class-3 #{(f/BlockEdge. block-7 block-6 2)}}
                               :sources #{class-3}}))
        "ClassGraphs are derived from Blockgraphs")))
