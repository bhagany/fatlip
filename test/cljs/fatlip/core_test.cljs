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
        orderings (-> graph
                      f/SparseGraph->ordered-graphs)
        cm-graph (f/best-ordering orderings)]
    (is (= (count orderings) 2) "Ordering algorithm short circuits")
    (is (= cm-graph (f/OrderedGraph->CountedAndMarkedGraph (orderings 0)))
        "The correct ordered graph is chosen")
    (is (= (count (:layers graph)) (count (:layers cm-graph)) 4) "Number of layers")
    (is (= (-> graph :layers (get 0) :nodes count)
           (-> cm-graph :layers (get 0) :items count)
           3)
        "Number of nodes in layer 0")
    (is (= (-> graph :layers (get 1) :nodes count)
           (-> cm-graph :layers (get 1) :items count)
           6)
        "Number of nodes in layer 1")
    (is (= (-> graph :layers (get 2) :nodes count)
           (-> cm-graph :layers (get 2) :items count)
           3)
        "Number of nodes in layer 2")
    (is (= (-> graph :layers (get 3) :nodes count)
           (-> cm-graph :layers (get 3) :items count)
           1)
        "Number of nodes in layer 3")
    (is (= (count (:ps graph)) 2) "Number of p nodes")
    (is (= (count (:qs graph)) 2) "Number of q nodes")
    (is (= (count (:rs graph)) 2) "Number of r nodes")
    (is (= (reduce + (map count (vals (:succs graph))))
           (reduce + (map count (vals (:preds graph))))
           14)
        "Number of edges")

    ;; ESK ordering
    (is (= (count (:marked cm-graph)) 2) "Number of marked edges")
    (is (= (:crossings cm-graph) 5) "Number of crossings")))


;; Fine-grained ESK
(deftest test-replace-ps
  (let [p (f/Node. :0-0 0 #{:a} 1)
        p-edge (f/Edge. p (f/Node. :1-0 0 #{:a} 1) #{:a} 1)
        not-p (f/Node. :0-1 0 #{:b} 1)
        seg-c-edge (f/Edge. not-p (f/Node. :3-99 3 #{:c} 1) #{:c} 1)
        seg-c [seg-c-edge]
        items [p seg-c not-p]
        ps #{p}
        succs {p #{p-edge}, not-p #{(f/Edge. not-p (f/Node. :1-0 0 #{:b} 1) #{:b} 1)}}]
    (is (= (f/replace-ps items ps succs)
           [[p-edge seg-c-edge] not-p])
        "P nodes get replaced by segment containers, and joined with adjacent segment containers")))


(deftest test-set-positions
  (let [seg-1 [(f/Edge. "src" "dest" #{} 0)
               (f/Edge. "src" "dest" #{} 0)]
        node-1 (f/Node. :0-0 0 #{} 0)
        node-2 (f/Node. :0-1 0 #{} 0)
        seg-2 [(f/Edge. "src" "dest" #{} 0)
               (f/Edge. "src" "dest" #{} 0)
               (f/Edge. "src" "dest" #{} 0)]
        node-3 (f/Node. :0-2 0 #{} 0)
        seg-3 [(f/Edge. "src" "dest" #{} 0)]
        node-4 (f/Node. :0-2 0 #{} 0)
        minus-ps [seg-1 node-1 node-2 seg-2 node-3 seg-3 node-4]]
    (is (= (f/set-positions minus-ps) {seg-1 0, node-1 2, node-2 3
                                       seg-2 4, node-3 7, seg-3 8
                                       node-4 9})
        "Positions are set correctly")))


(deftest test-get-measure
  (let [node (f/Node. :1-0 1 #{:a :b :c} 3)
        pr-node-1 (f/Node. :0-0 0 #{:a :b} 2)
        pr-node-2 (f/Node. :0-1 0 #{:c} 1)
        edge-1 (f/Edge. node pr-node-1 #{:a :b} 2)
        edge-2 (f/Edge. node pr-node-2 #{:c} 1)
        preds #{edge-1 edge-2}
        pred-positions {pr-node-1 1, pr-node-2 2}]
    (is (= (f/get-measure node preds pred-positions) (/ 4 3))
        "The measure of a node is calculated correctly")))


(deftest test-set-measures
  (let [l-node-1 (f/Node. :0-0 0 #{:a :b :c} 3)
        l-node-2 (f/Node. :0-1 0 #{:d} 1)
        l-node-3 (f/Node. :0-2 0 #{:e} 1)
        nl-node-1 (f/Node. :1-0 1 #{:c} 1)
        nl-node-2 (f/Node. :1-1 1 #{:b :e} 2)
        nl-node-3 (f/Node. :1-2 1 #{:a} 1)
        nl-node-4 (f/Node. :1-3 1 #{:d} 1)
        edge-1 (f/Edge. nl-node-1 l-node-1 #{:c} 1)
        edge-2 (f/Edge. nl-node-2 l-node-1 #{:b} 1)
        edge-3 (f/Edge. nl-node-2 l-node-3 #{:e} 1)
        edge-4 (f/Edge. nl-node-3 l-node-1 #{:a} 1)
        edge-5 (f/Edge. nl-node-4 l-node-2 #{:d} 1)
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
  (let [l-node-1 (f/Node. :0-0 0 #{:a :b :c} 3)
        l-node-2 (f/Node. :0-1 0 #{:d} 1)
        l-node-3 (f/Node. :0-2 0 #{:e} 1)
        nl-node-1 (f/Node. :1-0 1 #{:c} 1)
        nl-node-2 (f/Node. :1-1 1 #{:b :e} 2)
        nl-node-3 (f/Node. :1-2 1 #{:a} 1)
        nl-node-4 (f/Node. :1-3 1 #{:d} 1)
        nl-node-5 (f/Node. :1-4 1 #{:f :g} 2)
        l-seg-1 [(f/Edge. "meh" "somewhere else" #{:x} 1)]
        l-seg-2 [(f/Edge. "bleh" "elsewhere" #{:y} 1)
                 (f/Edge. "geh" nl-node-5 #{:f :g} 2)]
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
  (let [node-1 (f/Node. :0-0 0 #{:a :b :c} 3)
        node-2 (f/Node. :0-1 0 #{:d} 1)
        node-3 (f/Node. :0-2 0 #{:e} 1)
        edge-1 (f/Edge. "meh"  "somewhere else" #{:x} 1)
        edge-2 (f/Edge. "bleh" "elsewhere" #{:y} 1)
        edge-3 (f/Edge. "feh" node-2 #{:f :g} 2)
        edge-4 (f/Edge. "doesn't" "matter" #{:m} 1)
        seg-c-1 [edge-1]
        seg-c-2 [edge-2 edge-3 edge-4]
        minus-qs [node-1 seg-c-1 node-3 seg-c-2]
        qs #{node-2}]
    (is (= (f/add-qs minus-qs qs)
           [node-1 seg-c-1 node-3 [edge-2] node-2 [edge-4]])
        "Q nodes are placed correctly, splitting the segment containers they were in")))


(deftest test-sorted-edge-order
  (let [node-1 (f/Node. :0-0 0 #{:a :d :e} 3)
        node-2 (f/Node. :0-1 0 #{:c :g} 2)
        nl-node-1 (f/Node. :1-0 1 #{:a :c} 2)
        nl-node-2 (f/Node. :1-1 1 #{:d :g} 2)
        nl-node-3 (f/Node. :1-1 1 #{:e} 1)
        seg [(f/Edge. "mmm" "hmm" #{:b :f} 2)]
        ordered [node-1 seg node-2]
        next-ordered [nl-node-1 seg nl-node-2 nl-node-3]
        edge-1 (f/Edge. node-1 nl-node-1 #{:a} 1)
        edge-2 (f/Edge. node-1 nl-node-2 #{:d} 1)
        edge-3 (f/Edge. node-1 nl-node-3 #{:e} 1)
        edge-4 (f/Edge. seg seg #{:b :f} 2)
        edge-5 (f/Edge. node-2 nl-node-1 #{:c} 1)
        edge-6 (f/Edge. node-2 nl-node-2 #{:g} 1)
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
        edge (f/Edge. (f/Node. :0-0 0 #{:b} 1)
                      (f/Node. :1-3 1 #{:b} 1)
                      #{:b} 1)
        seg [(f/Edge. "mmm" "hmm" #{:b :h} 2)]
        sedge (f/Edge. seg seg #{:b :h} 2)
        crossed-edge (f/Edge. (f/Node. :0-1 1 #{:f} 1)
                              (f/Node. :1-2 1 #{:f} 1)
                              #{:f} 1)
        edge-1 (f/Edge. (f/Node. :0-2 0 #{:a :d :e} 3)
                        (f/Node. :1-0 1 #{:a :d :e} 3)
                        #{:a :d :e} 3)
        edge-2 (f/Edge. (f/Node. :0-3 0 #{:c :g} 2)
                        (f/Node. :1-1 1 #{:c :g} 2)
                        #{:c :g} 2)
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
  (let [l-node-1 (f/Node. :0-0 0 #{:a :b :c} 3)
        l-node-2 (f/Node. :0-1 0 #{:d} 1)
        l-node-3 (f/Node. :0-2 0 #{:e} 1)
        nl-node-1 (f/Node. :1-0 1 #{:c} 2)
        nl-node-2 (f/Node. :1-1 1 #{:b :e} 2)
        nl-node-3 (f/Node. :1-2 1 #{:a} 1)
        nl-node-4 (f/Node. :1-3 1 #{:d} 1)
        nl-node-5 (f/Node. :1-4 1 #{:f :g} 2)
        l-seg-1 [(f/Edge. "bleh" "somewhere else" #{:x} 1)]
        l-seg-2 [(f/Edge. "meh" "elsewhere" #{:y} 1)
                 (f/Edge. "geh" nl-node-5 #{:f :g} 2)]
        edge-1 (f/Edge. nl-node-1 l-node-1 #{:c} 1)
        edge-2 (f/Edge. nl-node-2 l-node-1 #{:b} 1)
        edge-3 (f/Edge. nl-node-2 l-node-3 #{:e} 1)
        edge-4 (f/Edge. nl-node-3 l-node-1  #{:a} 1)
        edge-5 (f/Edge. nl-node-4 l-node-2 #{:d} 1)
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
  (let [node (f/Node. :0-0 0 #{:a} 1)
        dests #{}]
    (is (= (f/count-sub-crossings-single-node node dests {}) 0)
        "A node with 0 successors has 0 sub-crossings"))
  (let [node (f/Node. :0-0 0 #{:a} 1)
        dest (f/Node. :1-0 1 #{:a} 1)
        dests #{dest}]
    (is (= (f/count-sub-crossings-single-node node dests {dest [:a]}) 0)
        "A node with 1 successor has 0 sub-crossings"))
  (let [node (f/Node. :0-0 0 #{:a :b} 2)
        dest-1 (f/Node. :1-0 1 #{:a} 1)
        dest-2 (f/Node. :1-1 1 #{:b} 1)
        dests [dest-1 dest-2]
        characters {node [:b :a]
                    dest-1 [:a]
                    dest-2 [:b]}]
    (is (= (f/count-sub-crossings-single-node node dests characters) 1)
        "A node with multiple successors has its sub-crossings counted")))


(deftest test-count-sub-crossings
  (let [l-node-1 (f/Node. :0-0 0 #{:a :b :c} 3)
        l-node-2 (f/Node. :0-1 0 #{:d} 1)
        l-node-3 (f/Node. :0-2 0 #{:e} 1)
        nl-node-1 (f/Node. :1-0 1 #{:c} 1)
        nl-node-2 (f/Node. :1-1 1 #{:b} 1)
        nl-node-3 (f/Node. :1-2 1 #{:a} 1)
        nl-node-4 (f/Node. :1-3 1 #{:d} 1)
        minus-ps [l-node-1 l-node-2 l-node-3]
        minus-qs [nl-node-1 nl-node-2 nl-node-3 nl-node-4]
        succs {l-node-1 (set (map #(f/Edge. l-node-1 % (:characters %) (:weight %))
                                  [nl-node-1 nl-node-2 nl-node-3]))
               l-node-2 (set (map #(f/Edge. l-node-2 % (:characters %) (:weight %))
                                  [nl-node-4]))}
        characters {l-node-1 [:a :b :c]
                    l-node-2 [:d]
                    l-node-3 [:e]
                    nl-node-1 [:c]
                    nl-node-2 [:b]
                    nl-node-3 [:a]
                    nl-node-4 [:d]}]
    (is (= (f/count-sub-crossings minus-ps minus-qs succs characters) 3)
        "Sub crossings are counted correctly")))


(deftest test-neighborify
  (let [node-1 (f/Node. :0-0 0 #{:a} 1)
        node-2 (f/Node. :0-1 0 #{:b} 1)
        node-3 (f/Node. :0-2 0 #{:c} 1)
        seg-1 (f/Segment. "bleh" 0 #{:d} 1)
        seg-2 (f/Segment. "geh" 0 #{:e} 1)
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
  (let [node-1 (f/Node. :0-0 0 #{:a} 1)
        node-2 (f/Node. :0-1 0 #{:b} 1)
        node-3 (f/Node. :0-2 0 #{:c} 1)
        seg-1 (f/Segment. "bleh" 0 #{:d} 1)
        seg-2 (f/Segment. "geh" 0 #{:e} 1)
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
  (let [node-1 (f/Node. :0-0 0 #{:a} 1)
        node-2 (f/Node. :0-1 0 #{:b} 1)
        edge-1 (f/Edge. node-1 "meh" #{:c} 1)
        edge-2 (f/Edge. node-2 "bleh" #{:d} 1)
        seg-c [edge-1 edge-2]
        ordered-layer (f/OrderedLayer. 0 0 [node-1 seg-c node-2])]
    (is (= (f/OrderedLayer->FlatLayer ordered-layer)
           (f/FlatLayer. 0 0 [node-1
                              (f/Edge->Segment edge-1 0)
                              (f/Edge->Segment edge-2 0)
                              node-2]))
        "Ordered layers get transformed into flat layers")))


(deftest test-CountedAndMarkedGraph->FlatGraph
  (let [node-0-0 (f/Node. :0-0 0 #{:a} 1)
        node-0-1 (f/Node. :0-1 0 #{:b} 1)
        node-0-2 (f/Node. :0-2 0 #{:c} 1)
        node-1-0 (f/Node. :1-0 1 #{:a :c} 2)
        node-2-0 (f/Node. :2-0 2 #{:a} 1)
        node-2-1 (f/Node. :2-1 2 #{:b} 1)
        node-2-2 (f/Node. :2-2 2 #{:c} 1)
        characters {node-0-0 [:a]
                    node-0-1 [:b]
                    node-0-2 [:c]
                    node-1-0 [:a :c]
                    node-2-0 [:a]
                    node-2-1 [:b]
                    node-2-2 [:c]}
        succ (f/Edge. node-0-1 node-2-1 #{:b} 1)
        pred (f/Edge. node-2-1 node-0-1 #{:b} 1)
        marked-1 (f/Edge. node-0-0 node-1-0 #{:a} 1)
        marked-2 (f/Edge. node-1-0 node-0-0 #{:a} 1)
        marked #{marked-1 marked-2}
        succs {node-0-0 #{marked-1}
               node-0-1 #{succ}
               node-0-2 #{(f/Edge. node-0-2 node-1-0 #{:c} 1)}
               node-1-0 #{(f/Edge. node-1-0 node-2-0 #{:a} 1)
                          (f/Edge. node-1-0 node-2-2 #{:c} 1)}}
        preds {node-1-0 #{marked-2
                          (f/Edge. node-1-0 node-0-2 #{:c} 1)}
               node-2-0 #{(f/Edge. node-2-0 node-1-0 #{:a} 1)}
               node-2-1 #{pred}
               node-2-2 #{(f/Edge. node-2-2 node-1-0 #{:c} 1)}}
        layers [(f/OrderedLayer. 0 0 [node-0-0 node-0-1 node-0-2])
                (f/OrderedLayer. 1 0 [node-1-0 [succ]])
                (f/OrderedLayer. 2 0 [node-2-0 node-2-1 node-2-2])]
        cm-graph (f/CountedAndMarkedGraph. layers succs preds 0 marked characters)
        succ-seg (f/Edge->Segment succ 1)
        pred-seg (f/Edge->Segment pred 1)]
    (is (= (f/CountedAndMarkedGraph->FlatGraph cm-graph)
           (f/map->FlatGraph {:layers [(f/FlatLayer. 0 0 [node-0-0 node-0-1 node-0-2])
                                       (f/FlatLayer. 1 0 [node-1-0 succ-seg])
                                       (f/FlatLayer. 2 0 [node-2-0 node-2-1 node-2-2])]
                              :succs succs
                              :preds preds
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
                                         node-2-2 0}
                              :marked marked
                              :crossings 0
                              :characters characters}))
        "Counted and marked graphs are converted to flat graphs")))


(deftest test-check-alignment
  (let [pred-edge (f/Edge. nil nil #{:a} 1)
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
  (let [node-1 (f/Node. :5-0 5 #{:a} 1)
        node-2 (f/Node. :0-0 0 #{:a} 1)
        pred (f/Edge. node-1 node-2 #{:a} 1)
        rev-pred (f/Edge. node-2 node-1 #{:a} 1)]
    (is (= (f/pred->segs+src pred)
           [(f/Segment. #{node-1 node-2} 1 #{:a} 1)
            (f/Segment. #{node-1 node-2} 2 #{:a} 1)
            (f/Segment. #{node-1 node-2} 3 #{:a} 1)
            (f/Segment. #{node-1 node-2} 4 #{:a} 1)
            node-1])
        "Predecessors are processed correctly when layers are increasing")
    (is (= (f/pred->segs+src rev-pred)
           [(f/Segment. #{node-1 node-2} 4 #{:a} 1)
            (f/Segment. #{node-1 node-2} 3 #{:a} 1)
            (f/Segment. #{node-1 node-2} 2 #{:a} 1)
            (f/Segment. #{node-1 node-2} 1 #{:a} 1)
            node-2])
        "Predecessors are processed correctly when layers are decreasing")))


(deftest test-blockify
  (let [;; Even predecessors
        node-1 (f/Node. :0-0 0 #{:a} 1)
        node-2 (f/Node. :0-1 0 #{:b} 1)
        ;; Odd predecessors
        node-3 (f/Node. :0-2 0 #{:c} 1)
        node-4 (f/Node. :0-3 0 #{:d} 1)
        node-5 (f/Node. :0-4 0 #{:e} 1)
        ;; weighted median
        node-6 (f/Node. :0-5 0 #{:f :g :h :i} 4)
        node-7 (f/Node. :0-6 0 #{:j} 1)
        node-8 (f/Node. :0-7 0 #{:k} 1)

        node-9 (f/Node. :1-0 1 #{:a :b} 2)
        edge-1 (f/Edge. node-9 node-1 #{:a} 1)
        edge-2 (f/Edge. node-9 node-2 #{:b} 1)
        node-10 (f/Node. :1-1 1 #{:c :d :e} 1)
        edge-3 (f/Edge. node-10 node-3 #{:c} 1)
        edge-4 (f/Edge. node-10 node-4 #{:d} 1)
        edge-5 (f/Edge. node-10 node-5 #{:e} 1)
        node-11 (f/Node. :1-2 1 #{:f :g :h :i :j :k} 6)
        edge-6 (f/Edge. node-11 node-6 #{:f :g :h :i} 4)
        edge-7 (f/Edge. node-11 node-7 #{:j} 1)
        edge-8 (f/Edge. node-11 node-8 #{:k} 1)
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


(let [node-0-0 (f/Node. :0-0 0 #{:a :b :c :d :e :f
                                 :g :h :i :j :k} 11)
      node-1-0 (f/Node. :1-0 1 #{:a :b :c :d :e} 5)
      node-1-1 (f/Node. :1-1 1 #{:f :g :h :i :j :k} 6)
      node-2-0 (f/Node. :2-0 2 #{:a :b :c :d} 4)
      node-2-1 (f/Node. :2-1 2 #{:f :g :h} 3)
      node-2-2 (f/Node. :2-2 2 #{:i :j} 2)
      node-2-3 (f/Node. :2-3 2 #{:k} 1)
      node-3-0 (f/Node. :3-0 3 #{:l :m :n :o :p :q :r :s} 8)
      node-3-1 (f/Node. :3-1 3 #{:f :g :h} 3)
      node-3-2 (f/Node. :3-2 3 #{:k} 1)
      node-4-0 (f/Node. :4-0 4 #{:l :m :n :o :p :q :r :s} 8)
      node-4-1 (f/Node. :4-1 4 #{:t :u :v :w :x} 5)
      node-4-2 (f/Node. :4-2 4 #{:i :j} 2)
      node-5-0 (f/Node. :5-0 5 #{:t :u :v :w :x} 5)
      node-6-0 (f/Node. :6-0 6 #{:y :z} 2)
      node-6-1 (f/Node. :6-1 6 #{:t :u :v :w :x} 5)
      node-7-0 (f/Node. :7-0 7 #{:1 :2 :3} 3)
      node-7-1 (f/Node. :7-1 7 #{:y :z} 2)
      node-8-0 (f/Node. :8-0 8 #{:1 :2 :3} 3)
      pred-1 (f/Edge. node-1-0 node-0-0 #{:a :b :c :d :e} 5)
      pred-2 (f/Edge. node-1-1 node-0-0 #{:f :g :h :i :j :k} 6)
      pred-3 (f/Edge. node-2-0 node-1-0 #{:a :b :c :d} 4)
      pred-4 (f/Edge. node-2-1 node-1-1 #{:f :g :h} 3)
      pred-5 (f/Edge. node-2-2 node-1-1 #{:i :j} 2)
      pred-6 (f/Edge. node-2-3 node-1-1 #{:k} 1)
      pred-7 (f/Edge. node-3-1 node-2-1 #{:f :g :h} 3)
      pred-8 (f/Edge. node-4-2 node-2-2 #{:i :j} 2)
      pred-9 (f/Edge. node-3-2 node-2-3 #{:k} 1)
      pred-10 (f/Edge. node-4-0 node-3-0 #{:l :m :n :o :p :q :r :s} 8)
      pred-11 (f/Edge. node-5-0 node-4-1 #{:t :u :v :w :x} 5)
      pred-12 (f/Edge. node-6-1 node-5-0 #{:t :u :v :w :x} 5)
      pred-13 (f/Edge. node-7-1 node-6-0 #{:y :z} 2)
      pred-14 (f/Edge. node-8-0 node-7-0 #{:1 :2 :3} 3)
      layer-0 (f/FlatLayer. 0 0 [node-0-0])
      layer-1 (f/FlatLayer. 1 0 [node-1-0 node-1-1])
      layer-2 (f/FlatLayer. 2 0 [node-2-0 node-2-1 node-2-2 node-2-3])
      layer-3 (f/FlatLayer. 3 0 [node-3-0 node-3-1 node-3-2])
      layer-4 (f/FlatLayer. 4 0 [node-4-0 node-4-1 node-4-2])
      layer-5 (f/FlatLayer. 5 0 [node-5-0])
      layer-6 (f/FlatLayer. 6 0 [node-6-0 node-6-1])
      layer-7 (f/FlatLayer. 7 0 [node-7-0 node-7-1])
      layer-8 (f/FlatLayer. 8 0 [node-8-0])
      seg (f/Segment. #{node-2-2 node-4-2} 3 #{:i :j} 2)
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
      block-graph (f/map->BlockGraph {:blocks [block-1
                                               block-5
                                               block-2
                                               block-8
                                               block-7
                                               block-6
                                               block-3
                                               block-4]
                                      :succs block-succs
                                      :preds block-preds
                                      :sources [block-1 block-5 block-8]})
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
      class-graph (f/map->ClassGraph {:classes classes
                                      :succs {class-2 #{(f/BlockEdge. block-5 block-2 8)
                                                        (f/BlockEdge. block-6 block-3 5)}
                                              class-3 #{(f/BlockEdge. block-7 block-6 2)}}
                                      :preds {class-1 #{(f/BlockEdge. block-2 block-5 8)
                                                        (f/BlockEdge. block-3 block-6 5)}
                                              class-2 #{(f/BlockEdge. block-6 block-7 2)}}
                                      :block-succs block-succs
                                      :block-preds block-preds
                                      :sources #{class-3}
                                      :sinks #{class-1}})]
  (deftest test-FlatGraph->BlockGraph
    (is (= (f/FlatGraph->BlockGraph graph) block-graph)
        "FlatGraphs translate to BlockGraphs"))
  (deftest test-classify-source
    (is (= (f/classify-source block-1 block-succs) (into #{} class-1))
        "Descendents of a source block are correctly categorized"))
  (deftest test-classify
    (is (= (f/classify block-graph) block-classes)
        "Classes are calculated from BlockGraphs"))
  (deftest test-BlockGraph->ClassGraph
    (is (= (f/BlockGraph->ClassGraph block-graph) class-graph)
        "ClassGraphs are derived from BlockGraphs"))
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
  (deftest test-get-rel-ys
    (is (= (f/get-rel-ys class-1 block-preds 50 15)
           {block-1 0
            block-2 110
            block-3 190
            block-4 255})
        "Relative y-positions within a class are calculated"))
  (deftest test-ClassGraph->YPlottedClassGraph
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
  (deftest test-FlatGraph->node-ys
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
    (is (= (f/plot graph 8 15 50 60 10)
           {:a [{:type :m, :x 0, :y 0}
                {:type :h, :x 50}],
            :b [{:type :m, :x 0, :y 10}
                {:type :h, :x 50}],
            :c [{:type :m, :x 0, :y 20}
                {:type :h, :x 50}],
            :d [{:type :m, :x 0, :y 30}
                {:type :h, :x 50}],
            :e [{:type :m, :x 0, :y 40}
                {:type :h, :x 0}],
            :f [{:type :m, :x 0, :y 50}
                {:type :h, :x 0}
                {:type :a, :radius 15, :sweep 0, :x 1.5745856353591172, :y 35}
                {:type :l, :x 15, :y 80}
                {:type :a, :radius 35, :sweep 1, :x 50, :y 80}
                {:type :a, :radius 35, :sweep 1, :x 85, :y 80}
                {:type :l, :x 85, :y 50}
                {:type :a, :radius 15, :sweep 0, :x 100, :y 50}],
            :g [{:type :m, :x 0, :y 60}
                {:type :h, :x 0}
                {:type :a, :radius 25, :sweep 0, :x 2.6243093922651948, :y 35}
                {:type :l, :x 25, :y 80}
                {:type :a, :radius 25, :sweep 1, :x 50, :y 80}
                {:type :a, :radius 25, :sweep 1, :x 75, :y 80}
                {:type :l, :x 75, :y 50}
                {:type :a, :radius 25, :sweep 0, :x 100, :y 50}],
            :h [{:type :m, :x 0, :y 70}
                {:type :h, :x 0}
                {:type :a, :radius 35, :sweep 0, :x 3.674033149171267, :y 35}
                {:type :l, :x 35, :y 80}
                {:type :a, :radius 15, :sweep 1, :x 50, :y 80}
                {:type :a, :radius 15, :sweep 1, :x 65, :y 80}
                {:type :l, :x 65, :y 50}
                {:type :a, :radius 35, :sweep 0, :x 100, :y 50}],
            :i [{:type :m, :x 0, :y 80}
                {:type :h, :x 0}
                {:type :a, :radius 35, :sweep 1, :x 35.00000000000001, :y 115}
                {:type :l, :x 35, :y 70}
                {:type :a, :radius 15, :sweep 0, :x 50, :y 70}
                {:type :a, :radius 25, :sweep 1, :x 62.18305555873586, :y 131.83055558735865}
                {:type :l, :x 142.6901666647585, :y 113.09833335241518}
                {:type :a, :radius 15, :sweep 0, :x 150, :y 100}],
            :j [{:type :m, :x 0, :y 90}
                {:type :h, :x 0}
                {:type :a, :radius 25, :sweep 1, :x 25.000000000000007, :y 115}
                {:type :l, :x 25, :y 70}
                {:type :a, :radius 25, :sweep 0, :x 50, :y 70}
                {:type :a, :radius 15, :sweep 1, :x 57.30983333524153, :y 123.09833335241517}
                {:type :l, :x 137.81694444126413, :y 121.83055558735862}
                {:type :a, :radius 25, :sweep 0, :x 150, :y 100}],
            :k [{:type :m, :x 0, :y 100}
                {:type :h, :x 0}
                {:type :a, :radius 15, :sweep 1, :x 11.032560937856337, :y 125.16280468928171}
                {:type :l, :x 38.96743906214366, :y 115.16280468928171}
                {:type :a, :radius 15, :sweep 0, :x 50, :y 105}
                {:type :a, :radius 15, :sweep 1, :x 59, :y 147}
                {:type :l, :x 91, :y 147}
                {:type :a, :radius 15, :sweep 0, :x 100, :y 135}],
            :l [{:type :m, :x 100, :y 0}
                {:type :h, :x 150}],
            :m [{:type :m, :x 100, :y 10}
                {:type :h, :x 150}],
            :n [{:type :m, :x 100, :y 20}
                {:type :h, :x 150}],
            :o [{:type :m, :x 100, :y 30}
                {:type :h, :x 150}],
            :p [{:type :m, :x 100, :y 40}
                {:type :h, :x 150}],
            :q [{:type :m, :x 100, :y 50} {:type :h, :x 150}],
            :r [{:type :m, :x 100, :y 60} {:type :h, :x 150}],
            :s [{:type :m, :x 100, :y 70}
                {:type :h, :x 150}],
            :t [{:type :m, :x 150, :y 65}
                {:type :a, :radius 15, :sweep 0, :x 164.884168150705, :y 50.276330184980495}
                {:type :l, :x 164.92923136115692, :y 53.97908022150265}
                {:type :a, :radius 55, :sweep 1, :x 219.9197552976123, :y 55}
                {:type :a, :radius 55, :sweep 1, :x 274.49503851686404, :y 48.17808959759357}
                {:type :l, :x 271.2053424445196, :y 21.860521018838117}
                {:type :a, :radius 15, :sweep 0, :x 286.0895105952246, :y 20}],
            :u [{:type :m, :x 150, :y 75}
                {:type :a, :radius 25, :sweep 0, :x 174.80694691784174, :y 50.460550308301194}
                {:type :l, :x 174.92750844051247, :y 54.164701999410596}
                {:type :a, :radius 45, :sweep 1, :x 219.9197552976123, :y 55}
                {:type :a, :radius 45, :sweep 1, :x 264.5722597497274, :y 49.41843694348569}
                {:type :l, :x 261.28256367738294, :y 23.10086836473017}
                {:type :a, :radius 25, :sweep 0, :x 286.0895105952246, :y 20}]
            :v [{:type :m, :x 150, :y 85}
                {:type :a, :radius 35, :sweep 0, :x 184.72972568497835, :y 50.64477043162132}
                {:type :l, :x 184.925785519868, :y 54.3503237773197}
                {:type :a, :radius 35, :sweep 1, :x 219.9197552976123, :y 55}
                {:type :a, :radius 35, :sweep 1, :x 254.6494809825907, :y 50.658784289377635}
                {:type :l, :x 251.35978491024628, :y 24.34121571062236}
                {:type :a, :radius 35, :sweep 0, :x 286.0895105952246, :y 20}],
            :w [{:type :m, :x 150, :y 95}
                {:type :a, :radius 45, :sweep 0, :x 194.652504452115, :y 50.82899055494148}
                {:type :l, :x 194.9240625992235, :y 54.53594555522848}
                {:type :a, :radius 25, :sweep 1, :x 219.9197552976123, :y 55}
                {:type :a, :radius 25, :sweep 1, :x 244.72670221545403, :y 51.899131635269896}
                {:type :l, :x 241.43700614310956, :y 25.581563056514184}
                {:type :a, :radius 45, :sweep 0, :x 286.0895105952246, :y 20}],
            :x [{:type :m, :x 150, :y 105}
                {:type :a, :radius 55, :sweep 0, :x 204.5752832192517, :y 51.01321067826212}
                {:type :l, :x 204.92233967857902, :y 54.721567333137}
                {:type :a, :radius 15, :sweep 1, :x 219.9197552976123, :y 55}
                {:type :a, :radius 15, :sweep 1, :x 234.80392344831733, :y 53.139478981161915}
                {:type :l, :x 231.5142273759729, :y 26.821910402406314}
                {:type :a, :radius 55, :sweep 0, :x 286.0895105952246, :y 20}],
            :y [{:type :m, :x 286.0895105952246, :y 0}
                {:type :a, :radius 25, :sweep 1, :x 306.0895105952246, :y 40}
                {:type :l, :x 324.0895105952246, :y 34}
                {:type :a, :radius 15, :sweep 0, :x 336.0895105952246, :y 25}],
            :z [{:type :m, :x 286.0895105952246, :y 10}
                {:type :a, :radius 15, :sweep 1, :x 298.0895105952246, :y 34}
                {:type :l, :x 316.0895105952246, :y 40}
                {:type :a, :radius 25, :sweep 0, :x 336.0895105952246, :y 25}],
            :1 [{:type :m, :x 336.0895105952246, :y 0}
                {:type :h, :x 336.0895105952246}],
            :2 [{:type :m, :x 336.0895105952246, :y 10}
                {:type :h, :x 336.0895105952246}],
            :3 [{:type :m, :x 336.0895105952246, :y 20}
                {:type :h, :x 336.0895105952246}]}))))

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

(deftest test-arc-centers
  (is (= (f/arc-center-up {:node-y [56 83]} 15) 41))
  (is (= (f/arc-center-down {:node-y [56 83]} 15) 98)))

(deftest test-arc-radii
  (is (= (f/arc-radius-up {:order 5} 15 20) 115))
  (is (= (f/arc-radius-down {:order 16 :node {:weight 21}} 15 20) 95)))

(deftest test-arc-y-info
  (is (= (f/arc-y-info {:order 5 :node {:weight 21} :node-y [56 456]}
                       {:order 2 :node {:weight 4} :node-y [62 122]}
                       :up 15 20)
         {:src-arc-y 41
          :src-arc-radius 115
          :dest-arc-y 137
          :dest-arc-radius 35}))
  (is (= (f/arc-y-info {:order 2 :node {:weight 4} :node-y [62 122]}
                       {:order 5 :node {:weight 21} :node-y [56 456]}
                       :down 15 20)
         {:src-arc-y 137
          :src-arc-radius 35
          :dest-arc-y 41
          :dest-arc-radius 115})))

(deftest test-add-y-info
  (is (= (f/add-y-info [{:pair [{:order 5 :node {:weight 21} :node-y [56 456]}
                                {:order 2 :node {:weight 4} :node-y [62 122]}]
                         :dir :up}
                        {:pair [{:order 2 :node {:weight 4} :node-y [62 122]}
                                {:order 5 :node {:weight 21} :node-y [56 456]}]
                         :dir :down}]
                       15 20)
         [{:pair [{:order 5 :node {:weight 21} :node-y [56 456]
                   :arc-y 41 :arc-radius 115}
                  {:order 2 :node {:weight 4} :node-y [62 122]
                   :arc-y 137 :arc-radius 35}]
           :dir :up
           :total-arc-radius 150}
          {:pair [{:order 2 :node {:weight 4} :node-y [62 122]
                   :arc-y 137 :arc-radius 35}
                  {:order 5 :node {:weight 21} :node-y [56 456]
                   :arc-y 41 :arc-radius 115}]
           :dir :down
           :total-arc-radius 150}])))

(deftest test-add-x-info
  (is (= (f/add-x-info [{:pair [{:node {:layer-id 0}}
                                {:node {:layer-id 1}}]}
                        {:pair [{:node {:layer-id 1}}
                                {:node {:layer-id 2}}]}]
                       [[15 27] [78 92] [156 200]])
         [{:pair [{:node {:layer-id 0} :arc-x 27 :xs [15 27]}
                  {:node {:layer-id 1} :arc-x 78 :xs [78 92]}]}
          {:pair [{:node {:layer-id 1} :arc-x 92 :xs [78 92]}
                  {:node {:layer-id 2} :arc-x 156 :xs [156 200]}]}])))

(deftest test-pair-up
  (is (= (f/pair-up [{:y 15} {:y 15} {:y 25} {:y 5}])
         [{:dir :level :pair [{:y 15} {:y 15}]}
          {:dir :down :pair [{:y 15} {:y 25}]}
          {:dir :up :pair [{:y 25} {:y 5}]}])))

(let [node-1 (f/Node. :0-0 0 #{:a :b :c} 3)
      node-2 (f/Node. :0-1 0 #{:d :e} 2)
      node-3 (f/Node. :0-2 0 #{:f} 1)
      node-4 (f/Node. :1-0 1 #{:a :b} 2)
      node-5 (f/Node. :1-1 1 #{:c :d :e} 3)
      node-6 (f/Node. :1-2 1 #{:f} 1)
      node-7 (f/Node. :2-0 2 #{:a :c} 2)
      node-8 (f/Node. :2-1 2 #{:b :e} 2)
      node-9 (f/Node. :2-2 2 #{:d :f} 2)
      path-info {:a [{:dir :down,
                      :pair [{:node node-1
                              :node-y [100 140], :y 100, :order 0, :arc-y 155, :arc-radius 55
                              :xs [0 15] :arc-x 15}
                             {:node node-4
                              :node-y [100 120], :y 120, :order 1, :arc-y 85, :arc-radius 35
                              :xs [275 302] :arc-x 275}],
                      :total-arc-radius 90}
                     {:dir :level,
                      :pair [{:node node-4
                              :node-y [100 120], :y 120, :order 1 :xs [275 302]}
                             {:node node-7
                              :node-y [100 120], :y 120, :order 1 :xs [602 680]}]}],
                 :b [{:dir :up,
                      :pair [{:node node-1
                              :node-y [100 140], :y 120, :order 1, :arc-y 85, :arc-radius 35
                              :xs [0 15] :arc-x 15}
                             {:node node-4
                              :node-y [100 120], :y 100, :order 0, :arc-y 135, :arc-radius 35
                              :xs [275 302] :arc-x 275}],
                      :total-arc-radius 70}
                     {:dir :down,
                      :pair [{:node node-4
                              :node-y [100 120], :y 100, :order 0, :arc-y 135, :arc-radius 35
                              :xs [275 302] :arc-x 302}
                             {:node node-8
                              :node-y [400 420], :y 400, :order 0, :arc-y 385, :arc-radius 15
                              :xs [602 680] :arc-x 602}],
                      :total-arc-radius 50}],
                 :c [{:dir :down,
                      :pair [{:node node-1
                              :node-y [100 140], :y 140, :order 2, :arc-y 155, :arc-radius 15
                              :xs [0 15] :arc-x 15}
                             {:node node-5
                              :node-y [400 440], :y 440, :order 2, :arc-y 385, :arc-radius 55
                              :xs [275 302] :arc-x 275}],
                      :total-arc-radius 70}
                     {:dir :up,
                      :pair [{:node node-5
                              :node-y [400 440], :y 440, :order 2, :arc-y 385, :arc-radius 55
                              :xs [275 302] :arc-x 302}
                             {:node node-7
                              :node-y [100 120], :y 100, :order 0, :arc-y 135, :arc-radius 35
                              :xs [602 680] :arc-x 602}],
                      :total-arc-radius 90}],
                 :e [{:dir :down,
                      :pair [{:node node-2
                              :node-y [400 420], :y 400, :order 0, :arc-y 435, :arc-radius 35
                              :xs [0 15] :arc-x 15}
                             {:node node-5
                              :node-y [400 440], :y 420, :order 1, :arc-y 385, :arc-radius 35
                              :xs [275 302] :arc-x 275}],
                      :total-arc-radius 70}
                     {:dir :level,
                      :pair [{:node node-5
                              :node-y [400 440], :y 420, :order 1 :xs [275 302]}
                             {:node node-8
                              :node-y [400 420], :y 420, :order 1 :xs [602 680]}]}],
                 :d [{:dir :up,
                      :pair [{:node node-2
                              :node-y [400 420], :y 420, :order 1, :arc-y 385, :arc-radius 35
                              :xs [0 15] :arc-x 15}
                             {:node node-5
                              :node-y [400 440], :y 400, :order 0, :arc-y 455, :arc-radius 55
                              :xs [275 302] :arc-x 275}],
                      :total-arc-radius 90}
                     {:dir :down,
                      :pair [{:node node-5
                              :node-y [400 440], :y 400, :order 0, :arc-y 455, :arc-radius 55
                              :xs [275 302] :arc-x 302}
                             {:node node-9
                              :node-y [700 720], :y 720, :order 1, :arc-y 685, :arc-radius 35
                              :xs [602 680] :arc-x 602}],
                      :total-arc-radius 90}],
                 :f [{:dir :level,
                      :pair [{:node node-3
                              :node-y [700 700], :y 700, :order 0 :xs [0 15]}
                             {:node node-6
                              :node-y [700 700], :y 700, :order 0 :xs [275 302]}]}
                     {:dir :level,
                      :pair [{:node node-6
                              :node-y [700 700], :y 700, :order 0 :xs [275 302]}
                             {:node node-9
                              :node-y [700 720], :y 700, :order 0 :xs [602 680]}]}]}]

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
                       (assoc-in [:pair 0 :arc-x] 15)
                       (assoc-in [:pair 1 :arc-x] 275))]
      (is (= (f/arcs-and-tangent pair-map)
             [{:type :a, :radius 55, :sweep 1, :x 46.22786856020158 :y 200.2749403664629}
              {:type :l, :x 255.12772000714446 :y 113.81132568774913}
              {:type :a, :radius 35, :sweep 0, :x 275 :y 85}]))))

  (deftest test-char-plots
    (let [up-info (-> (get (:b path-info) 0))
          up-plot [{:type :a, :radius 35, :sweep 0, :x 17.712552257943955, :y 53.600278873976116}
                   {:type :l, :x 271.987647109936, :y 100.12987338615297}
                   {:type :a, :radius 35, :sweep 1, :x 275, :y 135}
                   {:type :h, :x 302}]
          down-info (-> (get (:a path-info) 0))
          down-plot [{:type :a, :radius 55, :sweep 1, :x 46.22786856020158, :y 200.2749403664629}
                     {:type :l, :x 255.12772000714446, :y 113.81132568774913}
                     {:type :a, :radius 35, :sweep 0, :x 275, :y 85}
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

#_(let [characters {node-1 [:a :b :c]
                      node-2 [:e :d]
                      node-3 [:f]
                      node-4 [:b :a]
                      node-5 [:d :e :c]
                      node-6 [:f]
                      node-7 [:c :a]
                      node-8 [:b :e]
                      node-9 [:f :d]}
          ys {node-1 100 node-2 400 node-3 700
              node-4 100 node-5 400 node-6 700
              node-7 100 node-8 400 node-9 700}]
      (is (= (f/character-path-info [node-1 node-2 node-3 node-4 node-5
                                     node-6 node-7 node-8 node-9]
                                    characters ys 15 20)
             path-info)))
