(ns fatlip.order-test
  (:require [cljs.test :refer-macros [is deftest are testing]]
            [fatlip.protocols :refer [Node Edge Segment Edge->Segment rev]]
            [fatlip.core :as fc]
            [fatlip.order :as f]))


;; Fine-grained ESK
(deftest test-replace-ps
  (let [p (Node. :0-0 0 #{:a} 1)
        p-edge (Edge. p (Node. :1-0 0 #{:a} 1) #{:a} 1)
        not-p (Node. :0-1 0 #{:b} 1)
        seg-c-edge (Edge. not-p (Node. :3-99 3 #{:c} 1) #{:c} 1)
        seg-c [seg-c-edge]
        items [p seg-c not-p]
        ps #{p}
        succs {p #{p-edge}, not-p #{(Edge. not-p (Node. :1-0 0 #{:b} 1) #{:b} 1)}}]
    (is (= (f/replace-ps items ps succs)
           [[p-edge seg-c-edge] not-p])
        "P nodes get replaced by segment containers, and joined with adjacent segment containers")))


(deftest test-set-positions
  (let [seg-1 [(Edge. "src" "dest" #{} 0)
               (Edge. "src" "dest" #{} 0)]
        node-1 (Node. :0-0 0 #{} 0)
        node-2 (Node. :0-1 0 #{} 0)
        seg-2 [(Edge. "src" "dest" #{} 0)
               (Edge. "src" "dest" #{} 0)
               (Edge. "src" "dest" #{} 0)]
        node-3 (Node. :0-2 0 #{} 0)
        seg-3 [(Edge. "src" "dest" #{} 0)]
        node-4 (Node. :0-3 0 #{} 0)
        minus-ps [seg-1 node-1 node-2 seg-2 node-3 seg-3 node-4]]
    (is (= (f/set-positions minus-ps) {seg-1 0, node-1 2, node-2 3
                                       seg-2 4, node-3 7, seg-3 8
                                       node-4 9})
        "Positions are set correctly")))


(deftest test-get-measure
  (let [node (Node. :1-0 1 #{:a :b :c} 3)
        pr-node-1 (Node. :0-0 0 #{:a :b} 2)
        pr-node-2 (Node. :0-1 0 #{:c} 1)
        edge-1 (Edge. node pr-node-1 #{:a :b} 2)
        edge-2 (Edge. node pr-node-2 #{:c} 1)
        preds #{edge-1 edge-2}
        pred-positions {pr-node-1 1, pr-node-2 2}]
    (is (= (f/get-measure node preds pred-positions) (/ 4 3))
        "The measure of a node is calculated correctly")))


(deftest test-set-measures
  (let [l-node-1 (Node. :0-0 0 #{:a :b :c} 3)
        l-node-2 (Node. :0-1 0 #{:d} 1)
        l-node-3 (Node. :0-2 0 #{:e} 1)
        nl-node-1 (Node. :1-0 1 #{:c} 1)
        nl-node-2 (Node. :1-1 1 #{:b :e} 2)
        nl-node-3 (Node. :1-2 1 #{:a} 1)
        nl-node-4 (Node. :1-3 1 #{:d} 1)
        edge-1 (Edge. nl-node-1 l-node-1 #{:c} 1)
        edge-2 (Edge. nl-node-2 l-node-1 #{:b} 1)
        edge-3 (Edge. nl-node-2 l-node-3 #{:e} 1)
        edge-4 (Edge. nl-node-3 l-node-1 #{:a} 1)
        edge-5 (Edge. nl-node-4 l-node-2 #{:d} 1)
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
  (let [l-node-1 (Node. :0-0 0 #{:a :b :c} 3)
        l-node-2 (Node. :0-1 0 #{:d} 1)
        l-node-3 (Node. :0-2 0 #{:e} 1)
        nl-node-1 (Node. :1-0 1 #{:c} 1)
        nl-node-2 (Node. :1-1 1 #{:b :e} 2)
        nl-node-3 (Node. :1-2 1 #{:a} 1)
        nl-node-4 (Node. :1-3 1 #{:d} 1)
        nl-node-5 (Node. :1-4 1 #{:f :g} 2)
        nl-node-6 (Node. :1-5 1 #{:m} 1)
        l-seg-1 [(Edge. "meh" "somewhere else" #{:x} 1)]
        l-seg-2 [(Edge. "bleh" "elsewhere" #{:y} 1)
                 (Edge. "geh" nl-node-5 #{:f :g} 2)]
        split-edge-1 (Edge. "heh" "whatever" #{:h} 1)
        split-edge-2 (Edge. "teh" "edge" #{:i} 1)
        l-seg-3 [split-edge-1 split-edge-2]
        minus-ps [l-node-1 l-seg-1 l-node-2 l-seg-2 l-node-3 l-seg-3]
        positions {l-node-1 0
                   l-seg-1 1
                   l-node-2 2
                   l-seg-2 3
                   l-node-3 5
                   l-seg-3 6}
        non-qs [nl-node-1 nl-node-2 nl-node-3 nl-node-4 nl-node-5]
        measures {nl-node-1 0
                  nl-node-2 2.5
                  nl-node-3 0
                  nl-node-4 2
                  nl-node-5 7}]
    (is (= (f/merge-layer minus-ps positions non-qs measures)
           [nl-node-1 nl-node-3 l-seg-1 nl-node-4 nl-node-2 l-seg-2
            [split-edge-1] nl-node-5 [split-edge-2]])
        "Nodes and segment containers are merged correctly")))


(deftest test-add-qs
  (let [node-1 (Node. :0-0 0 #{:a :b :c} 3)
        node-2 (Node. :0-1 0 #{:d} 1)
        node-3 (Node. :0-2 0 #{:e} 1)
        edge-1 (Edge. "meh"  "somewhere else" #{:x} 1)
        edge-2 (Edge. "bleh" "elsewhere" #{:y} 1)
        edge-3 (Edge. "feh" node-2 #{:f :g} 2)
        edge-4 (Edge. "doesn't" "matter" #{:m} 1)
        seg-c-1 [edge-1]
        seg-c-2 [edge-2 edge-3 edge-4]
        minus-qs [node-1 seg-c-1 node-3 seg-c-2]
        qs #{node-2}]
    (is (= (f/add-qs minus-qs qs)
           [node-1 seg-c-1 node-3 [edge-2] node-2 [edge-4]])
        "Q nodes are placed correctly, splitting the segment containers they were in")))


(deftest test-sorted-edge-order
  (let [node-1 (Node. :0-0 0 #{:a :d :e} 3)
        node-2 (Node. :0-1 0 #{:c :g} 2)
        nl-node-1 (Node. :1-0 1 #{:a :c} 2)
        nl-node-2 (Node. :1-1 1 #{:d :g} 2)
        nl-node-3 (Node. :1-1 1 #{:e} 1)
        seg [(Edge. "mmm" "hmm" #{:b :f} 2)]
        ordered [node-1 seg node-2]
        next-ordered [nl-node-1 seg nl-node-2 nl-node-3]
        edge-1 (Edge. node-1 nl-node-1 #{:a} 1)
        edge-2 (Edge. node-1 nl-node-2 #{:d} 1)
        edge-3 (Edge. node-1 nl-node-3 #{:e} 1)
        edge-4 (Edge. seg seg #{:b :f} 2)
        edge-5 (Edge. node-2 nl-node-1 #{:c} 1)
        edge-6 (Edge. node-2 nl-node-2 #{:g} 1)
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
        edge (Edge. (Node. :0-0 0 #{:b} 1)
                      (Node. :1-3 1 #{:b} 1)
                      #{:b} 1)
        seg [(Edge. "mmm" "hmm" #{:b :h} 2)]
        sedge (Edge. seg seg #{:b :h} 2)
        crossed-edge (Edge. (Node. :0-1 1 #{:f} 1)
                              (Node. :1-2 1 #{:f} 1)
                              #{:f} 1)
        edge-1 (Edge. (Node. :0-2 0 #{:a :d :e} 3)
                        (Node. :1-0 1 #{:a :d :e} 3)
                        #{:a :d :e} 3)
        edge-2 (Edge. (Node. :0-3 0 #{:c :g} 2)
                        (Node. :1-1 1 #{:c :g} 2)
                        #{:c :g} 2)
        tree [(f/AccumulatorNode. 2
                                  #{edge-1 (rev edge-1)
                                    edge-2 (rev edge-2)}
                                  false)
              (f/AccumulatorNode. 1 #{} true)
              (f/AccumulatorNode. 1
                                  #{crossed-edge (rev crossed-edge)}
                                  false)]]
    (is (= (f/single-edge-super-crossings tree 3 edge)
           [tree 3 #{edge (rev edge)}])
        "Correctly sum up and mark an edge that crosses a segment")
    (is (= (f/single-edge-super-crossings tree 5 edge)
           [(-> tree
                (update-in [0 :node-edges] conj edge (rev edge))
                (update-in [0 :weight] inc))
            1 #{}])
        "Correctly sum up and don't mark and edge that crosses a segment")
    (is (= (f/single-edge-super-crossings tree 5 sedge)
           [(-> tree
                (assoc-in [0 :is-seg-c] true)
                (update-in [0 :weight] + 2))
            2
            #{crossed-edge (rev crossed-edge)}])
        "Correctly sum up crossings and markings when adding a segment")))


(deftest test-count-and-mark-super-crossings
  (let [l-node-1 (Node. :0-0 0 #{:a :b :c} 3)
        l-node-2 (Node. :0-1 0 #{:d} 1)
        l-node-3 (Node. :0-2 0 #{:e} 1)
        nl-node-1 (Node. :1-0 1 #{:c} 2)
        nl-node-2 (Node. :1-1 1 #{:b :e} 2)
        nl-node-3 (Node. :1-2 1 #{:a} 1)
        nl-node-4 (Node. :1-3 1 #{:d} 1)
        nl-node-5 (Node. :1-4 1 #{:f :g} 2)
        l-seg-1 [(Edge. "bleh" "somewhere else" #{:x} 1)]
        l-seg-2 [(Edge. "meh" "elsewhere" #{:y} 1)
                 (Edge. "geh" nl-node-5 #{:f :g} 2)]
        edge-1 (Edge. nl-node-1 l-node-1 #{:c} 1)
        edge-2 (Edge. nl-node-2 l-node-1 #{:b} 1)
        edge-3 (Edge. nl-node-2 l-node-3 #{:e} 1)
        edge-4 (Edge. nl-node-3 l-node-1  #{:a} 1)
        edge-5 (Edge. nl-node-4 l-node-2 #{:d} 1)
        minus-ps [l-node-1 l-seg-1 l-node-2 l-seg-2 l-node-3]
        minus-qs [nl-node-1 nl-node-3 l-seg-1 nl-node-4 nl-node-2 l-seg-2]
        preds {nl-node-1 #{edge-1}
               nl-node-2 #{edge-2 edge-3}
               nl-node-3 #{edge-4}
               nl-node-4 #{edge-5}}]
    (is (= (f/count-and-mark-super-crossings minus-ps minus-qs preds {})
           [5 #{edge-2 (rev edge-2) edge-3 (rev edge-3)}])
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
  (let [node (Node. :0-0 0 #{:a} 1)
        dests #{}]
    (is (= (f/count-sub-crossings-single-node node dests {}) 0)
        "A node with 0 successors has 0 sub-crossings"))
  (let [node (Node. :0-0 0 #{:a} 1)
        dest (Node. :1-0 1 #{:a} 1)
        dests #{dest}]
    (is (= (f/count-sub-crossings-single-node node dests {dest [:a]}) 0)
        "A node with 1 successor has 0 sub-crossings"))
  (let [node (Node. :0-0 0 #{:a :b} 2)
        dest-1 (Node. :1-0 1 #{:a} 1)
        dest-2 (Node. :1-1 1 #{:b} 1)
        dests [dest-1 dest-2]
        characters {node [:b :a]
                    dest-1 [:a]
                    dest-2 [:b]}]
    (is (= (f/count-sub-crossings-single-node node dests characters) 1)
        "A node with multiple successors has its sub-crossings counted")))


(deftest test-count-sub-crossings
  (let [l-node-1 (Node. :0-0 0 #{:a :b :c} 3)
        l-node-2 (Node. :0-1 0 #{:d} 1)
        l-node-3 (Node. :0-2 0 #{:e} 1)
        nl-node-1 (Node. :1-0 1 #{:c} 1)
        nl-node-2 (Node. :1-1 1 #{:b} 1)
        nl-node-3 (Node. :1-2 1 #{:a} 1)
        nl-node-4 (Node. :1-3 1 #{:d} 1)
        minus-ps [l-node-1 l-node-2 l-node-3]
        minus-qs [nl-node-1 nl-node-2 nl-node-3 nl-node-4]
        succs {l-node-1 (set (map #(Edge. l-node-1 % (:characters %) (:weight %))
                                  [nl-node-1 nl-node-2 nl-node-3]))
               l-node-2 (set (map #(Edge. l-node-2 % (:characters %) (:weight %))
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
  (let [node-1 (Node. :0-0 0 #{:a} 1)
        node-2 (Node. :0-1 0 #{:b} 1)
        node-3 (Node. :0-2 0 #{:c} 1)
        seg-1 (Segment. "bleh" 0 #{:d} 1)
        seg-2 (Segment. "geh" 0 #{:e} 1)
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
  (let [node-1 (Node. :0-0 0 #{:a} 1)
        node-2 (Node. :0-1 0 #{:b} 1)
        node-3 (Node. :0-2 0 #{:c} 1)
        seg-1 (Segment. "bleh" 0 #{:d} 1)
        seg-2 (Segment. "geh" 0 #{:e} 1)
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
  (let [node-1 (Node. :0-0 0 #{:a} 1)
        node-2 (Node. :0-1 0 #{:b} 1)
        edge-1 (Edge. node-1 "meh" #{:c} 1)
        edge-2 (Edge. node-2 "bleh" #{:d} 1)
        seg-c [edge-1 edge-2]
        ordered-layer (f/OrderedLayer. 0 0 [node-1 seg-c node-2])]
    (is (= (f/OrderedLayer->FlatLayer ordered-layer)
           (f/FlatLayer. 0 0 [node-1
                              (Edge->Segment edge-1 0)
                              (Edge->Segment edge-2 0)
                              node-2]))
        "Ordered layers get transformed into flat layers")))


(deftest test-OrderedGraph->FlatGraph
  (let [node-0-0 (Node. :0-0 0 #{:a} 1)
        node-0-1 (Node. :0-1 0 #{:b} 1)
        node-0-2 (Node. :0-2 0 #{:c} 1)
        node-1-0 (Node. :1-0 1 #{:a :c} 2)
        node-2-0 (Node. :2-0 2 #{:a} 1)
        node-2-1 (Node. :2-1 2 #{:b} 1)
        node-2-2 (Node. :2-2 2 #{:c} 1)
        characters {node-0-0 [:a]
                    node-0-1 [:b]
                    node-0-2 [:c]
                    node-1-0 [:a :c]
                    node-2-0 [:a]
                    node-2-1 [:b]
                    node-2-2 [:c]}
        succ (Edge. node-0-1 node-2-1 #{:b} 1)
        marked-1 (Edge. node-0-2 node-1-0 #{:c} 1)
        marked-2 (Edge. node-1-0 node-0-2 #{:c} 1)
        marked-3 (Edge. node-1-0 node-2-2 #{:c} 1)
        marked-4 (Edge. node-2-2 node-1-0 #{:c} 1)
        marked #{marked-1 marked-2 marked-3 marked-4}
        succs {node-0-0 #{(Edge. node-0-0 node-1-0 #{:a} 1)}
               node-0-1 #{succ}
               node-0-2 #{marked-1}
               node-1-0 #{(Edge. node-1-0 node-2-0 #{:a} 1)
                          marked-3}}
        preds {node-1-0 #{(Edge. node-1-0 node-0-0 #{:a} 1)
                          marked-2}
               node-2-0 #{(Edge. node-2-0 node-1-0 #{:a} 1)}
               node-2-1 #{(Edge. node-2-1 node-0-1 #{:b} 1)}
               node-2-2 #{marked-4}}
        layers [(f/OrderedLayer. 0 0 [node-0-0 node-0-1 node-0-2])
                (f/OrderedLayer. 1 0 [node-1-0 [succ]])
                (f/OrderedLayer. 2 0 [node-2-0 node-2-1 node-2-2])]
        succ-seg (Edge->Segment succ 1)
        minus-ps [[node-0-0 [succ] node-0-2] [node-1-0 [succ]]]
        minus-qs [[node-1-0 [succ]] [node-2-0 [succ] node-2-2]]
        o-graph (f/OrderedGraph. layers succs preds #{} #{} #{} minus-ps minus-qs characters)]
    (is (= (f/OrderedGraph->FlatGraph o-graph)
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
                              :crossings 2
                              :characters characters}))
        "Counted and marked graphs are converted to flat graphs")))
