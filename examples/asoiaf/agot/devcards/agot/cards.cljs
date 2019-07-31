(ns agot.cards
  (:require [agot.defs :as defs]
            [clojure.set :as set]
            [clojure.string :as s]
            [devcards.core :as dc :refer-macros [defcard]]
            [fatlip.sparse :refer [input->SparseGraph]]
            [fatlip.order :as ord]
            [fatlip.plot :refer [plot FlatGraph->ClassGraphs ys plot-xs min-y max-y left right]]
            [fatlip.protocols :refer [nodes succs Node]]
            [reagent.core :as r]))

(enable-console-print!)
(devcards.core/start-devcard-ui!)

(defcard default-render
  (dc/reagent defs/agot-component))

(defn edge-attrs
  [flat-graph node-ys layer-sep]
  (map (fn [edge]
         (let [node-1 (:src edge)
               node-2 (:dest edge)]
           {:x1 (* layer-sep (:layer-id node-1))
            :x2 (* layer-sep (:layer-id node-2))
            :y1 (get node-ys node-1)
            :y2 (get node-ys node-2)}))
       (reduce set/union (vals (succs flat-graph)))))

(defn node-debug-data
  [block node-ys layer-sep]
  (->> block
       (filter #(instance? Node %))
       (map (fn [node]
              {:x (* layer-sep (:layer-id node))
               :y (get node-ys node)
               :id (:id node)}))))

(defn block-debug-data
  [class node-ys layer-sep]
  (->> class
       (map #(let [left-node (left %)
                   right-node (right %)
                   x1 (* layer-sep (:layer-id left-node))
                   y1 (get node-ys left-node)
                   x2 (* layer-sep (:layer-id right-node))
                   block-height 36
                   buffer (/ block-height 2)
                   nodes (node-debug-data % node-ys layer-sep)]
               {:id (str "block-" (:id left-node))
                :x (- x1 buffer)
                :y (- y1 buffer)
                :width (+ (- x2 x1) block-height)
                :height block-height
                :nodes nodes}))))

(defn class-debug-data
  [class-graph node-ys layer-sep node-sep]
  (->> (:classes class-graph)
       (map (fn [class]
              (let [points
                    (->> class
                         (group-by #(get node-ys (first %)))
                         (sort-by first)
                         (reduce
                          (fn [[lefts rights] [y blocks]]
                            (let [l-layer (apply min
                                                 (map
                                                  (comp :layer-id left)
                                                  blocks))
                                  r-layer (apply max
                                                 (map
                                                  (comp :layer-id right)
                                                  blocks))
                                  buffer (/ node-sep 2)
                                  left (- (* l-layer layer-sep) buffer)
                                  right (+ (* r-layer layer-sep) buffer)
                                  top (- y buffer)
                                  bottom (+ y buffer)]
                              [(into lefts [{:x left :y top}
                                            {:x left :y bottom}])
                               (into rights [{:x right :y top}
                                             {:x right :y bottom}])]))
                          [[] '()])
                         (apply concat))
                    blocks (block-debug-data class node-ys layer-sep)]
                {:id (str "class-" (:id (ffirst class)))
                 :points points
                 :blocks blocks})))))

(defn plot-classes
  [flat-graph max-slope min-arc-radius layer-sep node-sep char-sep]
  (let [{:keys [layers characters]} flat-graph
        class-graphs (FlatGraph->ClassGraphs flat-graph)]
    (map
     (fn [class-graph]
       (let [alignment (:alignment class-graph)
             compaction (:compaction class-graph)
             node-ys (ys class-graph node-sep char-sep)
             edges (edge-attrs flat-graph node-ys layer-sep)
             ;; nodes (node-attrs flat-graph node-ys layer-sep)
             ;; blocks (block-attrs class-graph node-ys layer-sep)
             ;; classes (class-attrs class-graph node-ys layer-sep node-sep)
             classes (class-debug-data class-graph node-ys layer-sep node-sep)
             ;; min-x is always 0 for these debug graphs
             mn-x 0
             mx-x (* layer-sep (dec (count layers)))
             mn-y (min-y class-graph node-sep char-sep)
             mx-y (max-y class-graph node-sep char-sep)
             vbox (s/join " " [(- mn-x 50)
                               (- mn-y 50)
                               (+ 100 (- mx-x mn-x))
                               (+ 100 (- mx-y mn-y))])]
         {:classes classes :edges edges :vbox vbox
          :alignment alignment :compaction compaction}))
     class-graphs)))

(defn chart-classes
  ([input]
   (chart-classes input {}))
  ([input {:keys [max-slope min-arc-radius layer-sep node-sep char-sep]
           :or {max-slope 10
                min-arc-radius 0
                layer-sep 100
                node-sep 50
                char-sep 0}}]
   (-> input
       input->SparseGraph
       ord/SparseGraph->FlatGraph
       (plot-classes max-slope min-arc-radius layer-sep node-sep char-sep))))

(defonce state (r/atom {:nodes true :blocks true :classes true :edges false}))

(defn classes-component
  [state]
  (let [classes-plots (chart-classes defs/input)]
    (fn [_]
      (let [show @state]
        (into
         [:div
          [:h2 "Plotting"]
          [:input {:type "checkbox" :checked (:nodes show)
                   :onChange #(swap! state update :nodes not)}]
          [:span {:style {:margin-right "10px"}} "Nodes"]
          [:input {:type "checkbox" :checked (:blocks show)
                   :onChange #(swap! state update :blocks not)}]
          [:span {:style {:margin-right "10px"}} "Blocks"]
          [:input {:type "checkbox" :checked (:classes show)
                   :onChange #(swap! state update :classes not)}]
          [:span {:style {:margin-right "10px"}} "Classes"]
          [:input {:type "checkbox" :checked (:edges show)
                   :onChange #(swap! state update :edges not)}]
          [:span {:style {:margin-right "10px"}} "Edges"]]
         (map
          (fn [{:keys [classes edges vbox alignment compaction]}]
            ^{:key (str "classes-" alignment "-" compaction)}
            [:div
             [:style ".class-group:hover .debug-class { fill:yellow; opacity:.6 }"]
             [:h3 "Aligned " alignment ", Compacted " compaction]
             [:svg {:viewBox vbox :preserveAspectRatio "xMidYMid meet"}
              (when (:edges show)
                (map #(let [d (str "M" (:x1 %) " " (:y1 %) " "
                                   "L" (:x2 %) " " (:y2 %))
                            stroke "#000000"
                            fill "none"]
                        ^{:key d} [:path {:d d :stroke stroke :fill fill}])
                     edges))
              (map
               (fn [{:keys [id points blocks]}]
                 (into
                  ^{:key id}
                  [:g {:class "class-group"}
                   (let [pts (s/join " " (map (fn [{:keys [x y]}]
                                                (str x "," y)) points))]
                     [:polygon {:points pts :fill "green" :opacity ".15"
                                :stroke "black" :stroke-width "5px"
                                :stroke-opacity "1" :class "debug-class"
                                :visibility (if (:classes show)
                                              "visible" "hidden")}])]
                  (mapcat
                   (fn [{:keys [id x y width height nodes]}]
                     ^{:key id}
                     (into
                      [[:rect {:x x :y y :width width :height height
                               :fill "blue" :opacity ".25"
                               :rx "5" :class "debug-block"
                               :visibility (if (:blocks show)
                                             "visible" "hidden")}]]
                      (map
                       (fn [{:keys [id x y]}]
                         (let [stroke "#000000"
                               fill "#ff0000"]
                           ^{:key id}
                           [:circle {:cx x :cy y :r "10"
                                     :stroke stroke :fill fill
                                     :visibility (if (:nodes show)
                                                   "visible" "hidden")}]))
                       nodes)))
                   blocks)))
               classes)]])
          classes-plots))))))

(defcard classes-render
  (dc/reagent classes-component)
  state)

(defn plot-orderings
  ([input]
   (plot-orderings input {}))
  ([input {:keys [max-slope min-arc-radius layer-sep node-sep char-sep]
           :or {max-slope 10
                min-arc-radius 15
                layer-sep 50
                node-sep 50
                char-sep 15}}]
   (->> input
        input->SparseGraph
        ord/SparseGraph->ordered-graphs
        (map ord/OrderedGraph->FlatGraph)
        (map #(plot % max-slope min-arc-radius layer-sep node-sep char-sep)))))

(defn ordering-component
  []
  [:div
   (map-indexed
    (fn [i {:keys [min-x min-y max-x max-y plots]}]
      (let [width (- max-x min-x)
            height (- max-y min-y)]
        ^{:key (str "ordering-" i)}
        [:svg {:viewBox (s/join " " [min-x min-y width height])}
         (map #(let [{:keys [plots character]} %
                     d (s/join " " (map defs/data->path plots))
                     stroke (get defs/character-colors character)
                     fill "none"]
                 ^{:key character}
                 [:path {:d d :class character :stroke stroke :fill fill
                         :stroke-width (when (defs/pov-characters character) "2")}])
              plots)]))
    (plot-orderings defs/input))])

(defcard ordering
  (dc/reagent ordering-component))
