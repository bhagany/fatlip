(ns fatlip.debug
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [fatlip.protocols :refer [nodes succs Node]]
            [fatlip.sparse :refer [input->SparseGraph]]
            [fatlip.order :refer [SparseGraph->FlatGraph]]
            [fatlip.plot :refer [FlatGraph->ClassGraphs ys plot-xs min-y max-y]]
            [fatlip.draw.d3 :refer [data->path]]))


#_(defn plot-alignments
  [flat-graph max-slope min-arc-radius layer-sep node-sep char-sep]
  (let [{:keys [layers characters]} flat-graph]
    (->> flat-graph
         FlatGraph->ClassGraphs
         (map #(ys % node-sep char-sep))
         (map #(pathify % (nodes flat-graph) characters min-arc-radius char-sep))
         (map #(plot-xs % layers max-slope layer-sep)))))


#_(defn input->plots
  [input & {:keys [max-slope min-arc-radius layer-sep node-sep char-sep]
            :or {max-slope 10
                 min-arc-radius 15
                 layer-sep 50
                 node-sep 50
                 char-sep 15}}]
  (-> input
      input->SparseGraph
      SparseGraph->FlatGraph
      (plot-alignments max-slope min-arc-radius layer-sep node-sep char-sep)))


;; all plot data:
;; every ordering change
;; class graphs for blocks and classes, on demand
;; xs and ys for each alignment + the final, on demand



(defn plot-coarse
  [flat-graph max-slope min-arc-radius layer-sep node-sep char-sep]
  (let [{:keys [layers characters]} flat-graph
        class-graphs (FlatGraph->ClassGraphs flat-graph)]
    (map (fn [class-graph]
           (let [node-ys (ys class-graph node-sep char-sep)
                 nodes (map (fn [node]
                              {:x (* layer-sep (:layer-id node))
                               :y (get node-ys node)
                               :id (:id node)})
                            (nodes flat-graph))
                 edges (map (fn [edge]
                              (let [node-1 (:src edge)
                                    node-2 (:dest edge)]
                                {:x1 (* layer-sep (:layer-id node-1))
                                 :x2 (* layer-sep (:layer-id node-2))
                                 :y1 (get node-ys node-1)
                                 :y2 (get node-ys node-2)}))
                            (reduce set/union (vals (succs flat-graph))))
                 ;; min-x is always 0 for these debug graphs
                 mn-x 0
                 mx-x (* layer-sep (dec (count layers)))
                 mn-y (min-y class-graph node-sep char-sep)
                 mx-y (max-y class-graph node-sep char-sep)
                 ;; _ (println mn-x mx-x mn-y mx-y)
                 vbox (s/join " " [(- mn-x 50) (- mn-y 50) (+ 100 (- mx-x mn-x)) (+ 100 (- mx-y mn-y))])]
             {:nodes nodes :edges edges :vbox vbox}))
         class-graphs)))


(defn draw-d3-coarse!
  [plot-data]
  (doall (map
          (fn [p-data]
            (let [svg (-> js/d3 (.select "#app") (.append "svg") (.attr "viewBox" (:vbox p-data))
                          (.attr "preserveAspectRatio" "xMidYMid meet"))]
              (-> svg
                  (.selectAll "path")
                  (.data (clj->js (:edges p-data)))
                  (.enter)
                  (.append "path")
                  (.attr "d" (fn [e] (str "M" (aget e "x1") " " (aget e "y1") " "
                                          "L" (aget e "x2") " " (aget e "y2"))))
                  (.attr "stroke" "#000000")
                  (.attr "fill" "none"))
              (-> svg
                  (.selectAll "circle")
                  (.data (clj->js (:nodes p-data)))
                  (.enter)
                  (.append "circle")
                  (.attr "cx" #(.-x %))
                  (.attr "cy" #(.-y %))
                  (.attr "r" "10")
                  (.attr "stroke" "#000000")
                  (.attr "fill" "#ff0000")
                  (.attr "data-node" #(.-id %)))))
          plot-data)))


(defn chart-coarse!
  [input & {:keys [max-slope min-arc-radius layer-sep node-sep char-sep]
            :or {max-slope 10
                 min-arc-radius 15
                 layer-sep 100
                 node-sep 50
                 char-sep 15}}]
  (-> input
      input->SparseGraph
      SparseGraph->FlatGraph
      (plot-coarse max-slope min-arc-radius layer-sep node-sep char-sep)
      draw-d3-coarse!))
