(ns fatlip.draw.d3
  (:require cljsjs.d3
            [clojure.string :as s]))


(defn data->path
  [data]
  (case (.-type data)
    "h" (str "H" (.-x data))
    "m" (str "M" (.-x data) " " (.-y data))
    "l" (str "L" (.-x data) " " (.-y data))
    "a" (let [[radius sweep x y] (map #(aget data %)
                                      ["radius" "sweep" "x" "y"])]
          (str "A" (s/join " " [radius radius "0 0" sweep x y])))))


(defn draw-d3!
  [plot-data]
  (let [{:keys [min-x max-x min-y max-y]} plot-data
        width (- max-x min-x)
        height (- max-y min-y)
        svg (-> js/d3 (.select "#app") (.append "svg") (.attr "viewBox" (s/join " " [min-x
                                                                                     min-y
                                                                                     width
                                                                                     height])))]
    (-> svg
        (.selectAll "path")
        (.data (clj->js (:plots plot-data)))
        (.enter)
        (.append "path")
        (.attr "d" #(s/join " " (map data->path (.-plots %))))
        (.attr "class" #(.-character %))
        (.attr "stroke" "#000000")
        (.attr "fill" "none"))
    #_(-> svg
        (.selectAll "circle")
        (.data (clj->js (drop 8 (take 20 (filter #(= (:type %) :a) (mapcat :plots (drop 0 (take 1 plot-data))))))))
        (.enter)
        (.append "circle")
        (.attr "cx" #(aget % "arc-x"))
        (.attr "cy" #(aget % "arc-y"))
        (.attr "r" #(.-radius %))
        (.attr "class" #(.-character %))
        (.attr "stroke" "red")
        (.attr "fill" "none"))))
