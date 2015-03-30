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
  (let [svg (-> js/d3 (.select "#app") (.append "svg") (.attr "viewBox" #_"0 0 500 1000" "0 0 4700 1000"))]
    (-> svg
        (.selectAll "path")
        (.data (clj->js (drop 0 (take 1000 plot-data))))
        (.enter)
        (.append "path")
        (.attr "d" #(s/join " " (map data->path (.-plots %))))
        (.attr "class" #(.-character %))
        (.attr "stroke" "#000000")
        (.attr "fill" "none"))))