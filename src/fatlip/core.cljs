(ns fatlip.core
  (:require [fatlip.sparse :refer [input->SparseGraph]]
            [fatlip.order :refer [SparseGraph->FlatGraph]]
            [fatlip.plot :as fp]))

(defn plot
  ([input]
   (plot input {}))
  ([input {:keys [max-slope min-arc-radius layer-sep node-sep char-sep]
           :or {max-slope 10
                min-arc-radius 15
                layer-sep 50
                node-sep 50
                char-sep 15}
           :as opts}]
   (-> input
       input->SparseGraph
       SparseGraph->FlatGraph
       (fp/plot max-slope min-arc-radius layer-sep node-sep char-sep))))
