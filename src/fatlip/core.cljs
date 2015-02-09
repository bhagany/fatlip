(ns fatlip.core
  (:require [fatlip.sparse :refer [input->SparseGraph]]
            [fatlip.order :refer [SparseGraph->FlatGraph]]
            [fatlip.plot :refer [plot]]
            #_[fatlip.draw.svg :as draw-svg]))


(defn chart!
  [input & {:keys [max-slope min-arc-radius layer-sep node-sep char-sep]
            :or {max-slope 10
                 min-arc-radius 15
                 layer-sep 50
                 node-sep 50
                 char-sep 15}}]
  (-> input
      input->SparseGraph
      SparseGraph->FlatGraph
      (plot max-slope min-arc-radius layer-sep node-sep char-sep)))
