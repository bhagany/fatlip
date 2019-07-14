(ns agot.core
  (:require [agot.defs :as defs]
            [reagent.core :as r]))

(r/render [defs/agot-component] (js/document.getElementById "app"))

#_(def test-graph (debug/chart-coarse! input))
