(ns asoiaf.agot.dev
  (:require [asoiaf.agot.core :as agot]
            [figwheel.client :as fw]))

(enable-console-print!)

(fw/start {:websocket-url "ws://localhost:3449/figwheel-ws"})
