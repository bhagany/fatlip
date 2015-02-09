(ns asoiaf.agot.dev
  (:require [asoiaf.agot.core :as agot]
            [figwheel.client :as fw]))

(fw/start {:websocket-url "ws://localhost:3449/figwheel-ws"})
