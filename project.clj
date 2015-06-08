(defproject fatlip "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[cljsjs/d3 "3.5.3-0"]
                 [com.cemerick/piggieback "0.1.5"]
                 [environ "1.0.0"]
                 [figwheel "0.3.3" :exclusions [org.clojure/clojure]]
                 [om "0.7.3"]
                 [org.clojure/clojure "1.7.0-RC1"]
                 [org.clojure/clojurescript "0.0-3308" :classifier "aot" :exclusions [org.clojure/tools.reader
                                                                                      org.clojure/data.json]]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.rrb-vector "0.0.11"]
                 [org.clojure/data.json "0.2.6" :classifier "aot"]
                 [org.clojure/tools.reader "0.9.2" :classifier "aot"]]

  :plugins [[lein-cljsbuild "1.0.6"]
            [lein-figwheel "0.3.3"]]

  ;; :resource-paths to change where figwheel serves from?d

  :clean-targets ^{:protect false} ["resources/public/out" "fatlip.js" "fatlip.min.js"]

  :cljsbuild {:builds
              {:test {:source-paths ["src" "test"]
                      :figwheel true
                      :compiler {:output-to "resources/public/fatlip/test/test.js"
                                 :output-dir "resources/public/fatlip/test/out"
                                 :optimizations :none
                                 :main fatlip.test-runner
                                 :asset-path "out"
                                 :source-map true
                                 :cache-analysis true
                                 :verbose true
                                 :pretty-print true}}
               ;; examples
               :agot-dev {:source-paths ["src" "examples/asoiaf/agot/src"]
                          :figwheel true
                          :compiler {:output-to     "resources/public/asoiaf/agot/agot.js"
                                     :output-dir    "resources/public/asoiaf/agot/out"
                                     :main agot.core
                                     :asset-path    "out"
                                     :source-map    true
                                     :optimizations :none
                                     :recompile-dependents true
                                     :cache-analysis true}}}})
