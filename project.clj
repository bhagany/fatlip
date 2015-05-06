(defproject fatlip "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[cljsjs/d3 "3.5.3-0"]
                 [com.cemerick/piggieback "0.1.5"]
                 [environ "1.0.0"]
                 [figwheel "0.3.1"]
                 [om "0.7.3"]
                 [org.clojure/clojure "1.7.0-beta2"]
                 [org.clojure/clojurescript "0.0-3211"]
                 [org.clojure/core.rrb-vector "0.0.11"]
                 [org.clojure/core.match "0.3.0-alpha4"]]

  :node-dependencies [[source-map-support "0.2.8"]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-figwheel "0.3.1"]]

  :source-paths ["src" "target/classes"]

  :clean-targets ["resources/public/out" "fatlip.js" "fatlip.min.js"]

  :min-lein-version "2.0.0"

  :cljsbuild {:builds
              {:test {:source-paths ["src" "test"]
                      :compiler {:output-to "resources/public/fatlip/test.js"
                                 :output-dir "resources/public/fatlip/test/out"
                                 :optimizations :none
                                 :main fatlip.test-runner
                                 :asset-path "fatlip/test/out"
                                 :source-map true
                                 :cache-analysis true
                                 :verbose true
                                 :pretty-print true}}
               ;; examples
               :agot-dev {:source-paths ["src" "examples/asoiaf/agot/src" "examples/asoiaf/agot/dev"]
                          :compiler {:output-to     "resources/public/asoiaf/agot/agot.js"
                                     :output-dir    "resources/public/asoiaf/agot/out"
                                     :main asoiaf.agot.dev
                                     :asset-path    "out"
                                     :source-map    true
                                     :optimizations :none
                                     :recompile-dependents true
                                     :cache-analysis true}}}}

  :figwheel {:http-server-root "public" ;; default and assumes "resources"
             :server-port 3449 ;; default
             :css-dirs ["resources/public/css"] ;; watch and update CSS

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             ;; if you want to disable the REPL
             ;; :repl false

             ;; to configure a different figwheel logfile path
             ;; :server-logfile "tmp/logs/figwheel-logfile.log"
             })
