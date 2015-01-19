(defproject fatlip "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[com.cemerick/austin "0.1.6"]
                 [com.cemerick/piggieback "0.1.5"]
                 [compojure "1.2.0"]
                 [enlive "1.1.5"]
                 [environ "1.0.0"]
                 [figwheel "0.1.4-SNAPSHOT"]
                 [om "0.7.3"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2665"]
                 [org.clojure/core.rrb-vector "0.0.11"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [ring "1.3.1"]
                 [weasel "0.4.0-SNAPSHOT"]]

  :node-dependencies [[source-map-support "0.2.8"]]

  :plugins [[com.cemerick/clojurescript.test "0.3.1"]
            [lein-cljsbuild "1.0.4"]
            [lein-environ "1.0.0"]
            [lein-kibit "0.0.8"]
            [lein-npm "0.4.0"]]

  :source-paths ["src/clj" "src/cljs" "target/classes"]

  :clean-targets ["resources/public/out" "fatlip.js" "fatlip.min.js"]

  :min-lein-version "2.0.0"

  :uberjar-name "fatlip.jar"

  :cljsbuild {:builds
              {:dev {:source-paths ["src/cljs"]
                     :compiler {:output-to     "resources/public/fatlip.js"
                                :output-dir    "resources/public/out"
                                ;; :preamble      ["react/react.min.js"]
                                ;; :externs       ["react/externs/react.js"]
                                :source-map    true
                                :optimizations :none
                                :cache-analysis true}}
               :test {:source-paths ["src/cljs" "test/cljs"]
                      :notify-command ["phantomjs" :cljs.test/runner "target/test.js"]
                      :compiler {:output-to     "target/test.js"
                                 :optimizations :whitespace
                                 :pretty-print true}}}})
