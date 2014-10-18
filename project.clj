(defproject fatlip "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src/clj" "src/cljs"]

  :dependencies [[com.cemerick/austin "0.1.5"]
                 [com.cemerick/piggieback "0.1.3"]
                 [compojure "1.2.0"]
                 [enlive "1.1.5"]
                 [environ "1.0.0"]
                 [figwheel "0.1.4-SNAPSHOT"]
                 [om "0.7.3"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2371" :scope "provided"]
                 [org.clojure/core.rrb-vector "0.0.11"]
                 [ring "1.3.1"]
                 [weasel "0.4.0-SNAPSHOT"]]

  :plugins [[com.cemerick/clojurescript.test "0.3.1"]
            [lein-cljsbuild "1.0.3"]
            [lein-environ "1.0.0"]]

  :min-lein-version "2.0.0"

  :uberjar-name "fatlip.jar"

  :cljsbuild {:builds
              {:dev {:source-paths ["src/cljs"]
                     :compiler {:output-to     "resources/public/fatlip.js"
                                :output-dir    "resources/public/out"
                                :source-map    "resources/public/fatlip.js.map"
                                :preamble      ["react/react.min.js"]
                                :externs       ["react/externs/react.js"]
                                :optimizations :none
                                :pretty-print  true}}
               :test {:source-paths ["src/cljs" "test/cljs"]
                      :notify-command ["phantomjs" :cljs.test/runner "target/test.js"]
                      :compiler {:output-to     "target/test.js"
                                 :optimizations :simple
                                 :pretty-print true}}}}

  :profiles {:dev {:repl-options {:init-ns fatlip.server
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   :plugins [[lein-figwheel "0.1.4-SNAPSHOT"]
                             [com.cemerick/austin "0.1.5"]]
                   :figwheel {:http-server-root "public"
                              :port 3449}
                   :env {:is-dev true}}

             :uberjar {:hooks [leiningen.cljsbuild]
                       :env {:production true}
                       :omit-source true
                       :aot :all
                       :cljsbuild {:builds {:app
                                            {:compiler
                                             {:optimizations :advanced
                                              :pretty-print false}}}}}})
