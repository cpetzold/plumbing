(defproject prismatic/plumbing "0.2.3-SNAPSHOT"
  :description "Prismatic's Clojure utility belt."
  :url "https://github.com/Prismatic/plumbing"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}

  :dependencies [[prismatic/schema "0.2.1"]
                 [de.kotka/lazymap "3.1.0" :exclusions [org.clojure/clojure]]]

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.5.1"]
                                  [org.clojure/clojurescript "0.0-2202"]
                                  [com.keminglabs/cljx "0.3.2"]]
                   :plugins [[com.keminglabs/cljx "0.3.2"]
                             [lein-cljsbuild "0.3.2"]
                             [com.cemerick/austin "0.1.3"]
                             [com.cemerick/clojurescript.test "0.2.2"]]
                   :hooks [cljx.hooks #_leiningen.cljsbuild]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl
                                                     cljx.repl-middleware/wrap-cljx]}
                   :cljx {:builds [{:source-paths ["src"]
                                    :output-path "target/generated/src/clj"
                                    :rules :clj}
                                   {:source-paths ["src"]
                                    :output-path "target/generated/src/cljs"
                                    :rules :cljs}
                                   {:source-paths ["test"]
                                    :output-path "target/generated/test/clj"
                                    :rules :clj}
                                   {:source-paths ["test"]
                                    :output-path "target/generated/test/cljs"
                                    :rules :cljs}]}}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0-RC1"]]}}

  :aliases {"all" ["with-profile" "dev:dev,1.6"]}

  :lein-release {:deploy-via :shell
                 :shell ["lein" "deploy" "clojars"]}

  :prep-tasks ["cljx" "javac" "compile"]

  :source-paths ["target/generated/src/clj" "src"]

  :resource-paths ["target/generated/src/cljs"]

  :test-paths ["target/generated/test/clj" "test"]

  :cljsbuild {:test-commands {"unit" ["phantomjs" :runner
                                      "this.literal_js_was_evaluated=true"
                                      "target/unit-test.js"]}
              :builds
              {:dev {:source-paths ["src"
                                    "target/generated/src/clj"
                                    "target/generated/src/cljs"]
                     :compiler {:output-to "target/main.js"
                                :optimizations :whitespace
                                :pretty-print true}}
               :test {:source-paths ["src"
                                     "target/generated/src/clj"
                                     "target/generated/src/cljs"
                                     "target/generated/test/clj"
                                     "target/generated/test/cljs"]
                      :compiler {:output-to "target/unit-test.js"
                                 :optimizations :whitespace

                                 :pretty-print true}}}}

  :jvm-opts ^:replace [])
