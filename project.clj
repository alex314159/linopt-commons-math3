(defproject net.clojars.alex314159/linopt-commons-math3 "0.1.0-SNAPSHOT"
  :description "Generic linear-programming portfolio optimiser: an item universe + composable constraint builders over Apache Commons Math 3's simplex solver."
  :url "https://github.com/alex314159/linopt-commons-math3"
  :license {:name "MIT"
            :url  "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [org.apache.commons/commons-math3 "3.6.1"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :repl-options {:timeout 240000}
  ;; Benchmarks only — :bench is outside Leiningen's default profile chain, so
  ;; criterium and dev/ never reach the released jar. Run with: lein bench
  :profiles {:bench {:dependencies [[criterium "0.4.6"]]
                     :source-paths ["dev"]}}
  :aliases {"bench" ["with-profile" "+bench" "run" "-m" "linopt-commons-math3.bench"]})
