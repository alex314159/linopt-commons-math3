(ns linopt-commons-math3.bench
  "Benchmarks for the hot paths. Not part of the released jar — lives in the
  :bench profile only. Run with:  lein bench

  The simplex solve itself dominates at realistic universe sizes (a few
  hundred to a couple thousand candidate bonds); the constraint builders are
  cheap by comparison but still worth tracking since a few of them are
  O(n * distinct-groups)."
  (:require [criterium.core :as crit]
            [linopt-commons-math3.solver :as solver]
            [linopt-commons-math3.constraints :as constraints]))

(defn report [label f]
  (println (format "\n=== %s ===" label))
  (crit/quick-bench (f)))

(defn synthetic-universe
  "n items shaped like a cross-sectional bond universe: a rating bucket,
  a country, and a value to optimize on."
  [n]
  (vec (for [i (range n)]
         {:id i
          :country (str "C" (mod i 20))
          :bucket (nth [:ig :hy :ccc] (mod i 3))
          :value (Math/sin (double i))})))

(defn -main [& _]
  (let [n 1000
        universe (synthetic-universe n)
        base-constraints (vec (concat (constraints/position-bounds n 0.0 5.0)
                                      (constraints/sum-constraint n 100.0)))]
    (println "universe size:" n)
    (report "solver/linear-optimization (position bounds + sum-of-weights only)"
            #(solver/linear-optimization (conj (mapv :value universe) 0.0) base-constraints
                                         {:goal :maximize :non-negative? true}))
    (report "constraints/position-bounds" #(constraints/position-bounds n 0.0 5.0))
    (report "constraints/group-constraint (by country)"
            #(doseq [c (distinct (map :country universe))]
               (constraints/group-constraint universe :country c [0.0 10.0])))
    (report "constraints/grouped-cap-constraint (by bucket)"
            #(constraints/grouped-cap-constraint universe :bucket (fn [_ _] 40.0)))
    (shutdown-agents)))
