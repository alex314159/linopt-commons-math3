(ns linopt-commons-math3.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [linopt-commons-math3.solver :as solver]
            [linopt-commons-math3.universe :as universe]
            [linopt-commons-math3.constraints :as constraints]
            [linopt-commons-math3.core :as core]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-6))

;;; solver

(deftest linear-optimization-tests
  (testing "the textbook example from the docstring"
    (let [[point value] (solver/linear-optimization [-1 4 0]
                                                    [[-3 1] :<= 6
                                                     [-1 -2] :>= -4
                                                     [0 1] :>= -3])]
      (is (close? 10.0 (first point)))
      (is (close? -3.0 (second point)))
      (is (close? -22.0 value)))))

(deftest linear-optimization-with-abs-sum-tests
  (testing "allows short positions bounded by sum-of-abs-weights"
    (let [[solution value] (solver/linear-optimization-with-abs-sum
                            [1.0 2.0 0.0]
                            [[1.0 1.0] := 100.0]
                            120.0
                            {:goal :maximize})]
      ;; x1 + x2 = 100, |x1| + |x2| = 120 => x1 = -10, x2 = 110 maximizes x1 + 2x2
      (is (close? -10.0 (first solution)))
      (is (close? 110.0 (second solution)))
      (is (close? 210.0 value)))))

(deftest maximize-objective-tests
  (testing "solves directly over a universe of maps, forcing :maximize regardless of options"
    (let [universe [{:v 1.0} {:v 2.0}]
          ;; x1 <= 5, x2 <= 5, x1 + x2 = 6; maximize x1 + 2*x2 -> push weight to x2
          constraints [[1.0 0.0] :<= 5.0
                       [0.0 1.0] :<= 5.0
                       [1.0 1.0] := 6.0]
          [point value] (solver/maximize-objective universe :v constraints {:non-negative? true})]
      (is (close? 1.0 (first point)))
      (is (close? 5.0 (second point)))
      (is (close? 11.0 value)))))

;;; universe

(deftest universe-tests
  (let [u [{:id "A" :country "BR" :v 1.0 :dur 3.0}
           {:id "B" :country "MX" :v 5.0 :dur 3.0}
           {:id "C" :country "BR" :v 3.0 :dur 9.0}]]
    (testing "range-cut keeps items within [min max]"
      (is (= ["A" "C"] (map :id (universe/range-cut u :v [1.0 3.0])))))

    (testing "range-cut-all applies several numeric cuts in sequence"
      (is (= ["A"] (map :id (universe/range-cut-all u [[:v 1.0 3.0] [:dur 0.0 5.0]])))))

    (testing "exclude-values drops matching items, no-op on empty exclusion set"
      (is (= ["B"] (map :id (universe/exclude-values u :country ["BR"]))))
      (is (= ["A" "B" "C"] (map :id (universe/exclude-values u :country [])))))

    (testing "keep-values restricts to a membership set"
      (is (= ["A" "C"] (map :id (universe/keep-values u :country ["BR"])))))

    (testing "require-numeric drops nil/NaN"
      (let [nu [{:id "A" :v 1.0} {:id "B" :v Double/NaN} {:id "C" :v nil}]]
        (is (= ["A"] (map :id (universe/require-numeric nu [:v]))))))))

;;; constraints

(deftest position-bounds-tests
  (testing "one [min max] pair (2 flat constraints, 6 elements each triplet-of-3) per variable"
    (is (= 4 (count (partition 3 (constraints/position-bounds 2 0.0 5.0)))))))

(deftest sum-constraint-tests
  (testing "a single flat [coeffs relation rhs] constraint over all variables"
    (let [[coll rel target] (constraints/sum-constraint 3 100.0)]
      (is (= [1.0 1.0 1.0] (vec coll)))
      (is (= := rel))
      (is (= 100.0 target)))))

(deftest group-constraint-tests
  (let [u [{:g "A"} {:g "B"} {:g "A"}]]
    (testing "indicator vector selects group membership; result is flat, concat-safe"
      (let [[le _ hi coll2 _ ge] (constraints/group-constraint u :g "A" [1.0 10.0])]
        (is (= [1.0 0.0 1.0] (vec le)))
        (is (= le coll2))
        (is (= 10.0 hi))
        (is (= 1.0 ge))))))

(deftest value-band-constraint-tests
  (let [u [{:d 2.0} {:d 5.0}]]
    (testing "coefficients are the raw value, not an indicator"
      (let [[coll] (constraints/value-band-constraint u :d [3.0 6.0])]
        (is (= [2.0 5.0] (vec coll)))))))

(deftest predicate-constraint-tests
  (let [u [{:r 12} {:r 8} {:r 17}]]
    (testing "membership by arbitrary predicate, not equality; single one-sided constraint"
      (let [[coll rel target] (constraints/predicate-constraint u #(> (:r %) 10) :<= 5.0)]
        (is (= [1.0 0.0 1.0] (vec coll)))
        (is (= :<= rel))
        (is (= 5.0 target))))))

(deftest grouped-cap-constraint-tests
  (let [u [{:t "X" :bucket :ig} {:t "Y" :bucket :hy} {:t "Z" :bucket :ig}]]
    (testing "cap depends on the group; nil cap skips that group; result is one flat constraint"
      (let [row (constraints/grouped-cap-constraint u :bucket
                                                    (fn [g _] (when (= g :ig) 3.0)))]
        (is (= 3 (count row)))
        (let [[coll rel cap] row]
          (is (= [1.0 0.0 1.0] (vec coll)))
          (is (= :<= rel))
          (is (= 3.0 cap)))))))

(deftest benchmark-relative-group-constraint-tests
  (let [bench [{:c "BR" :w 10.0} {:c "MX" :w 5.0} {:c "AR" :w 2.0}]
        universe [{:c "BR"} {:c "MX"}]] ;; AR isn't investable
    (testing "only groups present in the investable universe are constrained"
      (let [flat (constraints/benchmark-relative-group-constraint universe bench :w :c 1.0)
            triplets (partition 3 flat)]
        (is (= 4 (count triplets))) ;; 2 groups x 2 (upper/lower) rows, flattened
        (is (every? #(not= "AR" %) (map second triplets)))))))

(deftest benchmark-relative-average-constraint-tests
  (let [bench [{:dur 4.0 :w 60.0} {:dur 8.0 :w 40.0}] ;; weighted avg dur = 5.6
        universe [{:dur 4.0} {:dur 8.0}]]
    (testing "band is centered on the true benchmark weighted average, scaled by total-weight"
      (let [[_ _ hi _ _ lo] (constraints/benchmark-relative-average-constraint universe bench :w :dur 0.5 100.0)]
        (is (close? 610.0 hi))
        (is (close? 510.0 lo))))))

;;; core — the single-ns re-export facade

(def impl-namespaces
  '[linopt-commons-math3.solver linopt-commons-math3.universe linopt-commons-math3.constraints])

(deftest core-api-tests
  (testing "every re-exported var carries arglists"
    (let [publics (ns-publics 'linopt-commons-math3.core)]
      (is (seq publics))
      (doseq [[sym v] publics]
        (is (seq (:arglists (meta v))) (str sym " lost its arglists")))))

  (testing "core re-exports every public function of every impl namespace"
    (let [exported (set (map deref (vals (ns-publics 'linopt-commons-math3.core))))]
      (doseq [ns-sym impl-namespaces
              [sym v] (ns-publics ns-sym)
              :when (fn? @v)]
        (is (contains? exported @v)
            (str ns-sym "/" sym " is not re-exported by linopt-commons-math3.core")))))

  (testing "aliases call through to the same implementation"
    (is (= (universe/range-cut [{:v 1}] :v [0 5]) (core/range-cut [{:v 1}] :v [0 5])))))
