(ns linopt-commons-math3.solver
  "Thin Clojure wrapper around Apache Commons Math 3's simplex LP solver.
  Nothing here knows what a variable represents - it takes coefficient
  vectors and constraint triplets and returns a point + objective value."
  (:import [org.apache.commons.math3.optim.nonlinear.scalar GoalType MultivariateFunctionMappingAdapter]
           [org.apache.commons.math3.optim.univariate UnivariatePointValuePair]
           [org.apache.commons.math3.optim BaseOptimizer OptimizationData MaxIter PointValuePair]
           [org.apache.commons.math3.optim.linear LinearObjectiveFunction LinearConstraint
            Relationship LinearConstraintSet SimplexSolver NonNegativeConstraint PivotSelectionRule]
           (java.util Collection)))

(def ^:const double-array-type (Class/forName "[D"))

(defn seq->double-array
  "Convert sequence to double array. Returns input if `vs` is double array already."
  ^doubles [vs]
  (cond
    (= (type vs) double-array-type) vs
    (nil? vs) nil
    (seqable? vs) (double-array vs)
    :else (let [arr (double-array 1)]
            (aset arr 0 (double vs))
            arr)))

(defn- constraint-relations
  ^Relationship [relation]
  (cond
    (#{:>= '>= :geq} relation) Relationship/GEQ
    (#{:<= '<= :leq} relation) Relationship/LEQ
    :else Relationship/EQ))

(defn- build-constraint
  ^LinearConstraint [[left relation right]]
  (let [relationship (constraint-relations relation)]
    (if (number? right)
      (LinearConstraint. (seq->double-array left) relationship (double right))
      (LinearConstraint. (seq->double-array (butlast left))
                         (double (last left))
                         relationship
                         (seq->double-array (butlast right))
                         (double (last right))))))

(defn- nonneg-coeffs?
  "True if every coefficient in `left` is >= 0. `left` is a `double-array` for
  most per-variable constraints (`position-bounds` et al build them via `bv`);
  `seq`-ing a double-array boxes every element just to iterate, which shows up
  at scale, so that shape gets a primitive loop instead of `every?`."
  [left]
  (if (= (type left) double-array-type)
    (let [^doubles a left]
      (loop [i 0]
        (or (= i (alength a))
            (and (>= (aget a i) 0.0) (recur (inc i))))))
    (every? #(>= (double %) 0.0) left)))

(defn- redundant-non-negative-floor?
  "True if `[left rel right]` is an always-true `>= r` floor (`r <= 0`, every
  coefficient in `left` non-negative) once every variable is already
  constrained `>= 0` - safe to drop instead of handing the solver an extra
  artificial-variable row for a constraint that can never bind.
  `position-bounds`/`group-constraint` emit exactly this shape whenever their
  lower band is 0, the common case for portfolio weights - each such row
  roughly doubles simplex iterations at scale, so this matters."
  [[left relation right]]
  (and (number? right)
       (<= (double right) 0.0)
       (#{:>= '>= :geq} relation)
       (nonneg-coeffs? left)))

(defn- maybe-stats?
  [stats? ^BaseOptimizer optimizer res]
  (if-not stats?
    res
    {:result res
     :evaluations (.getEvaluations optimizer)
     :iterations (.getIterations optimizer)}))

(defn- parse-result
  ([res] (parse-result nil res))
  ([^MultivariateFunctionMappingAdapter mfma res]
   (condp instance? res
     UnivariatePointValuePair (let [^UnivariatePointValuePair res res]
                                [(list (.getPoint res)) (.getValue res)])
     PointValuePair (let [^PointValuePair res res]
                      [(seq (if mfma
                              (.unboundedToBounded mfma (.getPointRef res))
                              (.getPointRef res))) (.getValue res)])
     res)))

(defn linear-optimization
  "Solves a linear problem.

   Target is defined as a vector of coefficients and constant as the last value:
   `[a1 a2 a3 ... c]` means `f(x1,x2,x3) = a1*x1 + a2*x2 + a3*x3 + ... +  c`

   Constraints are defined as a sequence of one of the following triplets:

   * `[a1 a2 a3 ...] R n` - which means `a1*x1+a2*x2+a3*x3+... R n`
   * `[a1 a2 a3 ... ca] R [b1 b2 b3 ... cb]` - which means `a1*x1+a2*x2+a3*x3+...+ca R b1*x1+b2*x2+b3*x3+...+cb`
   where `R` is a relationship and can be one of `<=`, `>=` or `=` as symbol or keyword. Also `:leq`, `:geq` and `:eq` are valid.

  Function returns pair of optimal point and function value. If `stat?` option is set to true, returns also information about number of iterations.

  Possible options:

  * `:goal` - `:minimize` (default) or `:maximize`
  * `:rule` - pivot selection rule, `:dantzig` (default) or `:bland`
  * `:max-iter` - maximum number of iterations, maximum integer by default
  * `:non-negative?` - allow non-negative variables only, default: `false`
  * `:epsilon` - convergence value, default: `1.0e-6`:
  * `:max-ulps` - floating point comparisons, default: `10` ulp
  * `:cut-off` - pivot elements smaller than cut-off are treated as zero, default: `1.0e-10`

  ```clojure
  (linear-optimization [-1 4 0] [[-3 1] :<= 6
                                 [-1 -2] :>= -4
                                 [0 1] :>= -3])
  ;; => [(9.999999999999995 -3.0) -21.999999999999993]
  ```"
  ([target constraints] (linear-optimization target constraints {}))
  ([target constraints {:keys [goal ^double epsilon ^int max-ulps ^double cut-off
                               rule non-negative? ^int max-iter stats?]
                        :or {goal :minimize epsilon 1.0e-6 max-ulps 10 cut-off 1.0e-10
                             rule :dantzig non-negative? false max-iter Integer/MAX_VALUE}}]
   (let [goal (if (= goal :minimize) GoalType/MINIMIZE GoalType/MAXIMIZE)
         rule (if (= rule :dantzig) PivotSelectionRule/DANTZIG PivotSelectionRule/BLAND)
         max-iter (MaxIter. max-iter)
         non-negative-constraint (NonNegativeConstraint. non-negative?)
         target (LinearObjectiveFunction. (seq->double-array (butlast target))
                                          (double (last target)))
         constraints (->> constraints
                          (partition 3)
                          (remove #(and non-negative? (redundant-non-negative-floor? %)))
                          ^Collection (map build-constraint)
                          (LinearConstraintSet.))
         ^BaseOptimizer solver (SimplexSolver. epsilon max-ulps cut-off)]
     (->> [goal rule max-iter non-negative-constraint target constraints]
          (into-array OptimizationData)
          (.optimize solver)
          (parse-result nil)
          (maybe-stats? stats? solver)))))

(defn bv
  "Creates a basis vector: an all-zero double array of length `n` with `w` at index `i`."
  [^long n ^long i ^double w]
  (doto (double-array n 0.0) (aset i w)))

(defn linear-optimization-with-abs-sum
  "Linear optimization with sum of absolute values constraint.

  For each original variable x_i, we introduce two non-negative variables:
  - x_i+ (positive part)
  - x_i- (negative part)

  Where: x_i = x_i+ - x_i-  and  |x_i| = x_i+ + x_i-

  Parameters:
  - target: vector of coefficients for objective function [a1 a2 ... c]
  - constraints: standard linear constraints (will be transformed, should include sum-of-weights)
  - sum-of-abs-weights: target for sum of absolute weights (must be positive)
  - options: same as linear-optimization

  Example:
  (linear-optimization-with-abs-sum
    [-1 4 0]  ; objective coefficients
    [[-3 1] :<= 6   ; other constraints
     [1 1] := 100]  ; sum of weights = 100
    120.0  ; sum of absolute weights
    {})"
  ([target constraints sum-of-abs-weights]
   (linear-optimization-with-abs-sum target constraints sum-of-abs-weights {}))
  ([target constraints sum-of-abs-weights options]
   (let [n (dec (count target))  ; number of original variables (excluding constant)
         constant (last target)
         coeffs (vec (butlast target))

         ;; New target: [c1 -c1 c2 -c2 ... cn -cn constant]
         ;; For x_i = x_i+ - x_i-, we need coefficients: c_i for x_i+ and -c_i for x_i-
         new-target (vec (concat (mapcat #(vector % (- %)) coeffs) [constant]))

         ;; Transform existing constraints
         ;; Each constraint [a1 a2 ... an] R b becomes [a1 -a1 a2 -a2 ... an -an] R b
         transformed-constraints
         (vec (mapcat
               (fn [[left relation right]]
                 (let [new-left (vec (mapcat #(vector % (- %)) left))]
                   [[new-left relation right]]))
               (partition 3 constraints)))

         ;; Add sum of absolute weights constraint: sum(x_i+ + x_i-) = sum-of-abs-weights
         ;; This is: [1 1 1 1 ... 1 1] = sum-of-abs-weights
         abs-sum-constraint [[(repeat (* 2 n) 1.0) := sum-of-abs-weights]]

         ;; Add non-negativity constraints for all auxiliary variables
         ;; Each x_i+ >= 0 and x_i- >= 0
         non-negativity-constraints (vec (mapcat (fn [i] [[(bv (* 2 n) i 1.0) :>= 0.0]]) (range (* 2 n))))

         ;; Combine all constraints
         all-constraints (vec (concat transformed-constraints
                                      abs-sum-constraint
                                      non-negativity-constraints))

         ;; Don't use non-negative? option as we've added explicit constraints
         updated-options (assoc options :non-negative? false)

         ;; Solve the transformed problem
         result (linear-optimization new-target (apply concat all-constraints) updated-options)

         ;; Transform result back: x_i = x_i+ - x_i-
         [solution value] result
         original-solution (mapv (fn [[plus minus]] (- plus minus))
                                 (partition 2 solution))]

     [original-solution value])))

(defn linear-optimization-generic
  "Generic linear optimization that supports optional absolute sum constraint
  (pass `:sum-of-abs-weights` in options to allow short positions)."
  ([target constraints] (linear-optimization-generic target constraints {}))
  ([target constraints options]
   (if-let [sum-abs (:sum-of-abs-weights options)]
     (linear-optimization-with-abs-sum target constraints sum-abs options)
     (linear-optimization target constraints options))))

(defn maximize-objective
  "Solve for a universe (a seq of items/maps) whose objective coefficients are
  `(get item objective-key)`, maximizing sum(objective) subject to `constraints`.

  `options` are the same as `linear-optimization-generic` (e.g. `:non-negative?`,
  `:sum-of-abs-weights`); `:goal` is forced to `:maximize`."
  ([universe objective-key constraints] (maximize-objective universe objective-key constraints {}))
  ([universe objective-key constraints options]
   (let [target (conj (mapv #(get % objective-key) universe) 0.0)]
     (linear-optimization-generic target constraints (assoc options :goal :maximize)))))
