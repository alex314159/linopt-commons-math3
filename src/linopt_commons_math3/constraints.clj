(ns linopt-commons-math3.constraints
  "Generic constraint builders over a `universe` (a seq of items - maps, or
  anything `get` works on - in the same order as the LP variables they
  represent).

  Every builder returns a FLAT sequence: `linopt-commons-math3.solver
  /linear-optimization` reads its `constraints` argument as one flat run of
  `coeffs relation rhs coeffs relation rhs ...` (it groups every 3 elements
  itself). Builder outputs are safe to `concat` together directly - flat
  seqs concatenated stay flat, so composing several constraints needs no
  extra flattening step.

  Nothing here knows what an item represents - callers supply the key/column
  names and grouping/weighting functions. Domain rules (rating buckets,
  issuer tiers, benchmark composition, ...) are composed from these."
  (:require [linopt-commons-math3.solver :as solver]))

(defn position-bounds
  "Per-item `[min max]` bound on each of the `n` variables. `mn`/`mx` can be
  `nil` to skip that side - e.g. pass `nil` for `mn` when the solver already
  gets `:non-negative? true`, so a redundant `>= 0` row (and its `n`-long
  coefficient array) is never allocated instead of being built and later
  dropped by the solver."
  [n mn mx]
  (vec (mapcat (fn [i]
                 (cond-> []
                   mn (into [(solver/bv n i 1.0) :>= mn])
                   mx (into [(solver/bv n i 1.0) :<= mx])))
               (range n))))

(defn sum-constraint
  "Sum of all `n` variables equals `target`."
  [n target]
  [(double-array n 1.0) := (double target)])

(defn group-constraint
  "Band constraint `[min max]` on the summed weight of items where
  `(get item group-key)` equals `target-value`."
  [universe group-key target-value [mn mx]]
  (let [coll (mapv #(if (= (get % group-key) target-value) 1.0 0.0) universe)]
    [coll :<= (double mx)
     coll :>= (double mn)]))

(defn value-band-constraint
  "Band constraint `[min max]` on the sum of `(get item value-key)` across
  `universe` (e.g. a weight-weighted duration/beta, where each item's LP
  coefficient IS its per-unit value)."
  [universe value-key [mn mx]]
  (let [coll (mapv #(get % value-key) universe)]
    [coll :<= (double mx)
     coll :>= (double mn)]))

(defn predicate-constraint
  "Single one-sided constraint on the summed weight of items matching `pred`
  (an arbitrary item predicate): `sum(weight_i : pred(item_i)) rel target`.
  Generalizes rating-bucket / flag-style caps that `group-constraint` can't
  express because membership isn't a single equality check. For a band on
  both sides, concat two calls (one `:<=`, one `:>=`)."
  [universe pred rel target]
  [(mapv #(if (pred %) 1.0 0.0) universe) rel (double target)])

(defn grouped-cap-constraint
  "Per-group upper bound, e.g. an issuer/ticker concentration limit where the
  cap itself depends on the group (rating, sector, ...).

  `group-key-fn` maps an item to its group. `cap-fn` receives `[group items]`
  (all items in that group) and returns that group's max weight, or nil to
  skip the constraint entirely for that group."
  [universe group-key-fn cap-fn]
  (vec (mapcat (fn [[group items]]
                 (when-let [cap (cap-fn group items)]
                   (let [group-set (set items)]
                     [(mapv #(if (contains? group-set %) 1.0 0.0) universe)
                      :<=
                      (double cap)])))
               (group-by group-key-fn universe))))

(defn benchmark-weights-by
  "Sum `weight-fn` over `bench-coll` (the reference/true-index universe),
  grouped by `group-key-fn`. Feeds `benchmark-relative-group-constraint`."
  [bench-coll weight-fn group-key-fn]
  (update-vals (group-by group-key-fn bench-coll)
               (fn [items] (reduce + 0.0 (map weight-fn items)))))

(defn benchmark-relative-group-constraint
  "Per-group `±ow-uw` band around each group's benchmark weight. The LP
  coefficient vectors run over `universe-coll` (the investable/LP variables);
  the benchmark weight for each group is computed over `bench-coll` (the
  reference universe, via `weight-fn`/`group-key-fn`). Only groups that have
  at least one investable item are constrained - a group with benchmark
  weight but no investable item can't be held, so a positive lower bound for
  it would be infeasible."
  [universe-coll bench-coll weight-fn group-key-fn ow-uw]
  (when ow-uw
    (let [bench-weights (benchmark-weights-by bench-coll weight-fn group-key-fn)]
      (vec (mapcat (fn [group]
                     (let [bw (get bench-weights group 0.0)]
                       (group-constraint universe-coll group-key-fn group [(- bw ow-uw) (+ bw ow-uw)])))
                   (distinct (map group-key-fn universe-coll)))))))

(defn benchmark-relative-average-constraint
  "`±ow-uw` band around the benchmark's weight-weighted average of `value-fn`
  (e.g. duration, beta). `bench-coll`/`weight-fn` compute the benchmark
  average; the LP coefficients are `value-fn` applied to `universe-coll`.

  The LP variables are weights on some total scale (e.g. 0-100 NAV%, or the
  invested 100-minus-cash if some weight is held back); `total-weight` is
  that scale (default `1.0`, i.e. weights already sum to 1), needed because
  `sum(value_i * weight_i) = bench-avg * total-weight` at the true benchmark
  average, not `bench-avg` itself."
  ([universe-coll bench-coll weight-fn value-fn ow-uw]
   (benchmark-relative-average-constraint universe-coll bench-coll weight-fn value-fn ow-uw 1.0))
  ([universe-coll bench-coll weight-fn value-fn ow-uw total-weight]
   (let [bws (mapv weight-fn bench-coll)
         vals* (mapv value-fn bench-coll)
         sum-bw (reduce + 0.0 bws)
         sum-bwv (reduce + 0.0 (map * bws vals*))
         bench-avg (if (pos? sum-bw) (/ sum-bwv sum-bw) 0.0)
         coll (mapv value-fn universe-coll)]
     [coll :<= (* total-weight (+ bench-avg ow-uw))
      coll :>= (* total-weight (max 0.0 (- bench-avg ow-uw)))])))
