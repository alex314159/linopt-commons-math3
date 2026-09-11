# linopt-commons-math3

A generic linear-programming portfolio optimiser: an item universe + composable
constraint builders, over [Apache Commons Math 3](https://commons.apache.org/proper/commons-math/)'s
simplex solver.

## Why

Building an optimised portfolio is always the same two steps: decide **what universe
you're choosing from** (a filtered list of candidates), then decide **what constraints
apply to it** (position bounds, group caps, benchmark-relative bands, ...) before handing
both to a linear solver. None of that is specific to bonds, equities or any other asset
class - it takes a seq of maps and some key names.

Same split as [svm-techml-smile](https://github.com/alex314159/svm-techml-smile) and
[var-techml-neanderthal](https://github.com/alex314159/var-techml-neanderthal): the generic
technique lives here, the domain wiring (what a "country" or a "rating bucket" means, where
the candidate data comes from) stays in the app.

## Install

```clojure
[net.clojars.alex314159/linopt-commons-math3 "0.1.0"]
```

## Namespaces

| ns | what's in it |
|----|--------------|
| `linopt-commons-math3.core` | re-exports everything below — require this one ns if you don't care about the split |
| `linopt-commons-math3.solver` | the LP engine: `linear-optimization`, `linear-optimization-with-abs-sum` (long/short via split positive/negative parts), `maximize-objective` |
| `linopt-commons-math3.universe` | narrowing a candidate collection: `range-cut`, `exclude-values`, `keep-values`, `require-numeric` |
| `linopt-commons-math3.constraints` | building LP constraints over a universe: per-item bounds, group/predicate constraints, issuer-style grouped caps, benchmark-relative bands |

## Shape of a problem

Every constraint builder returns a **flat** sequence (`coeffs relation rhs coeffs relation rhs
...`, matching what `linear-optimization` reads directly) - concatenating several builders'
output together is always safe, no separate flattening step needed.

```clojure
(require '[linopt-commons-math3.core :as lo])

;; universe: a seq of maps, in the order the LP variables will represent them
(def universe
  [{:id "A" :country "BR" :yield 8.1} {:id "B" :country "BR" :yield 7.4}
   {:id "C" :country "MX" :yield 6.9} {:id "D" :country "MX" :yield 5.2}])
(def n (count universe))

(def constraints
  (vec (concat (lo/position-bounds n nil 40.0)               ; no name over 40%
               (lo/sum-constraint n 100.0)                    ; fully invested
               (lo/group-constraint universe :country "BR" [0.0 60.0])))) ; BR <= 60%

(lo/maximize-objective universe :yield constraints {:non-negative? true})
;; => [(40.0 20.0 40.0 0.0) 748.0]  (point, objective value)
```

`position-bounds`' lower bound is `nil` here because `:non-negative? true` already
forces every variable `>= 0` - passing `0.0` instead still works (the solver drops
the now-redundant row before it reaches the simplex), but `nil` skips allocating
it in the first place.

Benchmark-relative bands (over/under-weight vs an index) anchor to a separate reference
collection (the *true* index, before any investability screen) so a name that's in the
benchmark but not investable doesn't make the constraint infeasible:

```clojure
(lo/benchmark-relative-group-constraint universe bench-universe :bm-weight :country 5.0)
;; -> per-country ±5% band around each country's benchmark weight
```

## Development

```
lein test
lein bench    # criterium, :bench profile only — never in the released jar
```
