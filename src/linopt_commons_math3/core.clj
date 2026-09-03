(ns linopt-commons-math3.core
  "Public API — re-exports all sub-namespaces for single-ns consumers.

  Aliases here carry the defining var's :doc, :arglists, :file and :line, so
  (doc linopt-commons-math3.core/foo) shows the real documentation and
  REPL-driven editor hover (Calva, CIDER) works. Static-only analysis
  (clj-kondo, Cursive's offline index) does not expand this macro and will
  show less — require the defining namespace directly if that is how your
  editor resolves symbols."
  (:require [linopt-commons-math3.solver :as solver]
            [linopt-commons-math3.universe :as universe]
            [linopt-commons-math3.constraints :as constraints]))

(defmacro ^:private defalias
  "def sym as an alias for another namespace's var, copying the metadata that
  documentation tooling reads. Without this a plain (def a b) alias has no
  :doc and no :arglists, so callers see nothing useful."
  [sym target]
  `(let [target# (var ~target)]
     (alter-meta! (def ~sym @target#)
                  merge
                  (select-keys (meta target#) [:doc :arglists :file :line :column]))
     (var ~sym)))

;;; solver
(defalias linear-optimization              solver/linear-optimization)
(defalias linear-optimization-with-abs-sum solver/linear-optimization-with-abs-sum)
(defalias linear-optimization-generic      solver/linear-optimization-generic)
(defalias maximize-objective               solver/maximize-objective)
(defalias bv                                solver/bv)
(defalias seq->double-array                 solver/seq->double-array)

;;; universe
(defalias range-cut          universe/range-cut)
(defalias range-cut-all       universe/range-cut-all)
(defalias exclude-values      universe/exclude-values)
(defalias exclude-values-all  universe/exclude-values-all)
(defalias keep-values         universe/keep-values)
(defalias require-numeric     universe/require-numeric)

;;; constraints
(defalias position-bounds                     constraints/position-bounds)
(defalias sum-constraint                      constraints/sum-constraint)
(defalias group-constraint                    constraints/group-constraint)
(defalias value-band-constraint               constraints/value-band-constraint)
(defalias predicate-constraint                 constraints/predicate-constraint)
(defalias grouped-cap-constraint              constraints/grouped-cap-constraint)
(defalias benchmark-weights-by                constraints/benchmark-weights-by)
(defalias benchmark-relative-group-constraint    constraints/benchmark-relative-group-constraint)
(defalias benchmark-relative-average-constraint  constraints/benchmark-relative-average-constraint)
