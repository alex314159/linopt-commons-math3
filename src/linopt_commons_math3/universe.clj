(ns linopt-commons-math3.universe
  "Generic universe-construction helpers: narrowing down a collection of
  candidate items (maps, or anything `get` works on) before constraints are
  built. Nothing here knows what an item represents - callers supply the
  key/column names.")

(defn range-cut
  "Keep only items where `(get item k)` falls within `[min max]` inclusive.
  Uses `compare`, not `<=`, so it works for any Comparable (numbers, strings,
  dates, ...), not just numbers."
  [universe k [mn mx]]
  (filter #(let [v (get % k)] (and (<= (compare mn v) 0) (<= (compare v mx) 0))) universe))

(defn range-cut-all
  "Apply a sequence of `[k min max]` range cuts in turn."
  [universe cuts]
  (reduce (fn [u [k mn mx]] (range-cut u k [mn mx])) universe cuts))

(defn exclude-values
  "Drop items whose `(get item k)` is in `excluded` (a collection or set)."
  [universe k excluded]
  (if (seq excluded)
    (let [excluded (set excluded)]
      (remove #(contains? excluded (get % k)) universe))
    universe))

(defn exclude-values-all
  "Apply `exclude-values` for each `[k excluded]` pair."
  [universe exclusions]
  (reduce (fn [u [k excluded]] (exclude-values u k excluded)) universe exclusions))

(defn keep-values
  "Keep only items whose `(get item k)` is in `included` (a collection or set) -
  e.g. restrict a universe to index constituents or currently-held names."
  [universe k included]
  (let [included (set included)]
    (filter #(contains? included (get % k)) universe)))

(defn require-numeric
  "Drop items where `(get item k)` is missing or NaN, for each `k` in `ks`.
  Useful before an optimisation target or risk constraint that needs a real
  number for every surviving item."
  [universe ks]
  (reduce (fn [u k]
            (remove #(let [v (get % k)]
                       (or (nil? v) (and (number? v) (Double/isNaN (double v)))))
                    u))
          universe ks))
