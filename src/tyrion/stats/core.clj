(ns tyrion.stats.core
  (:require
    [clojure.core.matrix.dataset :as ds]
    [clojure.core.matrix :as mat]
    [tyrion.utils :refer [column-index column-vals]]
    [tyrion.math :refer :all]))

;; Functions related to mean

;; Implementations for mean

(defn- nmean-ds
  [ks coll]
  (->> (-> (comp (map #(column-vals coll %))
                 (map #(/ (mat/esum %) (mat/row-count %) 1.0)))
           (sequence ks))
       (zipmap ks)))

(defn- nmean-mat
  [ks coll]
  (-> (comp (map #(mat/get-column coll %))
            (map #(/ (mat/esum %) (mat/row-count %) 1.0)))
      (sequence ks)
      vec))

(defn- nmean-maps
  [ks coll]
  (->> (map #(select-keys % ks) coll)
       (reduce #(merge-with + % %2))
       (merge-with * (zipmap ks (repeat (count ks) (/ 1.0 (count coll)))))))

(defn mean
  "Returns the mean of a collection."
  [coll]
  (/ (reduce + coll) (count coll) 1.0))

(defn nmean
  "Returns the means of selected keys in a collection.
  Coll can be a list/vector of numbers or list of maps, dataset or matrix."
  [ks coll]
  (cond (ds/dataset? coll) (nmean-ds ks coll)
        (mat/matrix? coll) (nmean-mat ks coll)
        :else (nmean-maps ks coll)))

;; Functions related to frequencies

;; Implementations details for freq
(defn- nfreq-impl-maps
  [ks maps]
  (->> (for [k ks]
         (->> (map #(get % k) maps)
              (frequencies)))
       (zipmap ks)))

(defn- nfreq-impl-ds
  [cols ds]
  (->> (-> (comp (map #(column-vals ds %))
                 (map frequencies))
           (sequence cols))
       (zipmap cols)))

(defn- nfreq-impl-mat
  [cols mats]
  (-> (comp (map #(mat/get-column mats %))
            (map frequencies))
      (sequence cols)
      vec))

(defn freq
  "Returns the result of clojure's frequencies function. "
  [coll]
  (frequencies coll))

(defn nfreq
  "Returns the frequencies of selected keys (ks) in coll."
  [ks coll]
  (cond
    (ds/dataset? coll) (nfreq-impl-ds ks coll)
    (mat/matrix? coll) (nfreq-impl-mat ks coll)
    :else (nfreq-impl-maps ks coll)))

;; Implementations for freq-by

(defn- nfreq-by-impl-ds-f
  [f cols ds]
  (->> (-> (comp (map #(map f (column-vals ds %)))
                 (map frequencies))
           (sequence cols))
       (zipmap cols)))

(defn- nfreq-by-impl-ds-fs
  [fs cols ds]
  (->> (-> (comp (map #(map (get fs %) (column-vals ds %)))
                 (map frequencies))
           (sequence cols))
       (zipmap cols)))

(defn- nfreq-by-impl-maps-fs
  [fs ks maps]
  (->> (for [k ks]
         (->> (map #((get fs k) (get % k)) maps)
              frequencies))
       (zipmap ks)))

(defn- nfreq-by-impl-maps-f
  [f ks maps]
  (->> (for [k ks]
         (->> (map #(f (get % k)) maps)
              frequencies))
       (zipmap ks)))

(defn- nfreq-by-impl-mat-f
  [f cols mats]
  (-> (comp (map #(map f (mat/get-column mats %)))
            (map frequencies))
      (sequence cols)
      vec))

(defn- nfreq-by-impl-mat-fs
  [fs cols mats]
  (-> (comp (map #(map (get fs %) (mat/get-column mats %)))
            (map frequencies))
      (sequence cols)
      vec))

(defn freq-by
  "Returns the frequencies of (map f elmt)."
  [f coll]
  (frequencies (map f coll)))

(defn nfreq-by
  "Returns the frequencies of (map fs elmt) for selected keys (ks) in coll.
  The collection can be either list of maps or core.matrix's dataset.
  The fs can be a single function that applied to all supplied keys
  or it can be a map where the key is each key in keys and the val is the function
  to be applied to each of the element for that key."
  [fs ks coll]
  (cond
    (ds/dataset? coll)
    (if (map? fs)
      (nfreq-by-impl-ds-fs fs ks coll)
      (nfreq-by-impl-ds-f fs ks coll))
    (mat/matrix? coll)
    (if (map? fs)
      (nfreq-by-impl-mat-fs fs ks coll)
      (nfreq-by-impl-mat-f fs ks coll))
    :else
    (if (map? fs)
      (nfreq-by-impl-maps-fs fs ks coll)
      (nfreq-by-impl-maps-f fs ks coll))))

;; Public function for mode

(defn mode
  "Returns the mode and its occurence of a collection of numbers."
  [coll]
  (->> (frequencies coll)
       (apply max-key val)
       key))

(defn nmode
  "Returns the mode of a collection of numbers for n-dimensional data, behaves
  the same as other functions in this namespace."
  [ks coll]
  (let [res (nfreq ks coll)]
    (if (map? res)
      (->> (vals res)
           (map #(->> (apply max-key val %) key))
           (zipmap ks))
      (mapv #(->> (apply max-key val %) key) res))))

;; Functions for mode-by

(defn mode-by
  "Like freq-by, but only returns the element with the highest occurence."
  [f coll]
  (->> (freq-by f coll)
       (apply max-key val)
       key))

(defn nmode-by
  "Like freq-by for selected keys (ks) in coll,
   but only returns the element with the highest occurence."
  [f ks coll]
  (let [raw (nfreq-by f ks coll)
        xform (comp (map #(get raw %))
                    (map #(apply max-key val %))
                    (map key))]
    (if (map? raw)
      (->> (sequence xform ks)
           (zipmap ks))
      (->> (map #(apply max-key val %) raw)
           (mapv key)))))

;; Public function for median


(defn median
  "Returns the median of a collection."
  [coll]
  (let [ctr (if (mat/matrix coll)
              (mat/row-count coll)
              (count coll))
        sorted (sort coll)]
    (if (even? ctr)
      (/ (+ (nth sorted (quot ctr 2))
            (nth sorted (- (quot ctr 2) 1)))
         2.0)
      (nth sorted (dec (quot ctr 2))))))

(defn- nmedian-maps
  [ks coll]
  (->> (for [k ks]
         {k (->> (map #(get % k) coll)
                 median)})
       (reduce merge)))

(defn- nmedian-ds
  [ks coll]
  (->> (-> (comp (map #(column-vals coll %))
                 (map median))
           (sequence ks))
       (zipmap ks)))

(defn- nmedian-mat
  [ks coll]
  (-> (comp (map #(mat/get-column coll %))
            (map median))
      (sequence ks)
      vec))

(defn nmedian
  "Returns the median of a collection for selected keys (ks) in coll."
  [ks coll]
  (cond (ds/dataset? coll) (nmedian-ds ks coll)
        (mat/matrix? coll) (nmedian-mat ks coll)
        :else (nmedian-maps ks coll)))

(defn variance
  "Returns the variance of a collection. When mean is known, it makes the job faster."
  ([coll]
    (let [dmean (mean coll)]
      (/ (transduce (map #(square (- % dmean))) + coll)
         (count coll))))
  ([coll dmean]
    (/ dmean (count coll))))












































