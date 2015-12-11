(ns tyrion.stats.core
  (:require
    [clojure.core.matrix.dataset :as ds]
    [clojure.core.matrix :as mat]
    [tyrion.utils :refer [column-index column-vals]]
    [tyrion.math :refer :all]))

;; Functions related to mean

;; Generic functions for calculation certain fn for n-dimensional data

(defn- generic-ds
  [f ks coll]
  (->> (-> (comp (map #(column-vals coll %))
                 (map f))
           (sequence ks))
       (zipmap ks)))

(defn- generic-mat
  [f ks coll]
  (-> (comp (map #(mat/get-column coll %))
            (map f))
      (sequence ks)
      vec))

(defn- generic-maps
  [f ks coll]
  (->> (-> (comp (map #(map (fn [x] (get x %)) coll))
                 (map f))
           (sequence ks))
       (zipmap ks)))

(defn mean
  "Returns the mean of a collection."
  [coll]
  (/ (reduce + coll) (count coll) 1.0))

(defn nmean
  "Returns the means of selected keys in a collection.
  Coll can be a list/vector of numbers or list of maps, dataset or matrix."
  [ks coll]
  (let [fmean #(/ (mat/esum %) (mat/row-count %) 1.0)]
    (cond (ds/dataset? coll) (generic-ds fmean ks coll)
          (mat/matrix? coll) (generic-mat fmean ks coll)
          :else (generic-maps mean ks coll))))

;; Functions related to frequencies

(defn freq
  "Returns the result of clojure's frequencies function. "
  [coll]
  (frequencies coll))

(defn nfreq
  "Returns the frequencies of selected keys (ks) in coll."
  [ks coll]
  (cond
    (ds/dataset? coll) (generic-ds frequencies ks coll)
    (mat/matrix? coll) (generic-mat frequencies ks coll)
    :else (generic-maps frequencies ks coll)))

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

(defn nmedian
  "Returns the median of a collection for selected keys (ks) in coll."
  [ks coll]
  (cond (ds/dataset? coll) (generic-ds median ks coll)
        (mat/matrix? coll) (generic-mat median ks coll)
        :else (generic-maps median ks coll)))

;; Public fn for variance and standard deviation

(defn variance
  "Returns the variance of a collection. When mean is known, it makes the job faster."
  ([coll]
   (let [dmean (mean coll)]
     (/ (transduce (map #(square (- % dmean))) + coll)
        (dec (count coll)))))
  ([coll dmean]
   (/ (transduce (map #(square (- % dmean))) + coll)
      (dec (count coll)))))

(defn nvariance
  "Returns the variance for selected keys (ks) in a collection."
  [ks coll]
  (cond (ds/dataset? coll) (generic-ds variance ks coll)
        (mat/matrix? coll) (generic-mat variance ks coll)
        :else (generic-maps variance ks coll)))

(defn stdev
  "Returns the standard deviation of a collection. When mean is known, it makes
  the job much faster."
  ([coll]
    (sqrt (variance coll)))
  ([coll dmean]
    (sqrt (variance coll dmean))))

(defn nstdev
  "Returns the standard deviation for selected keys(ks) in a collection."
  [ks coll]
  (cond (ds/dataset? coll) (generic-ds stdev ks coll)
        (mat/matrix? coll) (generic-mat stdev ks coll)
        :else (generic-maps stdev ks coll)))












































