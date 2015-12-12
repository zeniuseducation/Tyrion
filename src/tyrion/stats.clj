(ns tyrion.stats
  (:require
    [clojure.core.matrix.dataset :as ds]
    [clojure.core.matrix :as mat]
    [tyrion.utils :refer [column-index column-vals get-col]]
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
  (/ (mat/esum coll) (mat/row-count coll) 1.0))

(defn nmean
  "Returns the means of selected keys in a collection.
  Coll can be a list/vector of numbers or list of maps, dataset or matrix."
  [ks coll]
  (let [fmean (if (or (ds/dataset? coll) (mat/matrix? coll))
                #(/ (mat/esum %) (mat/row-count %) 1.0)
                mean)]
    (cond (ds/dataset? coll) (generic-ds fmean ks coll)
          (mat/matrix? coll) (generic-mat fmean ks coll)
          :else (generic-maps fmean ks coll))))

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
  (let [ctr (mat/row-count coll)
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
   (let [dmean (mean coll)
         ctr (mat/row-count coll)]
     (/ (transduce (map #(square (- % dmean))) + coll)
        (dec ctr))))
  ([coll dmean]
   (let [ctr (mat/row-count coll)]
     (/ (transduce (map #(square (- % dmean))) + coll)
        (dec ctr)))))

(defn deviations
  "Returns the deviations of a collection."
  [coll]
  (let [dmean (mean coll)]
    (sequence (map #(- % dmean)) coll)))

(defn ndeviations
  "Returns the deviations of a given keys of a list of maps/dataset/matrix."
  [ks coll]
  (cond (ds/dataset? coll) (generic-ds deviations ks coll)
        (mat/matrix? coll) (generic-mat deviations ks coll)
        :else (generic-maps deviations ks coll)))

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

(defn quartiles
  "Returns the quartiles in the form of [q1 q2 q3]."
  [coll]
  (let [sorted (sort coll)
        ctr (mat/row-count coll)
        nths (map #(int (* ctr (/ % 4))) (range 1 4))]
    (mapv #(nth sorted %) nths)))

(defn nquartiles
  "Returns the quartiles [q1 q2 q3] for selected keys in coll."
  [ks coll]
  (cond (ds/dataset? coll) (generic-ds quartiles ks coll)
        (mat/matrix? coll) (generic-mat quartiles ks coll)
        :else (generic-maps quartiles ks coll)))

(defn iq-range
  "Returns the interquartile range."
  [coll]
  (let [[q1 _ q3] (quartiles coll)]
    (- q3 q1)))

(defn niq-range
  "Returns the inter-quartile range for selected keys (ks) in coll."
  [ks coll]
  (cond (ds/dataset? coll) (generic-ds iq-range ks coll)
        (mat/matrix? coll) (generic-mat iq-range ks coll)
        :else (generic-maps iq-range ks coll)))

(defn covariance
  "Returns the covariance of two variables, either from two one-dim data
  (when given two args) or two supplied keys in a list of maps/data-set/matrix
  (three args)."
  ([c1 c2]
   {:pre [(== (mat/row-count c1) (mat/row-count c2))]}
   (let [ctr (mat/row-count c1)]
     (/ (mat/dot (deviations c1) (deviations c2))
        (- ctr 1) 1.0)))
  ([k1 k2 coll]
   (cond (ds/dataset? coll)
         (covariance (column-vals coll k1)
                     (column-vals coll k2))
         (mat/matrix? coll)
         (covariance (mat/get-column coll k1)
                     (mat/get-column coll k2))
         :else
         (covariance (map #(get % k1) coll)
                     (map #(get % k2) coll)))))

(defn correlation
  "Returns the correlation of two variables. Either from two one-dimensional data
  (when given two args) or two supplied keys of a list of maps/data-sets/matrix
  (three args)."
  ([c1 c2]
    (let [std1 (stdev c1)
          std2 (stdev c2)]
      (if (every? pos? [std1 std2])
        (/ (covariance c1 c2) std1 std2 1.0)
        0)))
  ([k1 k2 data]
    (let [c1 (get-col k1 data)
          c2 (get-col k2 data)]
      (correlation c1 c2))))














































