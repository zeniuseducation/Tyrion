(ns tyrion.development.descriptive)

(defn ^double mean
  "Calculate the mean of a collection of numerics data. Given one
  argument it returns the mean of that collection, given two arguments
  (a collection of maps and the selected keys, which is expected to be
  numeric data in each map) it returns the means of each key."
  ([^longs data]
     (let [size (double (count data))]
       (if-not (pos? size) 0
               (/ (reduce + data) size))))
  ([data selected-keys]
     (->> (for [k selected-keys]
            (map k data))
          (map mean)
          (zipmap selected-keys))))

;; TODO mean, variance, stdev, mode, mode-by, freq-by, quartiles
;; TODO iq-range, data-range, median, z-score,

;; TODO for regressions : lm, multivariable-lm, nonlinear-model, logit
;; TODO for classification : knn, naive-bayes, decision-tree, mcmc
;; TODO for clustering : kmeans, kmedoids, dbscan
;; TODO ann & deep learning
