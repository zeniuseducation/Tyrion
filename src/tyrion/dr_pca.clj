(ns tyrion.dr-pca
  (:require
    [tyrion.stats :as s]
    [clojure.core.matrix.dataset :as ds]
    [clojure.core.matrix :as mat]
    [tyrion.math :refer :all]
    [tyrion.utils :as u]
    [tyrion.data :as dset]))

;; Namespace for dimensionality reduction and principal component analysis

(defn- covar-correl
  "Returns the dimensions with highest covar and lowest correl amongst them"
  [dims target-dim data]
  (let [tmp (for [v1 dims v2 dims :when (not= v1 v2)]
              {:v1 v1
               :v2 v2
               :score (/ (s/covariance (u/get-col v1 data) (u/get-col v2 data))
                         (s/correlation (u/get-col v1 data) (u/get-col v2 data)))})]
    (loop [[x & xs] (mapcat (juxt :v1 :v2) (sort-by :score > tmp)) res #{}]
      (if x
        (if (= (count res) target-dim)
          (vec res)
          (recur xs (conj res x)))
        (vec res)))))

(defn pca
  "Returns the data with highest covariance and lowest correlation dimensions"
  [dims target-dim data]
  (let [ks (covar-correl dims target-dim data)]
    (->> (mapv #(u/get-col % data) ks)
         (mat/transpose))))
