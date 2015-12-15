(ns tyrion.utils
  (:require
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]))

(defn round
  "Round a double to the given precision (number of significant digits)"
  ([d] (round d 5))
  ([d precision]
   (let [factor (Math/pow 10 precision)]
     (/ (Math/round (* d factor)) factor))))

(defn column-index
  "Helper function to get the index of a column in ds"
  [ds-columns column-name]
  (->> (map-indexed #(do [% %2]) ds-columns)
       (filter #(= (second %) column-name))
       ffirst))

(defn column-vals
  "Helper function to get the values of a column in ds"
  [ds column-name]
  (let [idx (column-index (get ds :column-names) column-name)]
    (get-in ds [:columns idx])))

(defn get-col
  [k coll]
  (cond (ds/dataset? coll) (ds/column coll k)
        (mat/matrix? coll) (mat/get-column coll k)
        :else (mapv #(get % k) coll)))

(defn col-names
  [data]
  (if (ds/dataset? data)
    (get data :column-names)
    (keys (first data))))