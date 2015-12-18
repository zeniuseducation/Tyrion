(ns tyrion.classification
  (:require
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]
    [tyrion.utils :refer [get-col scale-data]]
    [tyrion.math :refer [square sqrt]]
    [tyrion.distance :as d]
    [tyrion.stats :as s]
    [taoensso.timbre :as log]))

(def distance-fn
  {:euclidean    d/euclidean
   :sq-euclidean d/sq-euclidean
   :cityblock    d/cityblock
   :hamming      d/hamming})

(defn nearest
  "Returns the label of the nearest neighbors of xy.
  dfn is distance fn, k is the number of nearest k neighbors, xy is the data,
  and tdata is the training data."
  ([dfn k xy tdata]
   (->> (sort-by #(dfn (:data %) xy) tdata)
        (take k)
        (map :label)
        frequencies
        (apply max-key val)
        first))
  ([dfn k xy tdata weighted?]
   (loop [[x & xs] (->> (sort-by #(dfn (:data %) xy) tdata) (take k))
          res {} i (int k)]
     (if x
       (recur xs (merge-with + res {(:label x) i}) (dec i))
       (->> (apply max-key val res) first)))))

(defn knn
  "Returns the k nearest neighbors classification, end result is {:data [] :label l}.
  Training data must in the form of list of {:data [] :label l}."
  ([k training-data data]
   (let [dfn (distance-fn :sq-euclidean)]
     (-> #(hash-map :data % :label (nearest dfn k % training-data))
         (mapv data))))
  ([k training-data data {:keys [fn-distance weighted?]}]
   (let [dfn (distance-fn fn-distance)]
     (-> #(hash-map :data % :label (nearest dfn k % training-data weighted?))
         (mapv data)))))




