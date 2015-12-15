(ns tyrion.clustering.kmeans
  (:require
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]
    [tyrion.utils :refer [get-col scale-data]]
    [tyrion.math :refer [square sqrt]]
    [tyrion.distance :as d]
    [tyrion.stats :as s]
    [taoensso.timbre :refer [info]]))

(defn- labeling
  "Labeling data with (range 1 (inc k))."
  [k data]
  (map #(hash-map :label % :data %2) (range 1 (inc k)) data))

(def distance-fn
  {:euclidean    d/euclidean
   :sq-euclidean d/sq-euclidean
   :cityblock    d/cityblock
   :hamming      d/hamming})

(defn- nearest
  "Returns the label of nearest data from datum using distance function dfn."
  [dfn datum data]
  (->> (apply min-key #(dfn datum (:data %)) data)
       :label))

(defn- kmeans-one
  "Kmeans clustering for one iteration only"
  [dfn tdata data]
  (let [ctr (mat/row-count data)]
    (loop [i (int 0) res (transient [])]
      (if (< i ctr)
        (let [x (try (mat/get-row data i) (catch Exception e))]
          (recur (+ i 1)
                 (->> (nearest dfn x tdata)
                      (hash-map :data x :label)
                      (conj! res))))
        (->> (persistent! res)
             (group-by :label))))))

(defn kmeans
  "Returns the kmeans clustering. This is the default using random seeds."
  [k raw & [opts]]
  (let [[dfn maxi scale scale-size]
        [(distance-fn (get opts :distance :sq-euclidean))
         (get opts :max-iter 100)
         (get opts :scale)
         (get opts :scale-size 1000)]
        data (if scale (scale-data raw scale-size) raw)
        ctr (mat/row-count data)
        dims (range (mat/row-count (mat/get-row data 0)))
        inits (->> (repeatedly k #(rand-int ctr))
                   (map #(mat/get-row data %))
                   (labeling k))]
    (loop [i (int 0) kms inits prev [] result []]
      (if (or (> i maxi) (= (map :data (sort-by :label kms))
                            (map :data (sort-by :label prev))))
        (->> (sort-by key result)
             (map second)
             (mat/emap :data))
        (let [tmp (kmeans-one dfn kms data)
              tmp-kms (->> (sort-by key tmp)
                           (map second)
                           (mapv #(map :data %))
                           (mapv #(s/nmean (vec dims) %))
                           (labeling k))]
          (recur (+ 1 i) tmp-kms kms tmp))))))


