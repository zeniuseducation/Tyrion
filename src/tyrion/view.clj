(ns tyrion.view
  (:require
    [gorilla-plot.core :as gp]
    [tyrion.regressions :refer [linear-regression]]
    [tyrion.utils :refer [get-col col-names]]
    [gorilla-repl.table :refer [table-view]]
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]
    [taoensso.timbre :as log]))

(def size (atom 700))

(def ^:private colours
  "Good colours to be used for differentiating plots."
  ["Magenta" "Lime" "Orange" "Blue" "SteelBlue" "Red" "GreenYellow"
   "LightCoral" "Maroon" "PaleVioletRed" "RebeccaPurple" "Cyan"])

(defn plot-components
  "Plot a list of maps with a specified k1 as x and k2 as y.
  k1 & k2 are keys in each map datum, and they must exist in all maps."
  [[k1 k2 :as keys-in-data] data]
  {:pre [(== 2 (count keys-in-data))
         (-> #(and (contains? % k1) (contains? % k2))
             (every? data))]}
  (gp/list-plot (map list
                     (map #(get % k1) data)
                     (map #(get % k2) data))))

(defn get-xy
  "Helper function to create list of [x y] for the purpose of plotting
  from a list of maps. x & y are keys in each map datum."
  [[x y :as keys-in-data] data]
  (map list
       (map #(get % x) data)
       (map #(get % y) data)))

(defn list-plot-compose
  "Like (compose & plots) but instead of plots they are list of [x y].
  And each list will be rendered to a specific color that is different to other lists."
  [& lists]
  (let [extreme-x (->> (mapcat #(get-col 0 %) lists)
                       ((juxt #(apply min %) #(apply max %))))
        extreme-y (->> (mapcat #(get-col 1 %) lists)
                       ((juxt #(apply min %) #(apply max %))))]
    (loop [[l & ls] lists i 0 res (transient [])]
      (if l
        (recur ls (inc i)
               (conj! res (gp/list-plot
                            l
                            :colour (colours i)
                            :symbol-size 15
                            :plot-size @size
                            :plot-range [extreme-x extreme-y])))
        (apply gp/compose (persistent! res))))))

(defn lm-plot
  "Plot the data and its linear-model's line."
  ([xs ys]
   (let [lm (linear-regression xs ys)]
     (gp/compose (gp/list-plot (->> (interleave xs ys)
                                    (partition 2))
                               :symbol-size 30
                               :plot-size @size
                               :plot-range [(:xrange lm)
                                            (:yrange lm)])
                 (gp/plot (:fn lm)
                          [(apply min xs) (apply max xs)]
                          :plot-points 200
                          :colour "steelblue"
                          :symbol-size 45))))
  ([xy-list]
   (let [xs (map first xy-list)
         ys (map second xy-list)]
     (lm-plot xs ys)))
  ([xkey ykey coll]
   (let [xs (get-col xkey coll)
         ys (get-col ykey coll)]
     (lm-plot xs ys))))

(defn- lm-plot-compose-impl-maps
  [& lists-of-xy-pairs]
  (let [extreme-x (->> (mapcat #(get-col 0 %) (map :data lists-of-xy-pairs))
                       ((juxt #(apply min %) #(apply max %))))
        extreme-y (->> (mapcat #(get-col 1 %) (map :data lists-of-xy-pairs))
                       ((juxt #(apply min %) #(apply max %))))]
    (loop [[lm & ls] lists-of-xy-pairs i 0 res (transient [])]
      (if lm
        (let [l (:data lm)
              xs (get-col 0 l)
              ys (get-col 1 l)]
          (recur ls (inc i)
                 (conj! (conj! res
                               (gp/list-plot
                                 (->> (interleave xs ys)
                                      (partition 2))
                                 :symbol-size 30
                                 :colour (colours i)
                                 :plot-size @size
                                 :plot-range [extreme-x extreme-y]))
                        (gp/plot (:fn lm)
                                 extreme-x
                                 :plot-points 200
                                 :colour (colours i)
                                 :symbol-size 45))))
        (apply gp/compose (persistent! res))))))

(defn- lm-plot-compose-impl-mat
  [& lists-of-xy-pairs]
  (let [extreme-x (->> (mapcat #(get-col 0 %) lists-of-xy-pairs)
                       ((juxt #(apply min %) #(apply max %))))
        extreme-y (->> (mapcat #(get-col 1 %) lists-of-xy-pairs)
                       ((juxt #(apply min %) #(apply max %))))]
    (loop [[l & ls] lists-of-xy-pairs i 0 res (transient [])]
      (if l
        (let [lm (linear-regression l)
              xs (get-col 0 l)
              ys (get-col 1 l)]
          (recur ls (inc i)
                 (conj! (conj! res
                               (gp/list-plot
                                 (->> (interleave xs ys)
                                      (partition 2))
                                 :symbol-size 30
                                 :colour (colours i)
                                 :plot-size @size
                                 :plot-range [extreme-x extreme-y]))
                        (gp/plot (:fn lm)
                                 extreme-x
                                 :plot-points 200
                                 :colour (colours i)
                                 :symbol-size 45))))
        (apply gp/compose (persistent! res))))))

(defn lm-plot-compose
  "Composed a linear regression plot for several collections of pairs.
  The goal is to automatically assigned different color to each set of data."
  [& lists-of-xy-pairs]
  (if (map? (first lists-of-xy-pairs))
    (apply lm-plot-compose-impl-maps lists-of-xy-pairs)
    (apply lm-plot-compose-impl-mat lists-of-xy-pairs)))

(defn cluster-plot
  "Like list-plot but with different colour for a different cluster.
  The data must be a list of list of xy-pairs."
  [clustered-data]
  (apply list-plot-compose clustered-data))

(defn table
  "Table view of data, coll can be a list of maps, dataset, or matrix.
  Given two arguments, ks are selected keys from coll."
  ([coll ks aliases]
   (cond (ds/dataset? coll)
         (-> (mapv #(get-col % coll) ks)
             mat/transpose
             (table-view :columns aliases))
         (mat/matrix? coll)
         (table-view coll :columns aliases)
         :else (table-view (mapv (apply juxt ks) coll)
                           :columns aliases)))
  ([coll]
   (cond
     (ds/dataset? coll) (let [cnames (col-names coll)]
                          (table coll cnames))
     (mat/matrix? coll) (table-view coll)
     :else (let [cnames (col-names coll)]
             (table coll cnames))))
  ([coll ks]
    (table coll ks ks)))
























