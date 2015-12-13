(ns tyrion.view
  (:require
    [gorilla-plot.core :as gp]
    [tyrion.regressions :refer [linear-regression]]
    [tyrion.utils :refer [get-col]]))

(def size (atom 700))

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
  (let [colours ["Magenta" "Lime" "Orange" "Blue" "SteelBlue" "Red" "GreenYellow"
                 "LightCoral" "Maroon" "PaleVioletRed" "RebeccaPurple" "Cyan"]
        extreme-x (->> (mapcat #(get-col 0 %) lists)
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

(defn lm-plot-compose
  "Composed a linear regression plot for several collections of pairs.
  The goal is to automatically assigned different color to each set of data."
  [& lists-of-xy-pairs]
  (if (map? (first lists-of-xy-pairs))
    (let [colours ["Magenta" "Lime" "Orange" "Blue" "SteelBlue" "Red" "GreenYellow"
                   "LightCoral" "Maroon" "PaleVioletRed" "RebeccaPurple" "Cyan"]
          extreme-x (->> (mapcat #(get-col 0 %) (map :data lists-of-xy-pairs))
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
          (apply gp/compose (persistent! res)))))
    (let [colours ["Magenta" "Lime" "Orange" "Blue" "SteelBlue" "Red" "GreenYellow"
                   "LightCoral" "Maroon" "PaleVioletRed" "RebeccaPurple" "Cyan"]
          extreme-x (->> (mapcat #(get-col 0 %) lists-of-xy-pairs)
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
          (apply gp/compose (persistent! res)))))))


