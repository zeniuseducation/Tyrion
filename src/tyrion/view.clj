(ns tyrion.view
  (:require [gorilla-plot.core :as gp]))

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
  "Create list of [x y] for the purpose of plotting from a list of maps."
  [[x y :as keys-in-data] data]
  (map list
       (map #(get % x) data)
       (map #(get % y) data)))

(defn compose-plot
  "List (compose & plots) but each instead of plots they are list of [x y].
  And each list will be rendered to a specific color that is different to other lists."
  [& lists]
  (let [colours ["Magenta" "Lime" "Orange" "Blue" "SteelBlue"]
        extreme-x (->> (mapcat #(map first %) lists)
                       ((juxt #(apply min %) #(apply max %))))
        extreme-y (->> (mapcat #(map second %) lists)
                       ((juxt #(apply min %) #(apply max %))))]
    (loop [[l & ls] lists i 0 res (transient [])]
      (if l
        (recur ls (inc i)
               (conj! res (gp/list-plot
                            l
                            :colour (colours i)
                            :symbol-size 40
                            :plot-range [extreme-x extreme-y])))
        (apply gp/compose (persistent! res))))))


