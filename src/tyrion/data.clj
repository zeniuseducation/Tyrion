(ns tyrion.data
  (:require [clojure.string :as cs]))

(def dataset
  {:iris    {:name        "Iris"
             :description "Iris dataset, good for clustering/classifications, or basic stats"
             :file        "./data/iris.edn"}
   :mammals {:name        "Mammals"
             :description "Brain and body weight of various mammals"
             :file        "./data/mammals.edn"}})

(defn load-meta
  [dataset-key]
  (get dataset dataset-key :iris))

(defn load-data
  ([dataset-key]
   (let [{:keys [file] :as d} (load-meta dataset-key)]
     (assoc d :data (->> (slurp file) read-string))))
  ([dataset-key data-only?]
   (let [{:keys [file]} (load-meta dataset-key)]
     (->> (slurp file) read-string))))


