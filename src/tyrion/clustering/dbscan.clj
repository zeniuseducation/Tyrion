(ns tyrion.clustering.dbscan
  (:require
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]
    [tyrion.distance :as d]
    [tyrion.utils :refer [get-col scale-data]]
    [tyrion.math :refer :all]))

(def distance-fn-list
  {:euclidean    d/euclidean
   :sq-euclidean d/sq-euclidean
   :cityblock    d/cityblock
   :hamming      d/hamming})




