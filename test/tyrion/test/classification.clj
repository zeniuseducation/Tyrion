(ns tyrion.test.classification
  (:require
    [clojure.test :refer :all]
    [tyrion.math :refer [square sqrt]]
    [tyrion.utils :refer [round]]
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]
    [taoensso.timbre :as log]
    [tyrion.classification :refer :all]))

(def tdata
  (for [i (range 0 31 5)
        j (range 0 31 5)]
    {:data [i j] :label (if (<= 0 i 15)
                          (if (<= 0 j 15)
                            :kecil-kecil
                            :kecil-gede)
                          (if (<= 0 j 15)
                            :gede-kecil
                            :gede-gede))}))

(deftest knn-test
  (time
    (let [ndata 1000
          randint 30
          data-1 (mapv vector
                       (repeatedly ndata #(rand-int randint))
                       (repeatedly ndata #(rand-int randint)))
          result-1 (knn 20 tdata data-1)
          ks #{:kecil-kecil :kecil-gede :gede-kecil :gede-gede}]

      (testing "K nearest neighbors"
        (log/info "\nTesting knn with" ndata "data size")
        (is (= ndata (count result-1)))
        (is (= ks (set (keys (group-by :label result-1)))))
        (is (= data-1 (mapv :data result-1)))
        (is (= 4 (count (keys (group-by :label result-1)))))))))


