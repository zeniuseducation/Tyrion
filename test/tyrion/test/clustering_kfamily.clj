(ns tyrion.test.clustering-kfamily
  (:require
    [clojure.test :refer :all]
    [tyrion.math :refer [square sqrt]]
    [tyrion.utils :refer [round]]
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]
    [taoensso.timbre :as log]
    [tyrion.clustering.kfamily :refer [kmeans]]))

(defn- assign
  "Helper function to make things fun"
  [x]
  (cond (even? x) (+ 300 (rand-int 100) x)
        (== 0 (rem x 5)) (+ (rand-int 1000) (* 2 x))
        :else (+ x (rand-int 200))))

(deftest kmeans-testing
  (log/info "\nTesting kmeans")
  (let [ndata 2000
        raw-x (repeatedly ndata #(rand-int 500))
        data (->> (for [x raw-x]
                    [x (assign x)])
                  (mat/matrix))]

    (testing "Kmeans with 2 dimensional data"
      (log/info (str "\nKmeans 2 dims with " ndata " data"))
      (let [k 5
            clustered (time (kmeans k data))]
        (is (= k (count clustered)))
        (is (= ndata (count (apply concat clustered))))
        (is (= clojure.lang.PersistentVector
               (->> clustered ffirst type)))
        (is (= 2 (->> clustered ffirst count)))
        (is (= ndata (->> clustered (map count) (reduce +)))))))

  (let [ndata 2000
        raw-x (repeatedly ndata #(rand-int 500))
        data (->> (for [x raw-x]
                    [x (assign x) (assign x) (assign x) (assign x)])
                  (mat/matrix))]

    (testing "Kmeans with 5 dimensional data"
      (log/info (str "\nKmeans 5 dims with " ndata " data"))
      (let [k 10
            clustered (time (kmeans k data))]
        (is (= k (count clustered)))
        (is (= ndata (count (apply concat clustered))))
        (is (= clojure.lang.PersistentVector
               (->> clustered ffirst type)))
        (is (= 5 (->> clustered ffirst count)))
        (is (= ndata (->> clustered (map count) (reduce +))))))))
