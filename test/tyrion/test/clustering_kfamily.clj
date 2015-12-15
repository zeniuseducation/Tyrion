(ns tyrion.test.clustering-kfamily
  (:require
    [clojure.test :refer :all]
    [tyrion.math :refer [square sqrt]]
    [tyrion.utils :refer [round]]
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]
    [taoensso.timbre :as log]
    [tyrion.clustering.kmeans :refer [kmeans]]))

(mat/set-current-implementation :vectorz)

(defn- assign
  "Helper function to make things fun"
  [x]
  (cond (even? x) (+ 300 (rand-int 100) x)
        (== 0 (rem x 5)) (+ (rand-int 1000) (* 2 x))
        :else (+ x (rand-int 200))))

(deftest kmeans-testing
  (log/info "\nTesting kmeans")
  (let [ndata 1000
        raw-x (repeatedly ndata #(rand-int 500))
        data (->> (for [x raw-x]
                    [x (assign x)])
                  (mat/matrix))]

    (testing "Kmeans with 2 dimensional data"
      (log/info (str "\nKmeans 2 dims with " ndata " data"))
      (let [k 5
            clustered (time (kmeans k data))]
        (is (= k (mat/row-count clustered)))
        (is (= ndata (mat/row-count (apply concat clustered))))
        (is (= mikera.vectorz.impl.ArraySubVector
               (->> clustered ffirst type)))
        (is (= 2 (->> clustered ffirst mat/row-count)))
        (is (= ndata (->> clustered (map count) (reduce +)))))))

  (let [ndata 1000
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
        (is (= mikera.vectorz.impl.ArraySubVector
               (->> clustered ffirst type)))
        (is (= 5 (->> clustered ffirst (mat/row-count))))
        (is (= ndata (->> clustered (map count) (reduce +))))))))
