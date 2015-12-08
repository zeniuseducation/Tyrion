(ns tyrion.distance-core
  (:require
    [clojure.test :refer :all]
    [tyrion.utils :refer :all]
    [tyrion.distance :as dist]))

(deftest type-test
  (let [xs (vec (range 1 11))
        ys (vec (range 1 11))
        ws (vec (take 10 (repeat 0.5)))]
    (testing "Output of chebyshev"
      (is (= true (number? (dist/chebyshev xs ys)))))
    (testing "Output of chisq"
      (is (= true (number? (dist/chisq xs ys)))))
    (testing "Output of cityblock"
      (is (= true (number? (dist/cityblock xs ys)))))
    (testing "Output of correlation"
      (is (= true (number? (dist/correlation xs ys)))))
    (testing "Output of cosine"
      (is (= true (number? (dist/cosine xs ys)))))
    (testing "Output of euclidean"
      (is (= true (number? (dist/euclidean xs ys)))))
    (testing "Output of hamming"
      (is (= true (number? (dist/hamming xs ys)))))
    (testing "Output of jaccard"
      (is (= true (number? (dist/jaccard xs ys)))))
    (testing "Output of minkowski"
      (is (= true (number? (dist/minkowski xs ys 1.0)))))
    (testing "Output of spannorm"
      (is (= true (number? (dist/spannorm xs ys)))))
    (testing "Output of sq-euclidean"
      (is (= true (number? (dist/sq-euclidean xs ys)))))
    (testing "Output of sqeuclidiean"
      (is (= true (number? (dist/sqeuclidiean xs ys)))))
    (testing "Output of w-cityblock"
      (is (= true (number? (dist/w-cityblock xs ys ws)))))
    (testing "Output of w-euclidean"
      (is (= true (number? (dist/w-euclidean xs ys ws)))))
    (testing "Output of w-hamming"
      (is (= true (number? (dist/w-hamming xs ys ws)))))
    (testing "Output of w-minkowski"
      (is (= true (number? (dist/w-minkowski xs ys ws 1.0)))))
    (testing "Output of w-sqeuclidean"
      (is (= true (number? (dist/w-sqeuclidean xs ys ws)))))))
