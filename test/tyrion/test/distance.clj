(ns tyrion.test.distance
  (:require
    [clojure.test :refer :all]
    [tyrion.utils :refer :all]
    [tyrion.distance :as dist]))

(deftest type-test
  (let [xs (vec (range 1 11))
        ys (vec (range 1 11))
        ws (vec (take 10 (repeat 0.5)))]
    (testing "Output type of chebyshev"
      (is (= true (number? (dist/chebyshev xs ys)))))
    (testing "Output type of chisq"
      (is (= true (number? (dist/chisq xs ys)))))
    (testing "Output type of cityblock"
      (is (= true (number? (dist/cityblock xs ys)))))
    (testing "Output type of correlation"
      (is (= true (number? (dist/correlation xs ys)))))
    (testing "Output type of cosine"
      (is (= true (number? (dist/cosine xs ys)))))
    (testing "Output type of euclidean"
      (is (= true (number? (dist/euclidean xs ys)))))
    (testing "Output type of hamming"
      (is (= true (number? (dist/hamming xs ys)))))
    (testing "Output type of jaccard"
      (is (= true (number? (dist/jaccard xs ys)))))
    (testing "Output type of minkowski"
      (is (= true (number? (dist/minkowski xs ys 1.0)))))
    (testing "Output type of spannorm"
      (is (= true (number? (dist/spannorm xs ys)))))
    (testing "Output type of sq-euclidean"
      (is (= true (number? (dist/sq-euclidean xs ys))))) 
    (testing "Output type of w-cityblock"
      (is (= true (number? (dist/w-cityblock xs ys ws)))))
    (testing "Output type of w-euclidean"
      (is (= true (number? (dist/w-euclidean xs ys ws)))))
    (testing "Output type of w-hamming"
      (is (= true (number? (dist/w-hamming xs ys ws)))))
    (testing "Output type of w-minkowski"
      (is (= true (number? (dist/w-minkowski xs ys ws 1.0)))))
    (testing "Output type of w-sqeuclidean"
      (is (= true (number? (dist/w-sqeuclidean xs ys ws)))))))


;; Test with empty vector

(deftest empty-vector-test
  (let [xs []
        p 2]
    (testing "Empty vector as input for sq-euclidean"
      (is (== 0.0 (dist/sq-euclidean xs xs))))
    (testing "Empty vector as input for euclidean"
      (is (== 0.0 (dist/euclidean xs xs))))
    (testing "Empty vector as input for cityblock"
      (is (== 0.0 (dist/cityblock xs xs))))
    (testing "Empty vector as input for chebyshev"
      (is (== 0.0 (dist/chebyshev xs xs))))
    (testing "Empty vector as input for minkowski"
      (is (== 0.0 (dist/minkowski xs xs p))))
    (testing "Empty vector as input for hamming"
      (is (== 0.0 (dist/hamming xs xs))))))

;; Test the output value

(deftest individual-metrics
  (let [xs [1.0]
        ys [2.0]
        p 2.0]
    (testing "Individiual metrics for sq-euclidean"
      (is (== 0.0 (dist/sq-euclidean xs xs)))
      (is (== 1.0 (dist/sq-euclidean xs ys))))
    (testing "Individiual metrics for euclidean"
      (is (== 0.0 (dist/euclidean xs xs)))
      (is (== 1.0 (dist/euclidean xs ys))))
    (testing "Individiual metrics for cityblock"
      (is (== 0.0 (dist/cityblock xs xs)))
      (is (== 1.0 (dist/cityblock xs ys))))
    (testing "Individiual metrics for chebyshev"
      (is (== 0.0 (dist/chebyshev xs xs)))
      (is (== 1.0 (dist/chebyshev xs ys))))
    (testing "Individiual metrics for minkowski"
      (is (== 0.0 (dist/minkowski xs xs p)))
      (is (== 1.0 (dist/minkowski xs ys p))))
    (testing "Individiual metrics for hamming"
      (is (== 0.0 (dist/hamming xs xs)))
      (is (== 1.0 (dist/hamming xs ys))))))

(deftest value-test
  (let [xs (vec (take 10 (repeat 1)))
        ys (vec (take 10 (repeat 5)))
        ws (vec (take 10 (range 1 2 0.1)))
        p 2.0]
    (testing "Output of chebyshev"
      (is (== 4.0 (dist/chebyshev xs ys))))))
