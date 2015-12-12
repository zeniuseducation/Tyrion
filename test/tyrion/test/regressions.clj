(ns tyrion.test.regressions
  (:require
    [clojure.test :refer :all]
    [tyrion.regressions :refer :all]
    [tyrion.math :refer [square sqrt]]
    [tyrion.utils :refer [round]]))

(deftest linear-regression-test
  (let [x-one (range 100)
        m 3.0 c 12.0
        y-one (map #(+ (* m %) c) x-one)
        lm-one (linear-regression x-one y-one)
        sets #{:correlation :gradient :intercept :data
               :xrange :yrange :fn :sum-squared-errors}]
    (testing "Two args linear-regression"
      (is (= sets (set (keys lm-one))))
      (is (= true (fn? (:fn lm-one))))
      (is (= clojure.lang.PersistentVector (type (:xrange lm-one))))
      (is (= clojure.lang.PersistentVector (type (:yrange lm-one))))
      (is (= (repeat 3 java.lang.Double)
             (mapv type ((juxt :gradient :intercept :sum-squared-errors) lm-one))))
      (is (= m (round (:gradient lm-one))))
      (is (= c (round (:intercept lm-one))))
      (is (= y-one (->> (map (:fn lm-one) x-one)
                        (map round)))))))
