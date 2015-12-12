(ns tyrion.test.regressions
  (:require
    [clojure.test :refer :all]
    [tyrion.regressions :refer :all]
    [tyrion.math :refer [square sqrt]]
    [tyrion.utils :refer [round]]
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]
    [taoensso.timbre :refer [info]]))

(deftest linear-regression-test
  (time
    (let [x-one (map float (range 50000))
          m 3.0 c 12.0
          y-one (map #(+ (* m %) c) x-one)
          lm-two (do (info "Linear regression tests for 50000 data")
                     (info "\nTiming for two args linear-regresion")
                     (time (linear-regression x-one y-one)))
          lm-one (do (info "\nTiming for one arg linear-regresion")
                     (->> (mat/matrix [x-one y-one])
                          mat/transpose linear-regression time))
          lm-three-map (do (info "\nTiming for map linear-regresion")
                           (->> (map #(hash-map :a % :b %2) x-one y-one)
                                (linear-regression :a :b)
                                time))
          lm-three-ds (do (info "\nTiming for ds linear-regresion")
                          (->> (mat/matrix [x-one y-one])
                               (mat/transpose)
                               (ds/dataset [:a :b])
                               (linear-regression :a :b)
                               time))
          sets #{:correlation :gradient :intercept :data
                 :xrange :yrange :fn :sum-squared-errors}]


      (testing "One arg linear-regression"
        (is (= sets (set (keys lm-one))))
        (is (= true (fn? (:fn lm-one))))
        (is (= clojure.lang.PersistentVector (type (:xrange lm-one))))
        (is (= clojure.lang.PersistentVector (type (:yrange lm-one))))
        (is (= (repeat 3 java.lang.Double)
               (mapv type ((juxt :gradient :intercept :sum-squared-errors) lm-one))))
        (is (= m (round (:gradient lm-one))))
        (is (= c (round (:intercept lm-one))))
        (is (= y-one (->> (map (:fn lm-one) x-one)
                          (map round))))
        (is (= (->> (interleave x-one y-one)
                    (partition 2))
               (:data lm-one))))

      (testing "Two args linear-regression"
        (is (= sets (set (keys lm-two))))
        (is (= true (fn? (:fn lm-two))))
        (is (= clojure.lang.PersistentVector (type (:xrange lm-two))))
        (is (= clojure.lang.PersistentVector (type (:yrange lm-two))))
        (is (= (repeat 3 java.lang.Double)
               (mapv type ((juxt :gradient :intercept :sum-squared-errors) lm-two))))
        (is (= m (round (:gradient lm-two))))
        (is (= c (round (:intercept lm-two))))
        (is (= y-one (->> (map (:fn lm-two) x-one)
                          (map round))))
        (is (= (->> (interleave x-one y-one)
                    (partition 2))
               (:data lm-two))))

      (testing "Three args map version linear-regression"
        (is (= sets (set (keys lm-three-map))))
        (is (= true (fn? (:fn lm-three-map))))
        (is (= clojure.lang.PersistentVector (type (:xrange lm-three-map))))
        (is (= clojure.lang.PersistentVector (type (:yrange lm-three-map))))
        (is (= (repeat 3 java.lang.Double)
               (mapv type ((juxt :gradient :intercept :sum-squared-errors) lm-three-map))))
        (is (= m (round (:gradient lm-three-map))))
        (is (= c (round (:intercept lm-three-map))))
        (is (= y-one (->> (map (:fn lm-three-map) x-one)
                          (map round))))
        (is (= (->> (interleave x-one y-one)
                    (partition 2))
               (:data lm-three-map))))

      (testing "Three args dataset version linear-regression"
        (is (= sets (set (keys lm-three-ds))))
        (is (= true (fn? (:fn lm-three-ds))))
        (is (= clojure.lang.PersistentVector (type (:xrange lm-three-ds))))
        (is (= clojure.lang.PersistentVector (type (:yrange lm-three-ds))))
        (is (= (repeat 3 java.lang.Double)
               (mapv type ((juxt :gradient :intercept :sum-squared-errors) lm-three-ds))))
        (is (= m (round (:gradient lm-three-ds))))
        (is (= c (round (:intercept lm-three-ds))))
        (is (= y-one (->> (map (:fn lm-three-ds) x-one)
                          (map round))))
        (is (= (->> (interleave x-one y-one)
                    (partition 2))
               (:data lm-three-map)))))))
