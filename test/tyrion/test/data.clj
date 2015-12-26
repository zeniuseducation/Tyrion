(ns tyrion.test.data
  (:require
    [clojure.test :refer :all]
    [tyrion.data :refer :all]
    [tyrion.utils :as u]
    [taoensso.timbre :as log]
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]))

(mat/set-current-implementation :vectorz)

(deftest data-tests
  (time
    (testing "Data loading"
      (log/info "Testing data loading for various data-set")
      (let [data-1 (load-data :iris)
            ctr-1 (count (:data data-1))
            iris (:data data-1)
            data-2 (load-data :mammals)
            ctr-2 (count (:data data-2))
            mammals (:data data-2)
            ks #{:name :file :numerics :description :data}]
        (is (= ks (set (keys data-1))))
        (is (= ks (set (keys data-2))))
        (is (= clojure.lang.PersistentVector
               (type iris)))
        (is (= clojure.lang.PersistentVector
               (type mammals)))
        (is (= (repeat ctr-1 clojure.lang.PersistentArrayMap)
               (mapv type iris)))
        (is (= (repeat ctr-2 clojure.lang.PersistentArrayMap)
               (mapv type mammals)))
        (is (= true (every? #(= (keys (first iris)) (keys %)) iris)))
        (is (= true (every? #(= (keys (first mammals)) (keys %)) mammals)))
        (is (= (repeat ctr-1 true)
               (mapv #(every? number? ((apply juxt (:numerics data-1)) %)) iris)))
        (is (= (repeat ctr-2 true)
               (mapv #(every? number? ((apply juxt (:numerics data-2)) %)) mammals)))))))
