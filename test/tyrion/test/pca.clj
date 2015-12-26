(ns tyrion.test.pca
  (:require
    [clojure.test :refer :all]
    [tyrion.pca :refer :all]
    [tyrion.utils :as u]
    [tyrion.data :as d]
    [clojure.core.matrix :as mat]
    [clojure.core.matrix.dataset :as ds]
    [taoensso.timbre :as log]))

(deftest pca-tests
  (time
    (let [data-map (->> (d/load-data :iris)
                        :data
                        (mapv #(dissoc % :species)))
          ks (keys (first data-map))
          data-mat (mat/matrix (mapv (apply juxt ks) data-map))
          data-ds (ds/dataset ks data-mat)]
      (testing "PCA"
        (log/info "Testing PCA")
        (let [res-map-1 (covar-correl ks 3 data-map)
              res-map-2 (pca ks 3 data-map)
              res-mat-1 (covar-correl 3 data-mat)
              res-mat-2 (pca 3 data-mat)
              res-ds-1 (covar-correl ks 3 data-ds)
              res-ds-2 (pca ks 3 data-ds)]
          (is (= clojure.lang.PersistentVector
                 (type res-map-1)))
          (is (= 3 (count res-map-1)))
          (is (= (repeat (count data-map) 3)
                 (map count res-map-2)))
          (is (= true (every? (set ks) res-map-1)))
          (is (= res-map-1 res-ds-1))
          (is (= res-map-2 res-ds-2))
          (is (= (mapv #(into #{} %) res-map-2)
                 (mapv #(into #{} %) res-mat-2))))))))

