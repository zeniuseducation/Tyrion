(ns tyrion.test.view
  (:require
    [clojure.test :refer :all]
    [tyrion.view :refer :all]
    [clojure.core.matrix :as mat]
    [taoensso.timbre :as log]
    [clojure.core.matrix.dataset :as ds]))

(mat/set-current-implementation :vectorz)

(deftest simple-plot-tests
  (log/info "\nTesting simple plots")
  (let [maxi 10
        data-map (for [i (range maxi)
                       j (range maxi)]
                   {:a i :b j :c (* i j) :d (+ i j 100)})]

    (log/info "\nTesting plot-components")
    (time
      (testing "Testing plot-components of list of maps"
        (is (= (for [i (range maxi)
                     j (range maxi)]
                 {:x i :y j})
               (-> (plot-components [:a :b] data-map)
                   (get-in [:content :data 0 :values]))))
        (is (= (for [i (range maxi)
                     j (range maxi)]
                 {:x i :y (* i j)})
               (-> (plot-components [:a :c] data-map)
                   (get-in [:content :data 0 :values]))))
        (is (= (for [i (range maxi)
                     j (range maxi)]
                 {:x i :y (+ 100 i j)})
               (-> (plot-components [:a :d] data-map)
                   (get-in [:content :data 0 :values])))))))

  (let [maxi 100
        maxi2 (* 2 maxi)
        data-one (map list (range maxi) (range maxi))
        data-two (map list (range maxi2) (range maxi2))]

    (log/info "\nTesting list-plot-compose")
    (time
      (testing "Testing list-plot-compose"
        (is (= (for [[a b] data-one]
                 {:x a :y b})
               (-> (list-plot-compose data-one)
                   (get-in [:content :data])
                   first :values)))
        (is (= (for [[a b] data-two]
                 {:x a :y b})
               (-> (list-plot-compose data-two)
                   (get-in [:content :data])
                   first :values)))
        (is (= [0 (dec maxi)]
               (-> (list-plot-compose data-one)
                   (get-in [:content :scales 0 :domain]))))
        (is (= [0 (dec maxi2)]
               (-> (list-plot-compose data-one data-two)
                   (get-in [:content :scales 0 :domain]))))
        (is (= (concat (for [[a b] data-one] {:x a :y b})
                       (for [[a b] data-two] {:x a :y b}))
               (->> (-> (list-plot-compose data-one data-two)
                        (get-in [:content :data]))
                    (mapcat :values))))))))

(deftest table-view-test
  (log/info "\nTesting table view")
  (let [maxi 100.0
        maps (for [i (range maxi)]
               (zipmap [:a :b :c]
                       (map double [i (* i i) (+ i i)])))
        mats (mat/matrix (for [i (range maxi)]
                           (map double [i (* i i) (+ i i)])))
        dset (ds/dataset [:a :b :c]
                         (for [i (range maxi)]
                           (map double [i (* i i) (+ i i)])))]
    (log/info "\nTesting table view")
    (testing "table view column names"
      (is (= [:columns [:a :b :c]] (get-in (table maps) [:opts])))
      (is (= [:columns [:a :b :c]] (get-in (table dset) [:opts])))
      (is (= nil (get-in (table mats) [:opts])))
      (is (= [:columns [:a :b]] (get-in (table maps [:a :b]) [:opts])))
      (is (= [:columns [:a :b]] (get-in (table dset [:a :b]) [:opts]))))

    (testing "table view column names"
      (is (= mats (mat/matrix (get-in (table maps) [:contents]))))
      (is (= mats (mat/matrix (get-in (table dset) [:contents]))))
      (is (= mats (get-in (table mats) [:contents])))
      (is (= (-> (mapv #(mat/get-column mats %) [0 1])
                 mat/transpose)
             (get-in (table maps [:a :b]) [:contents])))
      (is (= (-> (mapv #(mat/get-column mats %) [0 1])
                 mat/transpose)
             (get-in (table dset [:a :b]) [:contents]))))))























