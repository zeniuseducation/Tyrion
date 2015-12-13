(ns tyrion.test.view
  (:require
    [clojure.test :refer :all]
    [tyrion.view :refer :all]
    [clojure.core.matrix :as mat]
    [taoensso.timbre :as log]))

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























