(ns tyrion.test.stats-core
  (:require
    [clojure.test :refer :all]
    [tyrion.utils :refer :all]
    [tyrion.stats.core :refer :all]
    [clojure.core.matrix.dataset :as ds]
    [clojure.core.matrix :as mat]
    [tyrion.math :refer [square sqrt]]))

(mat/set-current-implementation :vectorz)

(defn- prime?
  "Helper function for test"
  [^long p]
  (cond (< p 2) false
        (== p 2) true
        (even? p) false
        :else (let [lim (inc (int (Math/sqrt p)))]
                (loop [i (int 3)]
                  (if (> i lim)
                    true
                    (if (== 0 (rem p i))
                      false
                      (recur (+ 2 i))))))))

(deftest mean-test
  (time
    (let [maxi 1999
          single-inc (range maxi)
          single-uniform (repeat maxi 50)
          single-steps (range 1 (* 2 maxi) 2)
          single-random (repeatedly maxi (fn [] (rand-int 10)))
          mean-sr (round (/ (reduce + single-random) (count single-random) (double 1.0)))]

      (testing "mean for one-dimensional data"
        (is (= (Math/floor (/ maxi 2.0)) (mean single-inc)))
        (is (= 50.0 (mean single-uniform)))
        (is (= (* maxi 1.0) (mean single-steps)))
        (is (= mean-sr (round (mean single-random)))))

      (testing "mean for dataset version"
        (is (= (zipmap [:a :b :d] [(Math/floor (/ maxi 2.0)) 50.0 mean-sr])
               (->> [single-inc single-uniform single-steps single-random]
                    (apply interleave)
                    (partition 4)
                    (ds/dataset [:a :b :c :d])
                    (nmean [:a :b :d])
                    (#(zipmap (keys %) (map round (vals %))))))))

      (testing "mean for maps version"
        (is (= (zipmap [:a :b :d] [(Math/floor (/ maxi 2.0)) 50.0 mean-sr])
               (->> [single-inc single-uniform single-steps single-random]
                    (apply map (fn [a b c d] {:a a :b b :c c :d d}))
                    (nmean [:a :b :d])
                    (#(zipmap (keys %) (map round (vals %))))))))

      (testing "mean for matrix version"
        (is (= [(Math/floor (/ maxi 2.0)) 50.0 mean-sr]
               (->> [single-inc single-uniform single-steps single-random]
                    (apply interleave)
                    (partition 4)
                    (mat/matrix)
                    (nmean [0 1 3])
                    (mapv round))))))))

(deftest mode-test
  (time
    (let [maxi 100
          most-common 13.0
          single-data (concat (repeat 20 most-common)
                              (range maxi)
                              (repeatedly 20 #(rand-int 50)))
          ndim-data (for [i single-data]
                      [i (+ i 10) (* i i)])
          map-version (mapv #(zipmap [:a :b :c] %) ndim-data)
          ds-version (ds/dataset [:a :b :c] ndim-data)
          mat-version (mat/matrix ndim-data)]

      (testing "modes"
        (is (= most-common (mode single-data)))
        (is (= {:a most-common
                :b (+ most-common 10)
                :c (#(* % %) most-common)}
               (nmode [:a :b :c] map-version)))
        (is (= {:a most-common :b (+ most-common 10) :c (#(* % %) most-common)}
               (nmode [:a :b :c] ds-version)))
        (is (= [most-common (+ most-common 10) (#(* % %) most-common)]
               (nmode [0 1 2] mat-version))))

      (testing "modes error"
        (is (= most-common (mode single-data)))
        (is (= {:a most-common
                :b (+ most-common 10)
                :d nil}
               (try (nmode [:a :b :d] map-version)
                    (catch Exception e))))
        (is (= nil
               (try (nmode [:a :b :c :d] ds-version)
                    (catch Exception e))))
        (is (= nil
               (try (nmode [0 1 2 3] mat-version)
                    (catch Exception e))))))))

(deftest mode-by-test
  (time
    (let [maxi 100
          most-common 13.0
          single-data (concat (repeat 20 most-common)
                              (range maxi)
                              (repeatedly 20 #(rand-int 50)))
          ndim-data (for [i single-data]
                      [i (+ i 10) (* i i)])
          map-version (mapv #(zipmap [:a :b :c] %) ndim-data)
          ds-version (ds/dataset [:a :b :c] ndim-data)
          mat-version (mat/matrix ndim-data)
          sqr #(* % %)
          cbrt #(Math/cbrt %)]

      (testing "mode-by one-dimensional data"
        (is (= (sqr most-common) (mode-by sqr single-data)))
        (is (= (cbrt most-common) (mode-by cbrt single-data))))

      (testing "mode-by ndim data map-version"
        (is (= {:a (cbrt most-common)
                :b (cbrt (+ most-common 10))
                :c (cbrt (sqr most-common))}
               (nmode-by cbrt [:a :b :c] map-version)))
        (is (= {:a (cbrt most-common)
                :b (cbrt (+ most-common 10))
                :c (cbrt (sqr most-common))}
               (nmode-by cbrt [:a :b :c] map-version))))

      (testing "mode-by ndim data ds-version"
        (is (= {:a (sqr most-common)
                :b (sqr (+ most-common 10))
                :c (sqr (sqr most-common))}
               (nmode-by sqr [:a :b :c] ds-version)))
        (is (= {:a (cbrt most-common)
                :b (cbrt (+ most-common 10))
                :c (cbrt (sqr most-common))}
               (nmode-by cbrt [:a :b :c] ds-version))))

      (testing "mode-by ndim data mat-version"
        (is (= [(sqr most-common)
                (sqr (+ most-common 10))
                (sqr (#(* % %) most-common))]
               (nmode-by sqr [0 1 2] mat-version)))
        (is (= [(cbrt most-common)
                (cbrt (+ most-common 10))
                (cbrt (#(* % %) most-common))]
               (nmode-by cbrt [0 1 2] mat-version))))

      (testing "mode-by error"
        (is (= (sqr most-common) (mode-by sqr single-data)))
        (is (= nil
               (try (nmode-by sqr [:a :b :d] map-version)
                    (catch Exception e))))
        (is (= nil
               (try (nmode-by sqr [:a :b :c :d] ds-version)
                    (catch Exception e))))
        (is (= nil
               (try (nmode-by sqr [0 1 2 3] mat-version)
                    (catch Exception e))))))))

(deftest median-test
  (time
    (let [maxi 99
          most-common (double (quot maxi 2))
          single-data (range 1 (inc maxi))
          ndim-data (for [i single-data]
                      [i (+ i 10) (* i i)])
          map-version (mapv #(zipmap [:a :b :c] %) ndim-data)
          ds-version (ds/dataset [:a :b :c] ndim-data)
          mat-version (mat/matrix ndim-data)]

      (testing "Median"
        (is (= (int most-common) (median single-data)))
        (is (= {:a (int most-common)
                :b (int (+ most-common 10))
                :c (int (#(* % %) most-common))}
               (nmedian [:a :b :c] map-version)))
        (is (= {:a most-common :b (+ most-common 10) :c (#(* % %) most-common)}
               (nmedian [:a :b :c] ds-version)))
        (is (= [most-common (+ most-common 10) (#(* % %) most-common)]
               (nmedian [0 1 2] mat-version))))

      (testing "Median error"
        (is (= (int most-common) (median single-data)))
        (is (= {:a (int most-common)
                :b (int (+ most-common 10))
                :d nil}
               (try (nmedian [:a :b :d] map-version)
                    (catch Exception e))))
        (is (= nil
               (try (nmedian [:a :b :c :d] ds-version)
                    (catch Exception e))))
        (is (= nil
               (try (nmedian [0 1 2 3] mat-version)
                    (catch Exception e))))))))


(deftest freq-test
  (time
    (let [maxi 300
          single-data (->> (fn [] (rand-int 10))
                           (repeatedly maxi)
                           (filter prime?))
          ndim-data (for [i single-data
                          j single-data]
                      [(* 1.0 i j) (+ 1.0 i j) (- i j 0.0) (+ 100.0 i (- 100 j))])
          matrix-version (mat/matrix ndim-data)
          ds-version (ds/dataset [:a :b :c :d] ndim-data)
          maps-version (mapv #(zipmap [:a :b :c :d] %) ndim-data)]

      ;; single-dimensional data test, nothing fancy just frequencies
      (testing "freq fn to one-dimensional data"
        (is (= (freq single-data)
               (frequencies single-data))))

      ;; n-dimensional data for both dataset and list of maps
      (testing "freq fn to n-dimensional data"
        (is (= [:a :c] (keys (nfreq [:a :c] ds-version))))
        (is (= [:b :d] (keys (nfreq [:b :d] maps-version))))
        (is (= [(->> (map first ndim-data)
                     (frequencies))
                (->> (map second ndim-data)
                     (frequencies))]
               (-> (nfreq [:a :b] ds-version)
                   ((juxt :a :b)))))
        (is (= [(->> (map first ndim-data)
                     (frequencies))
                (->> (map second ndim-data)
                     (frequencies))]
               (nfreq [0 1] matrix-version)))
        (is (= [(->> (map first ndim-data)
                     (frequencies))
                (->> (map second ndim-data)
                     (frequencies))]
               (-> (nfreq [:a :b] maps-version)
                   ((juxt :a :b))))))))

  (time
    (let [maxi 1000
          single-data (->> (fn [] (rand-int 10))
                           (repeatedly maxi))
          primes (filter prime? single-data)
          count-primes (count primes)]

      ;; single-dimensional data test
      (testing "freq-by to one-dimensional data"
        (is (= {true count-primes false (- maxi count-primes)}
               (freq-by prime? single-data)))

        (is (= {true 50 false 50}
               (freq-by odd? (range 100))))

        (is (= {0 20 1 20 2 20 3 20 4 20}
               (freq-by #(rem % 5) (range 1 101)))))

      ;; n-dimensional data tests
      (testing "freq-by to n-dimensional data using one fn"
        (is (= {:a {true count-primes false (- maxi count-primes)}
                :c (let [ctr (count (filter prime? (range maxi)))]
                     {true ctr false (- maxi ctr)})}
               (let [dats (-> #(hash-map :a % :b %2 :c %2)
                              (map single-data (range maxi)))]
                 (nfreq-by prime? [:a :c] dats))))

        (is (= {:a {true count-primes false (- maxi count-primes)}
                :c (let [ctr (count (filter prime? (range maxi)))]
                     {true ctr false (- maxi ctr)})}
               (let [dats (->> (interleave single-data (range maxi) (range maxi))
                               (partition 3)
                               (ds/dataset [:a :b :c]))]
                 (nfreq-by prime? [:a :c] dats))))

        (is (= [{true count-primes false (- maxi count-primes)}
                (let [ctr (count (filter prime? (range maxi)))]
                  {true ctr false (- maxi ctr)})]
               (let [dats (->> (interleave single-data (range maxi) (range maxi))
                               (partition 3)
                               (mat/matrix))]
                 (nfreq-by prime? [0 2] dats)))))


      (testing "freq-by to n-dimensional data using one map of fs"
        (is (= {:a (let [ctr (->> primes
                                  (filter prime?)
                                  count)]
                     {true ctr false (- maxi ctr)})
                :b {true (quot maxi 2) false (quot maxi 2)}}
               (let [dats (-> #(hash-map :a % :b %2 :c (+ % %2))
                              (map single-data (range maxi)))]
                 (-> {:a prime? :b odd?}
                     (nfreq-by [:a :b] dats)))))

        (is (= {:a {true count-primes false (- maxi count-primes)}
                :b {true (quot maxi 2) false (quot maxi 2)}}
               (let [dats (->> (interleave single-data
                                           (range maxi)
                                           (range maxi))
                               (partition 3)
                               (ds/dataset [:a :b :c]))]
                 (-> {:a prime? :b #(== 0 (rem % 2))}
                     (nfreq-by [:a :b] dats)))))

        (is (= [{true count-primes false (- maxi count-primes)}
                {true (quot maxi 2) false (quot maxi 2)}]
               (let [dats (->> (interleave single-data
                                           (range maxi)
                                           (range maxi))
                               (partition 3)
                               (mat/matrix))]
                 (-> {0 prime? 2 #(== 0 (rem % 2))}
                     (nfreq-by [0 2] dats)))))))))

(deftest variance-stdev-test
  (time
    (let [ndata 1000
          zero-data (repeat ndata 10)
          inc-data (range ndata)
          more-data (map square (range ndata))
          var-inc-data (let [dmean (mean inc-data)]
                         (/ (->> inc-data
                                 (map #(square (- % dmean)))
                                 (reduce +))
                            (dec (count inc-data))))
          var-more-data (let [dmean (mean more-data)]
                          (/ (->> more-data
                                  (map #(square (- % dmean)))
                                  (reduce +))
                             (dec (count more-data))))]
      (testing "One-dim data variance and stdev"
        (is (= 0.0 (variance zero-data)))
        (is (= 0.0 (stdev zero-data)))
        (let [result (variance inc-data)]
          (is (= var-inc-data result))
          (is (= (sqrt var-inc-data)
                 (stdev inc-data)))
          (is (= java.lang.Double (type result)))
          (is (= java.lang.Double (type (sqrt result))))))

      (testing "Maps version of variance & stdev"
        (let [data (map #(hash-map :a %1 :b %2 :c %3)
                        zero-data
                        inc-data
                        more-data)
              var-result (nvariance [:a :b :c] data)
              std-result (nstdev [:a :b :c] data)]
          (is (= {:a 0.0 :b var-inc-data :c var-more-data}
                 var-result))
          (is (= {:a 0.0
                  :b (sqrt var-inc-data)
                  :c (sqrt var-more-data)}
                 std-result))
          (is (= (repeat 3 java.lang.Double)
                 (map type (vals var-result))))
          (is (= (repeat 3 java.lang.Double)
                 (map type (vals std-result))))))

      (testing "Dataset version of variance and stdev"
        (let [data (->> (interleave zero-data inc-data more-data)
                        (partition 3)
                        (ds/dataset [:a :b :c]))]
          (let [var-result (nvariance [:a :b :c] data)
                std-result (nstdev [:a :b :c] data)]
            (is (= {:a 0.0 :b var-inc-data :c var-more-data}
                   var-result))
            (is (= {:a 0.0
                    :b (sqrt var-inc-data)
                    :c (sqrt var-more-data)}
                   std-result))
            (is (= (repeat 3 java.lang.Double)
                   (map type (vals var-result))))
            (is (= (repeat 3 java.lang.Double)
                   (map type (vals std-result)))))))

      (testing "Matrix version of variance and stdev"
        (let [data (->> (interleave zero-data inc-data more-data)
                        (partition 3)
                        mat/matrix)]
          (let [var-result (nvariance [0 1 2] data)
                std-result (nstdev [0 1 2] data)]
            (is (= [0.0 var-inc-data var-more-data]
                   var-result))
            (is (= (repeat 3 java.lang.Double)
                   (map type var-result)))
            (is (= [0.0 (sqrt var-inc-data) (sqrt var-more-data)]
                   std-result))
            (is (= (repeat 3 java.lang.Double)
                   (map type std-result)))))))))

(deftest dispersions-testing
  (time
    (let [ndata 1000
          inc-data (range ndata)
          cept-data (map (partial + 1000) (range ndata))
          square-data (map square (range ndata))
          map-version (map #(hash-map :a %1 :b %2 :c %3)
                           inc-data cept-data square-data)
          ds-version (->> (interleave inc-data cept-data square-data)
                          (partition 3)
                          (ds/dataset [:a :b :c]))
          mat-version (->> (interleave inc-data cept-data square-data)
                           (partition 3)
                           (mat/matrix))]

      (testing "Interquartile range one-dim data"
        (is (= (- 8 3) (iq-range (range 1 11))))
        (is (= (- 75 25) (iq-range (range 1 101)))))

      (testing "Inter-quartile range for map version"
        (let [result (niq-range [:a :b] map-version)]
          (is (= true (map? result)))
          (is (= [true true]
                 (map #(number? (val %)) result)))))

      (testing "Inter-quartile range for ds version"
        (let [result (niq-range [:a :b] ds-version)]
          (is (= true (map? result)))
          (is (= [true true]
                 (map #(number? (val %)) result)))))

      (testing "Inter-quartile range for mat version"
        (let [result (niq-range [0 1] mat-version)]
          (is (= true (vector? result)))
          (is (= [500.0 500.0] result))))

      (testing "Covariance"
        (is (= true (number? (covariance inc-data cept-data))))
        (is (= true (number? (covariance square-data cept-data))))
        (is (= true (number? (covariance :a :b ds-version))))
        (is (= true (number? (covariance :a :b map-version))))
        (is (= true (number? (covariance 0 1 mat-version)))))

      (testing "Deviations"
        (is (= ndata (count (deviations inc-data))))
        (is (= true (every? number? (deviations cept-data))))
        (is (= {:a ndata :b ndata}
               (-> (ndeviations [:a :b] map-version)
                   (update-in [:a] count)
                   (update-in [:b] count))))
        (is (= {:a ndata :b ndata}
               (-> (ndeviations [:a :b] ds-version)
                   (update-in [:a] count)
                   (update-in [:b] count))))
        (is (= [ndata ndata]
               (-> (ndeviations [0 1] mat-version)
                   (update-in [0] count)
                   (update-in [1] count)))))

      (testing "Correlation"
        (is (= 1.0 (round (correlation inc-data cept-data))))
        (is (= 1.0 (round (correlation :a :b map-version))))
        (is (= 1.0 (round (correlation :a :b ds-version))))
        (is (= 1.0 (round (correlation 0 1 mat-version))))))))








