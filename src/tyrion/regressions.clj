(ns tyrion.regressions
  (:require
    [tyrion.math :refer :all]
    [tyrion.stats :refer :all]
    [tyrion.utils :refer [get-col]]
    [clojure.core.matrix :as mat]))

(defn linear-regression
  "Returns the complete model of simple linear regression model.
  This is the default fn that uses OLS. Returns the complete map."
  ([xcol ycol]
   (let [xs (mat/matrix xcol)
         ys (mat/matrix ycol)
         corr (correlation xs ys)
         gradient (/ (* (stdev ys) corr) (stdev xs))
         intercept (- (mean ys) (* gradient (mean xs)))
         fun (fn [x] (+ (* gradient x) intercept))]
     {:correlation        corr
      :gradient           gradient
      :intercept          intercept
      :xrange             [(apply min xs) (apply max xs)]
      :yrange             [(apply min ys) (apply max ys)]
      :fn                 fun
      :sum-squared-errors (->> (map #(square (- %2 (fun %))) xs ys)
                               (reduce +))
      :data               (->> (interleave xs ys)
                               (partition 2))}))
  ([xy-pairs]
   (let [xs (get-col 0 xy-pairs)
         ys (get-col 1 xy-pairs)]
     (linear-regression xs ys)))
  ([xkey ykey coll]
   (linear-regression (get-col xkey coll)
                      (get-col ykey coll))))
