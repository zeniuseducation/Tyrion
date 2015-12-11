(ns tyrion.regressions
  (:require
    [tyrion.math :refer :all]
    [tyrion.stats.core :refer :all]
    [clojure.core.matrix.dataset :as ds]
    [clojure.core.matrix :as mat]))

(defn linear-regression
  "Returns the complete model of simple linear regression model.
  This is the default fn that uses OLS. Returns the complete map."
  ([xs ys]
   (let [corr (correlation xs ys)
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
                               (reduce +))}))
  ([xkey ykey coll]
   (linear-regression (get-col xkey coll)
                      (get-col ykey coll))))
