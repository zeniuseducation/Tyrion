(ns tyrion.distribution
  (:require [tyrion.stats :as stat]
            [tyrion.math :as math]))

;;Distribution analysis
;;Find skewness and curtosis of a distribution
;;This namespace will be integrated with stats.cljs
;;Check http://distributionsjl.readthedocs.org/

(defn skewness
  "Still a naive implementation of skewness
  See http://stackoverflow.com/questions/17588085/kurtosis-skewness-of-a-bar-graph-python"
  [coll]
  (let [mean (stat/mean coll)
        x-mean (mapv - coll (repeat mean))
        c (count coll)
        top (/ (->> x-mean
                    (map math/cube)
                    (reduce +))
               c 1.0)
        bot (math/pow (/ (->> x-mean
                              (map math/square)
                              (reduce +)) c)
                      1.5)]
    (/ top bot)))

(defn curtosis
  [coll]
  (let [mean (stat/mean coll)
        x-mean (mapv - coll (repeat mean))
        c (count coll)
        top (/ (->> x-mean
                    (map #(math/pow % 4))
                    (reduce +))
               c 1.0)
        bot (math/pow (/ (->> x-mean
                              (map math/square)
                              (reduce +)) c)
                      2)]
    ;;why 3 .....
    (- (/ top bot) 3)))
