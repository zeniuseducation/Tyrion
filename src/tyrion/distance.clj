(ns tyrion.distance
  (:require [tyrion.math :as math]))

;;Attemp to copy https://github.com/JuliaStats/Distances.jl
;;Still under heavy development

;;All of thix function assumes that xs, ys and ws have the same length

;;TODO More distance
;;Rogers-Tanimoto
;;KLDivergence
;;JSDivergence
;;BhattacharyyaDist
;;HellingerDist
;;Mahalanobis
;;SqMahalanobis
;;WeightedEuclidean
;;WeightedSqEuclidean
;;WeightedCityblock
;;WeightedMinkowski
;;WeightedHamming

(defn sq-euclidean
  "sqrt(sum((x - y) .^ 2))"
  [xs ys]
  (->> xs
       (map (comp math/square -) ys)
       (reduce +)))

(defn euclidean
  "sum((x - y).^2)"
  [xs ys]
  (math/sqrt (sq-euclidean xs ys)))

(defn cityblock
  "sum(abs(x - y))"
  [xs ys]
  (->> xs
       (map (comp math/abs -) ys)
       (reduce +)))

(defn chebyshev
  "max(abs(x - y))"
  [xs ys]
  (->> xs
       (map (comp math/abs -) ys)
       (cons 0)
       (apply max)))

(defn minkowski
  "sum(abs(x - y).^p) ^ (1/p)"
  [xs ys p]
  (let [sigma (->> xs
                   (map (comp math/abs -) ys)
                   (map #(math/pow % p))
                   (reduce +))]
    (math/pow sigma (double (/ 1.0 p)))))

(defn hamming
  "sum(x .!= y)"
  [xs ys]
  (->> xs
       (map == ys)
       (map (fn [bool]
              (if bool 0 1)))
       (reduce +)))

(defn jaccard
  "1 - sum(min(x, y)) / sum(max(x, y))"
  [xs ys]
  (let [[sum-min sum-max] (->> [xs ys]
                               (map (juxt (partial apply min)
                                          (partial apply max)))
                               (map (partial reduce +)))]
    (- 1 (/ sum-min sum-max))))

(defn cosine
  "Check here https://reference.wolfram.com/language/ref/CosineDistance.html"
  [xs ys]
  (let [dot (->> [xs ys]
                 (map (partial apply *))
                 (reduce +))
        sq #(->> %
                 (map math/abs)
                 (map math/square)
                 (reduce +)
                 math/sqrt)]
    (- 1 (/ dot (* (sq xs) (sq ys)) 1.0))))

(defn correlation
  "cosine_dist(x - mean(x), y - mean(y))"
  [xs ys]
  (let [mean (fn [coll]
               (let [c (count coll)]
                 (/ (reduce + coll) c)))
        meanx (mean xs)
        meany (mean ys)]
    (cosine (mapv - xs (repeat meanx))
            (mapv - ys (repeat meany)))))

(defn chisq
  "sum((x - y).^2 / (x + y))"
  [xs ys]
  (let [xy+ (map + xs ys)]
    (->> xs
         (map (comp math/square -) ys)
         (map (fn [a b]
                (/ b a))
              xy+)
         (reduce +))))

(defn spannorm
  "max(x - y) - min(x - y )"
  [xs ys]
  (let [diff (map - xs ys)]
    (- (apply max diff)
       (apply min diff))))

(defn w-sqeuclidean
  "sum((x - y).^2 .* w)"
  [xs ys ws]
  (->> xs
       (map (comp math/square -) ys)
       (map * ws)
       (reduce +)))

(defn w-euclidean
  "sqrt(sum((x - y).^2 .* w))"
  [xs ys ws]
  (math/sqrt (w-sqeuclidean xs ys ws)))

(defn w-cityblock
  "sum(abs(x - y) .* w)"
  [xs ys ws]
  (->> xs
       (map (comp math/abs -) ys)
       (map * ws)
       (reduce +)))

(defn w-minkowski
  "sum(abs(x - y).^p .* w) ^ (1/p)"
  [xs ys ws p]
  (let [sigma (->> xs
                   (map - ys)
                   (map math/abs)
                   (map #(math/pow % p))
                   (map * ws)
                   (apply +))]
    (math/pow sigma (/ 1.0 p))))

(defn w-hamming
  "sum((x .!= y) .* w)"
  [xs ys ws]
  (->> [xs ys]
       (apply map (fn [x y]
                    (if (= x y) 0 1)))
       (map * ws)
       (reduce +)))