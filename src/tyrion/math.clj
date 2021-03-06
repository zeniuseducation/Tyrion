(ns tyrion.math)

;;General Purpose math
;;Wrapping java.Math


(defn abs ^double [^double x]
  (if (< x 0.0) (- x) x))

(defn acos ^double [^double x]
  (Math/acos x))

(defn asin ^double [^double x]
  (Math/asin x))

(defn atan ^double [^double x]
  (Math/atan x))

(defn atan2 ^double [^double y ^double x]
  (Math/atan2 y x))

(defn cbrt ^double [^double a]
  (Math/cbrt a))

(defn ceil ^double [^double a]
  (Math/ceil a))

#_(defn copisign [])

(defn cos ^double [^double a]
  (Math/cos a))

(defn cosh ^double [^double a]
  (Math/cosh a))

(defn exp ^double [^double a]
  (Math/exp a))

(defn expm1 ^double [^double a]
  (Math/expm1 a))

(defn floor ^double [^double a]
  (Math/floor a))

#_(defn getexponent [])

(defn log ^double [^double a]
  (Math/log a))

(defn log10 ^double [^double a]
  (Math/log10 a))

(defn log1p ^double [^double a]
  (Math/log1p a))

#_(defn max [])

#_(defn min [])

#_(defn nextafter [])

#_(defn nextup [])

(defn pow ^double [^double a ^double b]
  (Math/pow a b))

(defn random ^double []
  (Math/random))

(defn rint ^double [^double a]
  (Math/rint a))

(defn round  [^double a]
  (Math/round a))

#_(defn signum [])

(defn sin ^double [^double a]
  (Math/sin a))

(defn sinh ^double [^double a]
  (Math/sinh a))

(defn square ^double [^double a]
  (* a a))

(defn sqrt ^double [^double a]
  (Math/sqrt a))

(defn tan ^double [^double a]
  (Math/tan a))

(defn tanh ^double [^double a]
  (Math/tanh a))

(defn ->degrees ^double [^double angrad]
  (Math/toDegrees angrad))

(defn ->radians ^double [^double angdeg]
  (Math/toDegrees angdeg))

#_(defn ulp [])





