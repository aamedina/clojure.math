(ns clojure.math.numeric-tower
  (:refer-clojure :exclude [+ * / - rationalize integer? even? odd?
                            quot rem mod])
  (:import (org.apache.commons.math3.complex Complex)))

(def ^:const e Math/E)
(def ^:const pi Math/PI)

(defprotocol Num
  (add [x y])
  (multiply [x y])
  (subtract [x y])
  (abs [x])
  (signum [x]))

(defprotocol Real
  (rationalize [x]))

(defprotocol Integral
  (quot-rem [x y]))

(defprotocol Fractional
  (divide [x y]))

(defprotocol Floating
  (exp [x])
  (sqrt [x])
  (log [x])
  (sin [x])
  (cos [x])
  (asin [x])
  (atan [x])
  (acos [x])
  (sinh [x])
  (cosh [x]))

(defprotocol RealFrac
  (proper-fraction [x]))

(defprotocol RealFloat
  (float-radix [x])
  (float-digits [x])
  (float-range [x])
  (decode-float [x])
  (encode-float [x significand exponent])
  (nan? [x])
  (infinite? [x])
  (denormalized? [x])
  (negative-zero? [x])
  (ieee? [x]))

(defn +
  ([] 0)
  ([x] {:pre [(satisfies? Num x)]} x)
  ([x y] (add x y))
  ([x y & more] (reduce add (add x y) more)))

(defn *
  ([] 1)
  ([x] {:pre [(satisfies? Num x)]} x)
  ([x y] (multiply x y))
  ([x y & more] (reduce multiply (multiply x y) more)))

(defn /
  ([x] (divide 1 x))
  ([x y] (divide x y))
  ([x y & more] (reduce divide (divide x y) more)))

(defn -
  ([x] (subtract 0 x))
  ([x y] (subtract x y))
  ([x y & more] (reduce subtract (subtract x y) more)))

(defn quot
  [x y]
  (first (quot-rem x y)))

(defn rem
  [x y]
  (second (quot-rem x y)))

(defn mod
  [x y])

(defn gcd
  [x y])

(defn lcm
  [x y])

(defn integer?
  [x]
  (satisfies? Integral x))

(defn even?
  [n]
  (if (integer? n)
    (zero? (bit-and (clojure.lang.RT/uncheckedLongCast n) 1))
    (throw (IllegalArgumentException. (str "Argument must be integral: " n)))))

(defn odd?
  [n]
  (not (even? n)))

(defn expt
  [z w]
  (cond
    (pos? w) (reduce * (repeat w z))
    (neg? w) (/ 1 (reduce * (repeat (- w) z)))
    (zero? w) 1))

(defn truncate
  [x]
  (first (proper-fraction x)))

(defn round
  [x]
  (let [[integral fractional] (proper-fraction x)]
    (if (>= 0.5 (- 1.0 fractional))
      (inc integral)
      integral)))

(defn ceiling
  [x]
  (let [[integral fractional] (proper-fraction x)]
    (if (or (zero? fractional) (neg? integral))
      integral
      (inc integral))))

(defn floor
  [x]
  (let [[integral fractional] (proper-fraction x)]
    (if (or (zero? fractional) (pos? integral))
      integral
      (dec integral))))

(defn exponent
  [x]
  (if (zero? x)
    0
    (+ (float-digits x) (second (decode-float x)))))

(defn significand
  [x]
  (if (zero? x)
    0.0
    (let [[min max radix] (conj (float-range x) (float-radix x))
          [mantissa _] (decode-float x)]
      (encode-float x mantissa (- (float-digits x))))))

(defn atan2
  [y x]
  (cond
    (pos? x) (atan (/ y x))
    (and (zero? x) (pos? y)) (/ pi 2)
    (and (neg? x) (pos? y)) (+ pi (atan (/ y x)))
    (or (and (<= x 0) (pos? y))
        (and (negative-zero? x) (negative-zero? y)))
    (- (atan2 (- y) x))
    (and (zero? y) (or (neg? x) (negative-zero? x))) pi
    (and (zero? x) (zero? y)) y
    :else (+ x y)))

(defn complex?
  [x]
  (instance? Complex x))

(defn make-rectangular
  ([^double x]
     (Complex. x))
  ([^double x ^double y]
     (Complex. x y)))

(defn make-polar
  [^double magnitude ^double angle]
  ;; (+ (* magnitude (cos angle)) (* magnitude (sin angle) 0+1i))
  )

(defn real-part
  [^Complex z]
  (.getReal z))

(defn imag-part
  [^Complex z]
  (.getImaginary z))

(defn magnitude
  [^Complex z]
  )

(defn angle
  [^Complex z]
  (.getArgument z))

(defn conjugate
  [^Complex z]
  (.conjugate z))

(defmethod print-method Complex
  [^Complex x ^java.io.Writer writer]
  (.write writer (str (real-part x) "+" (imag-part x) "i")))

