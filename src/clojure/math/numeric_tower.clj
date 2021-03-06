(ns clojure.math.numeric-tower
  (:refer-clojure :exclude [+ * / - rationalize integer? even? odd?
                            quot rem mod bit-and bit-or bit-xor bit-not
                            bit-shift-left bit-shift-right bit-set bit-clear
                            bit-test bit-flip bit-and-not])
  (:require [clojure.tools.namespace.repl :refer [refresh-all]]))

(def ^:dynamic *complex-number-type*)
(def ^:dynamic e)
(def ^:dynamic pi)
(def ^:dynamic i)

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

(defprotocol Complex
  (real-part [z])
  (imag-part [z])
  (magnitude [z])
  (angle [z])
  (conjugate [z]))

(defprotocol Bitwise
  (-bit-and [x y])
  (-bit-or [x y])
  (-bit-xor [x y])
  (bit-not [x])
  (bit-clear [x n])
  (bit-set [x n])
  (bit-flip [x n])
  (bit-shift-left [x n])
  (bit-shift-right [x n])
  (bit-test [x n])
  (bit-size [x])
  (signed? [x]))

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

(defn bit-and
  ([x y] (-bit-and x y))
  ([x y & more]
     (reduce bit-and (bit-and x y) more)))

(defn bit-or
  ([x y] (-bit-or x y))
  ([x y & more]
     (reduce bit-or (bit-or x y) more)))

(defn bit-xor
  ([x y] (-bit-xor x y))
  ([x y & more]
     (reduce bit-xor (bit-xor x y) more)))

(defn quot
  [x y]
  (first (quot-rem x y)))

(defn rem
  [x y]
  (second (quot-rem x y)))

(defn div-mod
  [n d]
  (let [[q r :as qr] (quot-rem n d)]
    (if (== (signum r) (- (signum d)))
      [(dec q) (+ r d)]
      qr)))

(defn div
  [x y]
  (first (div-mod x y)))

(defn mod
  [x y]
  (second (div-mod x y)))

(defn gcd
  [x y]
  (loop [a (abs x)
         b (abs y)]
    (if (zero? b)
      a
      (recur b (rem a b)))))

(defn square
  [x]
  (* x x))

(defn cube
  [x]
  (* x x x))

(defn lcm
  [x y]
  (if (or (zero? x) (zero? y))
    0
    (abs (* (quot x (gcd x y)) y))))

(defn integer?
  [x]
  (satisfies? Integral x))

(defn even?
  [n]
  (if (integer? n)
    (zero? (bit-and (unchecked-long n) 1))
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

(defn clamp
  [x y]
  (max (- x) (min x y)))

(defn scale-float
  [k x]
  (if (or (zero? k) (zero? x) (nan? x) (infinite? x))
    x
    (let [[m n] (decode-float x)
          [l h] (float-range x)
          d (float-digits x)
          b (+ (dec h) (* 4 d))]
      (encode-float x m (+ n (clamp b k))))))

(defn complex?
  [x]
  (satisfies? Complex x))

(defmulti make-rectangular (fn [x y] *complex-number-type*))

(defn make-polar
  [magnitude angle]
  (+ (* magnitude (cos angle)) (* magnitude (sin angle) i)))
