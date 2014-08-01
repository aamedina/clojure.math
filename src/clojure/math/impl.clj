(ns clojure.math.impl
  (:refer-clojure :exclude [+ * / - rationalize integer? even? odd?
                            quot rem mod bit-and bit-or bit-xor bit-not
                            bit-shift-left bit-shift-right bit-set bit-clear
                            bit-test bit-flip bit-and-not])
  (:require [clojure.math.numeric-tower :refer :all])
  (:alias core clojure.core))

(set! *warn-on-reflection* true)

(extend-protocol Num
  Number
  (add [x y]
    (if (instance? Number y)
      (. clojure.lang.Numbers (add x y))
      (add y x)))
  (multiply [x y]
    (if (instance? Number y)
      (. clojure.lang.Numbers (multiply x y))
      (multiply y x)))
  (subtract [x y]
    (if (instance? Number y)
      (. clojure.lang.Numbers (minus x y))
      (add (.negate ^org.apache.commons.math3.complex.Complex y) x)))
  (abs [x] (Math/abs ^double x))
  (signum [x] (Math/signum ^double x))

  org.apache.commons.math3.complex.Complex
  (add [x y]
    (if (complex? y)
      (.add x ^org.apache.commons.math3.complex.Complex y)
      (.add x (double y))))
  (multiply [x y]
    (if (complex? y)
      (.multiply x ^org.apache.commons.math3.complex.Complex y)
      (.multiply x (double y))))
  (subtract [x y]
    (if (complex? y)
      (.subtract x ^org.apache.commons.math3.complex.Complex y)
      (.subtract x (double y))))
  (abs [x] (.abs x))
  (signum [x] (Math/signum ^double (real-part x))))

(extend-protocol Real
  Number
  (rationalize [x] (. clojure.lang.Numbers (rationalize x))))

(extend-protocol Fractional
  Number
  (divide [x y] (. clojure.lang.Numbers (divide x y))))

(defmacro quot-rem*
  [x y]
  (if (and (number? x) (number? y))
    [(. clojure.lang.Numbers (quotient x y))
     (. clojure.lang.Numbers (remainder x y))]
    `[(. clojure.lang.Numbers (quotient ~x ~y))
      (. clojure.lang.Numbers (remainder ~x ~y))]))

(extend-protocol Integral
  Integer
  (quot-rem [x y] (quot-rem* x y))
  
  Long
  (quot-rem [x y] (quot-rem* x y))

  clojure.lang.BigInt
  (quot-rem [x y] (quot-rem* x y))

  BigInteger
  (quot-rem [x y] (quot-rem* x y))

  Short
  (quot-rem [x y] (quot-rem* x y))

  Byte
  (quot-rem [x y] (quot-rem* x y)))

(extend-protocol Floating
  Number
  (exp [x]
    (Math/exp x))
  (sqrt [x]
    (if (pos? x)
      (Math/sqrt x)
      (sqrt (make-rectangular x 0))))
  (log [x]
    (Math/log x))
  (sin [x]
    (Math/sin x))
  (cos [x]
    (Math/cos x))
  (asin [x]
    (Math/asin x))
  (atan [x]
    (Math/atan x))
  (acos [x]
    (Math/acos x))
  (sinh [x]
    (Math/sinh x))
  (cosh [x]
    (Math/cosh x))

  org.apache.commons.math3.complex.Complex
  (exp [x]
    (.exp x))
  (sqrt [x]
    (.sqrt x))
  (log [x]
    (.log x))
  (sin [x]
    (.sin x))
  (cos [x]
    (.cos x))
  (asin [x]
    (.asin x))
  (atan [x]
    (.atan x))
  (acos [x]
    (.acos x))
  (sinh [x]
    (.sinh x))
  (cosh [x]
    (.cosh x)))

(extend-protocol RealFrac
  Float
  (proper-fraction [x]
    (let [q (int x)]
      [q (- x q)]))
  
  Double
  (proper-fraction [x]
    (let [q (long x)]
      [q (- x q)]))

  clojure.lang.Ratio
  (proper-fraction [x]
    (let [[numerator denominator] [(.-numerator x) (.-denominator x)]
          [q r] (quot-rem numerator denominator)]
      [q (/ r denominator)]))

  java.math.BigDecimal
  (proper-fraction [x]
    (let [q (bigint x)]
      [q (- x q)])))

(extend-protocol RealFloat
  Float
  (float-radix [x] 2)
  (float-digits [x] 24)
  (float-range [x] [Float/MIN_VALUE Float/MAX_VALUE])
  (decode-float [x]
    (let [bits (Float/floatToIntBits x)
          sign (if (zero? (core/bit-shift-right bits 31)) 1 -1)
          exponent (core/bit-and (core/bit-shift-right bits 23) 0xff)
          mantissa (if (zero? exponent)
                     (core/bit-shift-left (core/bit-and bits 0x7fffff) 1)
                     (core/bit-or (core/bit-and bits 0x7fffff) 0x800000))]
      [(* mantissa sign) (- (Math/getExponent x) 23)]))
  (encode-float [_ significand exponent]
    (float (* significand (expt 2 exponent))))
  (nan? [x] (.isNaN x))
  (infinite? [x] (.isInfinite x))
  (denormalized? [x]
    (and (or (pos? x) (neg? x))
         (== (Math/getExponent x) (dec Float/MIN_EXPONENT))))
  (negative-zero? [x]
    (and (zero? x) (not (.equals ^Float (float 0.0) ^Float (float x)))))
  (ieee? [x] true)
  
  Double
  (float-radix [x] 2)
  (float-digits [x] 53)
  (float-range [x] [Double/MIN_VALUE Double/MAX_VALUE])
  (decode-float [x]
    (let [bits (Double/doubleToLongBits x)
          sign (if (zero? (core/bit-shift-right bits 63)) 1 -1)
          exponent (int (core/bit-and (core/bit-shift-right bits 52) 0x7ff))
          mantissa (if (zero? exponent)
                     (core/bit-shift-left (core/bit-and bits 0xfffffffffffff) 1)
                     (core/bit-or (core/bit-and bits 0xfffffffffffff)
                                  0x10000000000000))]
      [(* mantissa sign) (- (Math/getExponent x) 52)]))
  (encode-float [_ significand exponent]
    (double (* significand (expt 2 exponent))))
  (nan? [x] (.isNaN x))
  (infinite? [x] (.isInfinite x))
  (denormalized? [x]
    (and (or (pos? x) (neg? x))
         (== (Math/getExponent x) (dec Double/MIN_EXPONENT))))
  (negative-zero? [x]
    (and (zero? x) (not (.equals ^Double (double 0.0) ^Double (double x)))))
  (ieee? [x] true))

(extend-protocol Complex
  org.apache.commons.math3.complex.Complex
  (real-part [^org.apache.commons.math3.complex.Complex z]
    (.getReal z))
  (imag-part [^org.apache.commons.math3.complex.Complex z]
    (.getImaginary z))
  (magnitude [z]
    (.abs z))
  (angle [z]
    (.getArgument z))
  (conjugate [z]
    (.conjugate z)))

(defmethod print-method org.apache.commons.math3.complex.Complex
  [^org.apache.commons.math3.complex.Complex x ^java.io.Writer writer]
  (let [[real imag] ((juxt real-part imag-part) x)]
    (.write writer (str real (when (pos? imag) "+") imag "i"))))

(alter-var-root #'*complex-number-type*
                (constantly org.apache.commons.math3.complex.Complex))

(alter-var-root #'i (constantly org.apache.commons.math3.complex.Complex/I))

(defmethod make-rectangular org.apache.commons.math3.complex.Complex
  [x y]
  (org.apache.commons.math3.complex.Complex. x y))

(extend-protocol Bitwise
  Byte
  (-bit-and [x y]
    (. clojure.lang.Numbers and x y))
  (-bit-or [x y]
    (. clojure.lang.Numbers or x y))
  (-bit-xor [x y]
    (. clojure.lang.Numbers xor x y))
  (bit-not [x]
    (. clojure.lang.Numbers not x))
  (bit-clear [x n]
    (. clojure.lang.Numbers clearBit x n))
  (bit-set [x n]
    (. clojure.lang.Numbers setBit x n))
  (bit-flip [x n]
    (. clojure.lang.Numbers flipBit x n))
  (bit-shift-left [x n]
    (. clojure.lang.Numbers shiftLeft x n))
  (bit-shift-right [x n]
    (. clojure.lang.Numbers shiftRight x n))
  (bit-rotate-left [x n])
  (bit-rotate-right [x n])
  (bit-test [x n]
    (. clojure.lang.Numbers testBit x n))
  (signed? [x] true)
  (bit-size [x] Byte/SIZE)

  Short
  (-bit-and [x y]
    (. clojure.lang.Numbers and x y))
  (-bit-or [x y]
    (. clojure.lang.Numbers or x y))
  (-bit-xor [x y]
    (. clojure.lang.Numbers xor x y))
  (bit-not [x]
    (. clojure.lang.Numbers not x))
  (bit-clear [x n]
    (. clojure.lang.Numbers clearBit x n))
  (bit-set [x n]
    (. clojure.lang.Numbers setBit x n))
  (bit-flip [x n]
    (. clojure.lang.Numbers flipBit x n))
  (bit-shift-left [x n]
    (. clojure.lang.Numbers shiftLeft x n))
  (bit-shift-right [x n]
    (. clojure.lang.Numbers shiftRight x n))
  (bit-rotate-left [x n])
  (bit-rotate-right [x n])
  (bit-test [x n]
    (. clojure.lang.Numbers testBit x n))
  (signed? [x] true)
  (bit-size [x] Short/SIZE)

  Integer
  (-bit-and [x y]
    (. clojure.lang.Numbers and x y))
  (-bit-or [x y]
    (. clojure.lang.Numbers or x y))
  (-bit-xor [x y]
    (. clojure.lang.Numbers xor x y))
  (bit-not [x]
    (. clojure.lang.Numbers not x))
  (bit-clear [x n]
    (. clojure.lang.Numbers clearBit x n))
  (bit-set [x n]
    (. clojure.lang.Numbers setBit x n))
  (bit-flip [x n]
    (. clojure.lang.Numbers flipBit x n))
  (bit-shift-left [x n]
    (. clojure.lang.Numbers shiftLeft x n))
  (bit-shift-right [x n]
    (. clojure.lang.Numbers shiftRight x n))
  (bit-rotate-left [x n])
  (bit-rotate-right [x n])
  (bit-test [x n]
    (. clojure.lang.Numbers testBit x n))
  (signed? [x] true)
  (bit-size [x] Integer/SIZE)

  Long
  (-bit-and [x y]
    (. clojure.lang.Numbers and x y))
  (-bit-or [x y]
    (. clojure.lang.Numbers or x y))
  (-bit-xor [x y]
    (. clojure.lang.Numbers xor x y))
  (bit-not [x]
    (. clojure.lang.Numbers not x))
  (bit-clear [x n]
    (. clojure.lang.Numbers clearBit x n))
  (bit-set [x n]
    (. clojure.lang.Numbers setBit x n))
  (bit-flip [x n]
    (. clojure.lang.Numbers flipBit x n))
  (bit-shift-left [x n]
    (. clojure.lang.Numbers shiftLeft x n))
  (bit-shift-right [x n]
    (. clojure.lang.Numbers shiftRight x n))
  (bit-rotate-left [x n])
  (bit-rotate-right [x n])
  (bit-test [x n]
    (. clojure.lang.Numbers testBit x n))
  (signed? [x] true)
  (bit-size [x] Long/SIZE)

  BigInteger
  (-bit-and [x y]
    (. clojure.lang.Numbers and x y))
  (-bit-or [x y]
    (. clojure.lang.Numbers or x y))
  (-bit-xor [x y]
    (. clojure.lang.Numbers xor x y))
  (bit-not [x]
    (. clojure.lang.Numbers not x))
  (bit-clear [x n]
    (. clojure.lang.Numbers clearBit x n))
  (bit-set [x n]
    (. clojure.lang.Numbers setBit x n))
  (bit-flip [x n]
    (. clojure.lang.Numbers flipBit x n))
  (bit-shift-left [x n]
    (. clojure.lang.Numbers shiftLeft x n))
  (bit-shift-right [x n]
    (. clojure.lang.Numbers shiftRight x n))
  (bit-rotate-left [x n])
  (bit-rotate-right [x n])
  (bit-test [x n]
    (. clojure.lang.Numbers testBit x n))
  (signed? [x] true)
  (bit-size [x] (.bitLength x))

  clojure.lang.BigInt
  (-bit-and [x y]
    (. clojure.lang.Numbers and x y))
  (-bit-or [x y]
    (. clojure.lang.Numbers or x y))
  (-bit-xor [x y]
    (. clojure.lang.Numbers xor x y))
  (bit-not [x]
    (. clojure.lang.Numbers not x))
  (bit-clear [x n]
    (. clojure.lang.Numbers clearBit x n))
  (bit-set [x n]
    (. clojure.lang.Numbers setBit x n))
  (bit-flip [x n]
    (. clojure.lang.Numbers flipBit x n))
  (bit-shift-left [x n]
    (. clojure.lang.Numbers shiftLeft x n))
  (bit-shift-right [x n]
    (. clojure.lang.Numbers shiftRight x n))
  (bit-test [x n]
    (. clojure.lang.Numbers testBit x n))
  (signed? [x] true)
  (bit-size [x] (.bitLength x)))
