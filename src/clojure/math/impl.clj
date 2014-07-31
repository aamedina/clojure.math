(ns clojure.math.impl
  (:refer-clojure :exclude [+ * / - rationalize integer? even? odd?
                            quot rem mod])
  (:require [clojure.math.numeric-tower :refer :all])
  (:alias core clojure.core))

(extend-protocol Num
  Number
  (add [x y] (. clojure.lang.Numbers (add x y)))
  (multiply [x y] (. clojure.lang.Numbers (multiply x y)))
  (subtract [x y] (. clojure.lang.Numbers (minus x y)))
  (abs [x] (Math/abs x))
  (signum [x] (Math/signum x))

  org.apache.commons.math3.complex.Complex
  (add [x y] (.add x y))
  (multiply [x y] (.multiply x y))
  (subtract [x y] (.subtract x y))
  (abs [x] (.abs x))
  (signum [x] (Math/signum (real-part x))))

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
  Float
  (exp [x] (Math/exp x))
  (sqrt [x] (Math/sqrt x))
  (log [x] (Math/log x))
  (sin [x] (Math/sin x))
  (cos [x] (Math/cos x))
  (asin [x] (Math/asin x))
  (atan [x] (Math/atan x))
  (acos [x] (Math/acos x))
  (sinh [x] (Math/sinh x))
  (cosh [x] (Math/cosh x))

  Double
  (exp [x] (Math/exp x))
  (sqrt [x] (Math/sqrt x))
  (log [x] (Math/log x))
  (sin [x] (Math/sin x))
  (cos [x] (Math/cos x))
  (asin [x] (Math/asin x))
  (atan [x] (Math/atan x))
  (acos [x] (Math/acos x))
  (sinh [x] (Math/sinh x))
  (cosh [x] (Math/cosh x))

  Integer
  (exp [x] (Math/exp x))
  (sqrt [x] (Math/sqrt x))
  (log [x] (Math/log x))
  (sin [x] (Math/sin x))
  (cos [x] (Math/cos x))
  (asin [x] (Math/asin x))
  (atan [x] (Math/atan x))
  (acos [x] (Math/acos x))
  (sinh [x] (Math/sinh x))
  (cosh [x] (Math/cosh x))
  
  Long
  (exp [x] (Math/exp x))
  (sqrt [x] (Math/sqrt x))
  (log [x] (Math/log x))
  (sin [x] (Math/sin x))
  (cos [x] (Math/cos x))
  (asin [x] (Math/asin x))
  (atan [x] (Math/atan x))
  (acos [x] (Math/acos x))
  (sinh [x] (Math/sinh x))
  (cosh [x] (Math/cosh x))

  clojure.lang.BigInt
  (exp [x] (Math/exp x))
  (sqrt [x] (Math/sqrt x))
  (log [x] (Math/log x))
  (sin [x] (Math/sin x))
  (cos [x] (Math/cos x))
  (asin [x] (Math/asin x))
  (atan [x] (Math/atan x))
  (acos [x] (Math/acos x))
  (sinh [x] (Math/sinh x))
  (cosh [x] (Math/cosh x))

  BigInteger
  (exp [x] (Math/exp x))
  (sqrt [x] (Math/sqrt x))
  (log [x] (Math/log x))
  (sin [x] (Math/sin x))
  (cos [x] (Math/cos x))
  (asin [x] (Math/asin x))
  (atan [x] (Math/atan x))
  (acos [x] (Math/acos x))
  (sinh [x] (Math/sinh x))
  (cosh [x] (Math/cosh x))

  Short
  (exp [x] (Math/exp x))
  (sqrt [x] (Math/sqrt x))
  (log [x] (Math/log x))
  (sin [x] (Math/sin x))
  (cos [x] (Math/cos x))
  (asin [x] (Math/asin x))
  (atan [x] (Math/atan x))
  (acos [x] (Math/acos x))
  (sinh [x] (Math/sinh x))
  (cosh [x] (Math/cosh x))

  Byte
  (exp [x] (Math/exp x))
  (sqrt [x] (Math/sqrt x))
  (log [x] (Math/log x))
  (sin [x] (Math/sin x))
  (cos [x] (Math/cos x))
  (asin [x] (Math/asin x))
  (atan [x] (Math/atan x))
  (acos [x] (Math/acos x))
  (sinh [x] (Math/sinh x))
  (cosh [x] (Math/cosh x))

  BigDecimal
  (exp [x] (Math/exp x))
  (sqrt [x] (Math/sqrt x))
  (log [x] (Math/log x))
  (sin [x] (Math/sin x))
  (cos [x] (Math/cos x))
  (asin [x] (Math/asin x))
  (atan [x] (Math/atan x))
  (acos [x] (Math/acos x))
  (sinh [x] (Math/sinh x))
  (cosh [x] (Math/cosh x)))

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
          sign (if (zero? (bit-shift-right bits 31)) 1 -1)
          exponent (bit-and (bit-shift-right bits 23) 0xff)
          mantissa (if (zero? exponent)
                     (bit-shift-left (bit-and bits 0x7fffff) 1)
                     (bit-or (bit-and bits 0x7fffff) 0x800000))]
      [(* mantissa sign) (- (Math/getExponent x) 23)]))
  (encode-float [_ significand exponent]
    (float (* significand (expt 2 exponent))))
  (nan? [x] (.isNaN x))
  (infinite? [x] (.isInfinite x))
  (denormalized? [x]
    (and (or (pos? x) (neg? x))
         (== (Math/getExponent x) (dec Float/MIN_EXPONENT))))
  (negative-zero? [x]
    (and (zero? x) (not (.equals (float 0.0) (float x)))))
  (ieee? [x] true)
  
  Double
  (float-radix [x] 2)
  (float-digits [x] 53)
  (float-range [x] [Double/MIN_VALUE Double/MAX_VALUE])
  (decode-float [x]
    (let [bits (Double/doubleToLongBits x)
          sign (if (zero? (bit-shift-right bits 63)) 1 -1)
          exponent (int (bit-and (bit-shift-right bits 52) 0x7ff))
          mantissa (if (zero? exponent)
                     (bit-shift-left (bit-and bits 0xfffffffffffff) 1)
                     (bit-or (bit-and bits 0xfffffffffffff) 0x10000000000000))]
      [(* mantissa sign) (- (Math/getExponent x) 52)]))
  (encode-float [_ significand exponent]
    (double (* significand (expt 2 exponent))))
  (nan? [x] (.isNaN x))
  (infinite? [x] (.isInfinite x))
  (denormalized? [x]
    (and (or (pos? x) (neg? x))
         (== (Math/getExponent x) (dec Double/MIN_EXPONENT))))
  (negative-zero? [x]
    (and (zero? x) (not (.equals 0.0 (double x)))))
  (ieee? [x] true))

(extend-protocol Complex
  org.apache.commons.math3.complex.Complex
  (real-part [z]
    (.getReal z))
  (imag-part [z]
    (.getImaginary z))
  (magnitude [z]
    (.magnitude z))
  (angle [z]
    (.getArgument z)))

(defmethod print-method org.apache.commons.math3.complex.Complex
  [^org.apache.commons.math3.complex.Complex x ^java.io.Writer writer]
  (.write writer (str (real-part x) "+" (imag-part x) "i")))
