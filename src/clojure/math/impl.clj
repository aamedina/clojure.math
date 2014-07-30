(ns clojure.math.impl
  (:refer-clojure :exclude [+ * / - rationalize int integer? even? odd?
                            quot rem mod])
  (:require [clojure.math.numeric-tower :refer :all])
  (:import (org.apache.commons.math3.complex Complex))
  (:alias core clojure.core))

(extend-protocol Num
  Number
  (add [x y] (. clojure.lang.Numbers (add x y)))
  (multiply [x y] (. clojure.lang.Numbers (multiply x y)))
  (subtract [x y] (. clojure.lang.Numbers (minus x y)))
  (abs [x] (Math/abs x))
  (signum [x] (Math/signum x))

  Complex
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

  Number
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
