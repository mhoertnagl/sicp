(ns ch2.ex2-5)

(def bigint-0 (biginteger 0))
(def bigint-2 (biginteger 2))
(def bigint-3 (biginteger 3))

(defn prime-cons [a b]
  (.multiply (.pow bigint-2 a)
             (.pow bigint-3 b)))

; Suppose p = 2^n*3^m. If we repeatedly divide this number by two
; as long as it is divisible by 2, we will arrive at n. Likewise,
; if we repeatedly divide by 3 as long as it is divisible by 3,
; we wind up with m.

(defn bigint-zero? [p] (= (.compareTo p bigint-0) 0))
(defn bigint-divisible [f p] (= (.mod p f) 0))

(defn count-prime-factor [f p]
  (if (bigint-zero? p)
    0
    (loop [q p
           n 0]
      (if (bigint-divisible f q)
        (recur (.divide q f) (inc n))
        n))))

(defn prime-car [p] (count-prime-factor bigint-2 p))
(defn prime-cdr [p] (count-prime-factor bigint-3 p))
