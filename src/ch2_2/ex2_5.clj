(ns ch2_2.ex2-5
  (:require [clojure.test :refer :all]))

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

(deftest test
  (testing "count prime factor"
    (is (= (count-prime-factor (biginteger 2) (biginteger 0)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 1)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 2)) 1))
    (is (= (count-prime-factor (biginteger 2) (biginteger 3)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 4)) 2))
    (is (= (count-prime-factor (biginteger 2) (biginteger 5)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 6)) 1))
    (is (= (count-prime-factor (biginteger 2) (biginteger 7)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 8)) 3)))

  (testing "car of prime-pair"
    (is (= (prime-car (prime-cons 0 0)) 0))
    (is (= (prime-car (prime-cons 1 0)) 1))
    (is (= (prime-car (prime-cons 0 1)) 0))
    (is (= (prime-car (prime-cons 1 1)) 1))
    (is (= (prime-car (prime-cons 41 98)) 41))
    (is (= (prime-car (prime-cons 42 99)) 42)))

  (testing "cdr of prime-pair"
    (is (= (prime-cdr (prime-cons 0 0)) 0))
    (is (= (prime-cdr (prime-cons 1 0)) 0))
    (is (= (prime-cdr (prime-cons 0 1)) 1))
    (is (= (prime-cdr (prime-cons 1 1)) 1))
    (is (= (prime-cdr (prime-cons 41 98)) 98))
    (is (= (prime-cdr (prime-cons 42 99)) 99)))
  )
