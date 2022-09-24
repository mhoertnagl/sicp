(ns ex2-5-test
  (:require [clojure.test :refer :all])
  (:require [ex2-5 :refer :all]))

(deftest exercise-2-5
  (testing "count prime factor"
    (is (= (count-prime-factor (biginteger 2) (biginteger 0)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 1)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 2)) 1))
    (is (= (count-prime-factor (biginteger 2) (biginteger 3)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 4)) 2))
    (is (= (count-prime-factor (biginteger 2) (biginteger 5)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 6)) 1))
    (is (= (count-prime-factor (biginteger 2) (biginteger 7)) 0))
    (is (= (count-prime-factor (biginteger 2) (biginteger 8)) 3))
)

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