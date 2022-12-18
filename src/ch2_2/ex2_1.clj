(ns ch2_2.ex2-1
  (:require [clojure.test :refer :all]))

(defn gcd [a b] (.gcd (biginteger a) (biginteger b)))
(defn sign [a] (.signum (biginteger a)))
(defn abs [a] (.abs (biginteger a)))

; Exercise 2.1
(defn make-rat [n d]
  (let [g (gcd n d)
        n' (* (sign d) (/ n g))
        d' (abs (/ d g))]
    [n' d']))

(defn numer [x] (first x))

(defn denom [x] (second x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [x]
  (println (str (numer x) "/" (denom x))))

(deftest tests
  (testing "make-rat should handle positive arguments"
    (def act (make-rat 1 2))
    (def exp (make-rat 1 2))
    (is (= (numer exp) (numer act)))
    (is (= (denom exp) (denom act))))

  (testing "make-rat should handle negative numerator"
    (def act (make-rat -1 2))
    (def exp (make-rat -1 2))
    (is (= (numer exp) (numer act)))
    (is (= (denom exp) (denom act))))

  (testing "make-rat should handle negative denominator"
    (def act (make-rat 1 -2))
    (def exp (make-rat -1 2))
    (is (= (numer exp) (numer act)))
    (is (= (denom exp) (denom act))))

  (testing "make-rat should handle negative numerator and denominator"
    (def act (make-rat -1 -2))
    (def exp (make-rat 1 2))
    (is (= (numer exp) (numer act)))
    (is (= (denom exp) (denom act))))
  )

