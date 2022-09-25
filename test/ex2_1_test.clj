(ns ex2-1-test
  (:require [clojure.test :refer :all])
  (:require [ex2-1 :refer :all]))

(deftest exercise-2-1
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
