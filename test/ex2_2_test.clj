(ns ex2-2-test
  (:require [clojure.test :refer :all])
  (:require [ch2.ex2-2 :refer :all]))

(deftest exercise-2-2
 (testing "compute the midpoint of a segment"
   (def p (make-point 1 2))
   (def q (make-point 4 9))
   (def m (make-point 5/2 11/2))
   (def s (make-segment p q))
   (is (= m (midpoint-segment s))))
 )