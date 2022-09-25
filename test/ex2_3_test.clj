(ns ex2-3-test
  (:require [clojure.test :refer :all])
  (:require [ex2-2 :refer :all])
  (:require [ex2-3 :refer :all]))

(deftest exercise-2-3
 (testing "compute the area of a rectangle"
   (def p (make-point 1 2))
   (def q (make-point 4 9))
   (def s (make-segment p q))
   (def r1 (make-rectangle-1 p q))
   (def r2 (make-rectangle-2 s))
   (is (= 21 (rectangle-area r1)))
   (is (= 21 (rectangle-area r2))))

 (testing "compute the perimeter of a rectangle"
   (def p (make-point 1 2))
   (def q (make-point 4 9))
   (def s (make-segment p q))
   (def r1 (make-rectangle-1 p q))
   (def r2 (make-rectangle-2 s))
   (is (= 20 (rectangle-perimeter r1)))
   (is (= 20 (rectangle-perimeter r2))))
)