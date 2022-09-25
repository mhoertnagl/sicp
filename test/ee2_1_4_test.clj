(ns ee2-1-4-test
  (:require [clojure.test :refer :all])
  (:require [ee2-1-4 :refer :all]))

; (defn abs [x] (max x (- x)))
(defn close-to [a b epsilon] (<= (abs (- a b)) epsilon))

(defn interval-close-to [x y epsilon]
  (and (close-to (lower-bound x) (lower-bound y) epsilon)
       (close-to (upper-bound x) (upper-bound y) epsilon)))

(deftest extended-exercise-2-1-4
  (testing "adding intervals"
    (def x (make-interval 2.4 3.3))
    (def y (make-interval 1.7 2.1))
    (def z (make-interval 4.1 5.4))
    (is (interval-close-to (add-interval x y) z 1e-7)))
)

(deftest exercise-2-8
  (testing "subtracting intervals"
    (def x (make-interval 2.8 3.2))
    (def y (make-interval 1.6 2.1))
    (def z (make-interval 0.7 1.6))
    (is (interval-close-to (sub-interval x y) z 1e-7)))
)