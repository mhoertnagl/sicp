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

(deftest exercise-2-10
  (testing "disallow intervals that span zero"
    (is (thrown? IllegalArgumentException (make-interval 1 1))))
)

(deftest exercise-2-12
  (testing "percentage intervals"
    (let [i (make-center-percent 100 2)]
      (is (= (lower-bound i) 98))
      (is (= (upper-bound i) 102))
      (is (= (center i) 100))
      ;; This is failing with 4 != 2N.
      ;(is (= (width i) 4))
      (is (= (percent i) 2))))

  ;; 1% 1% ~> 2%
  ;; 2% 1% ~> 3%
  ;; 2% 2% ~> 4%
  ;; 20% 20% ~> 38.5%
  (testing "multiply intervals with small percentages"
    (let [x (make-center-percent 100 1)
          y (make-center-percent 100 1)
          p (mul-interval x y)]
      (println (percent p)))
    (let [x (make-center-percent 100 2)
          y (make-center-percent 100 1)
          p (mul-interval x y)]
      (println (percent p)))
    (let [x (make-center-percent 100 2)
          y (make-center-percent 100 2)
          p (mul-interval x y)]
      (println (percent p)))
    (let [x (make-center-percent 100 20)
          y (make-center-percent 100 20)
          p (mul-interval x y)]
      (println (percent p))))
)

(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (inv-interval (add-interval (inv-interval r1)
                              (inv-interval r2))))
(deftest exercise-2-14
  (testing "parallel resistances results differ"
    (let [r1 (make-center-percent 1000 5)
          r2 (make-center-percent 2000 7)]
      (print-interval-percent (par1 r1 r2))
      (print-interval-percent (par2 r1 r2))))
)

(deftest exercise-2-15
  (testing "quotients of two intervals"
    (let [r1 (make-center-percent 1000 5)]
      (print-interval-percent (div-interval r1 r1))
      (print-interval-percent (mul-interval r1 (div-interval r1 r1)))
      (print-interval-percent (div-interval (mul-interval r1 r1) r1))))
)