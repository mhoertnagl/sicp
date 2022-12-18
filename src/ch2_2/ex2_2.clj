(ns ch2_2.ex2-2
  (:require [clojure.test :refer :all]))

(defn avg [a b] (/ (+ a b) 2))

(defn make-point [x y] [x y])
(defn x-point [p] (first p))
(defn y-point [p] (second p))
(defn point-str [p] (str "(" (x-point p) "," (y-point p) ")"))
(defn print-point [p] (println (point-str p)))

(defn make-segment [p q] [p q])
(defn start-segment [s] (first s))
(defn end-segment [s] (second s))

(defn midpoint-segment [s]
  (let [p (start-segment s)
        q (end-segment s)]
    (make-point (avg (x-point p) (x-point q))
                (avg (y-point p) (y-point q)))))

(deftest tests
  (testing "compute the midpoint of a segment"
    (def p (make-point 1 2))
    (def q (make-point 4 9))
    (def m (make-point 5/2 11/2))
    (def s (make-segment p q))
    (is (= m (midpoint-segment s))))
  )
