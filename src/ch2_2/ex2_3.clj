(ns ch2_2.ex2-3
  (:require [clojure.test :refer :all])
  (:require [ch2_2.ex2-2 :refer :all]))

; Rectangle specified by its diagonal segment.
(defn make-rectangle-2
  "Defines a rectangle in terms of its diagonal segment s. Where the
   start of the segment is the top-left corner and the end of the
   segment denotes the bottom-right corner."
  [s]
  s)

; Rectangle implemented as a pair of points.
(defn make-rectangle-1
  "Defines a rectangle in terms of its top-left (p) and bottom-right (q)
   corners, where q has to be greater that p in both coordinates. The
   tow points then form a diagonal segment."
  [p q]
  (make-segment p q))

(defn rectangle-width [r]
  (let [s (start-segment r)
        e (end-segment r)]
    (- (x-point e) (x-point s))))

(defn rectangle-height [r]
  (let [s (start-segment r)
        e (end-segment r)]
    (- (y-point e) (y-point s))))

(defn rectangle-area [r]
  (* (rectangle-width r) (rectangle-height r)))

(defn rectangle-perimeter [r]
  (* 2 (+ (rectangle-width r) (rectangle-height r))))

(deftest tests
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
