(ns ch2_4.complex
  (:require [clojure.test :refer :all])
  (:require [utils.lists :refer :all]))

(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(defn attach-tag [type-tag contents] (list type-tag contents))

(defn type-tag [datum]
  (if (pair? datum)
    (first datum)
    (println "Bad tagged datum -- TYPE-TAG" datum)))

(defn contents [datum]
  (if (pair? datum)
    (second datum)
    (println "Bad tagged datum -- CONTENTS" datum)))

(deftest tests
  (testing "dot-product"
    (is (= (dot-product [1 2 3] [4 5 6]) 32)))

  (testing "matrix-*-vector"
    (is (= (matrix-*-vector [[1 2 3] [4 5 6] [7 8 9]] [1 1 1]) [6 15 24])))

  (testing "transpose"
    (is (= (transpose [[1 2 3] [4 5 6] [7 8 9]])
           [[1 4 7] [2 5 8] [3 6 9]])))

  (testing "matrix-*-matrix"
    (is (= (matrix-*-matrix [[1 2] [4 5]] [[1 2] [4 5]])
           [[9 12] [24 33]])))
)
