(ns ch2_2.ex2-37
  (:require [clojure.test :refer :all])
  (:require [ch2_2.ex2-36 :refer :all]))

(defn dot-product [v w]
  (reduce + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map (fn [r] (dot-product r v)) m))

(defn transpose [m]
  (accumulate-n conj [] m))

(defn matrix-*-matrix [m n]
  (let [cs (transpose n)]
    (map (fn [r] (matrix-*-vector cs r)) m)))

(deftest tests2
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
