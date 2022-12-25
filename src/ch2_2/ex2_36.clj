(ns ch2_2.ex2-36
  (:require [clojure.test :refer :all]))

(defn accumulate-n [op init xs]
  (if (empty? (first xs))
    []
    (cons (reduce op init (map first xs))
          (accumulate-n op init (map rest xs)))))

(deftest tests
  (testing "sum"
    (is (= (accumulate-n + 0 [[1 2 3] [4 5 6] [7 8 9] [10 11 12]]) [22 26 30])))
)
