(ns ch2_2.ex2-17 (:require [clojure.test :refer :all]))

(defn last-pair [xs]
  (case (count xs)
    0 (throw (IllegalArgumentException. "Vector is empty"))
    1 xs
    (last-pair (rest xs))))

(deftest exercise-2-17
  (testing "last-pair returns the last element in a sequence"
    (is (= (last-pair [1 2 3]) [3])))
)