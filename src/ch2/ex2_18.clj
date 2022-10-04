(ns ch2.ex2-18 (:require [clojure.test :refer :all]))

(defn my-reverse [xs]
  (case (count xs)
    0 []
    (conj (my-reverse (rest xs)) (first xs))))

(deftest tests
   (testing "last-pair returns the last element in a sequence"
            (is (= (my-reverse [1 2 3]) [3 2 1])))
 )