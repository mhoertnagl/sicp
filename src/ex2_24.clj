(ns ex2-24 (:require [clojure.test :refer :all]))

(defn count-leaves [xs]
  (cond (nil? xs) 0
        (not (vector? (first xs))) 1
        :else (+ (count-leaves (first xs))
                 (count-leaves (rest xs)))))

(deftest tests
  (testing "count leaves"
    (is (= (count-leaves [[1 2] 3 4]) 4)))
)