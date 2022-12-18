(ns ch2.ex2-35
  (:require [clojure.test :refer :all]))

(defn count-leaves [xs]
  (reduce (fn [acc x] (+ acc
                         (if (list? x)
                           (count-leaves x)
                           1)))
          0
          xs))

(deftest tests
  (testing "count leaves"
    (is (= (count-leaves nil) 0))
    (is (= (count-leaves (list)) 0))
    (is (= (count-leaves (list 1 2 3 4)) 4))
    (is (= (count-leaves (list (list 1 2) 3 4)) 4))
    (is (= (count-leaves (list 1 2 (list 3 4))) 4))
    (is (= (count-leaves (list (list 1 2) (list 3 4))) 4)))
  )
