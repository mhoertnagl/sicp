(ns ex2-17-test
  (:require [clojure.test :refer :all])
  (:require [ex2-17 :refer :all]))

(deftest exercise-2-17
  (testing "last-pair returns the last element in a sequence"
    (is (= (last-pair [1 2 3]) [3])))
)