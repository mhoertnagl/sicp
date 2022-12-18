(ns ch2.ex2-34
  (:require [clojure.test :refer :all]))

(defn horner-eval [x as]
  (reduce (fn [acc a] (+ (* acc x) a)) (reverse as)))

(deftest tests
  (testing "horner-eval"
    (is (= (horner-eval 2 (list 1 3 0 5 0 1))
           79)))
)
