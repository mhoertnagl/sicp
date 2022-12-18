(ns ch2_2.ex2-23 (:require [clojure.test :refer :all]))

(defn for-each [f xs]
  (if (empty? xs)
    nil
    (do
      (f (first xs))
      (recur f (rest xs)))))

(deftest tests
  (testing "for-each print"
    (for-each (fn [x] (println x)) [1 2 3 4]))
)