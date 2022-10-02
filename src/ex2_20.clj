(ns ex2-20 (:require [clojure.test :refer :all]))

(defn same-parity [x & xs]
  (filter (if (even? x) even? odd?) (conj xs x)))

(deftest tests
  (testing "same-parity odd"
    (is (= (same-parity 1 2 3 4 5 6 7) [1 3 5 7])))

  (testing "same-parity even"
    (is (= (same-parity 2 3 4 5 6 7) [2 4 6])))
)