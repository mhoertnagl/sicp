(ns ch2.ex2-33
  (:require [clojure.test :refer :all]))

(defn my-map [f xs]
  (reduce (fn [ys y] (conj ys (f y))) [] xs))

(defn my-concat [xs ys]
  (reduce conj ys (reverse xs)))

(defn my-len [xs]
  (reduce (fn [acc x] (inc acc)) 0 xs))

(deftest tests
  (testing "my-map"
    (is (= (my-map (fn [x] (* x x)) (list 1 2 3))
           (list 1 4 9))))

  (testing "my-concat"
    (is (= (my-concat (list 1 2 3) (list 4 5 6))
           (list 1 2 3 4 5 6))))

  (testing "my-len"
    (is (= (my-len (list 1 2 3 4))
           4)))
)