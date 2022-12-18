(ns ch2_2.ex2-27 (:require [clojure.test :refer :all]))

(defn deep-reverse [x]
  (cond (nil? x) nil
        (not (list? x)) x
        (empty? x) (list)
        :else (concat (deep-reverse (rest x))
                      (list (deep-reverse (first x))))))

(deftest tests
  (testing "deep-reverse"
    (is (= (deep-reverse nil) nil))
    (is (= (deep-reverse ()) ()))
    (is (= (deep-reverse (list 1 2 3 4)) (list 4 3 2 1)))
    (is (= (deep-reverse (list (list 1 2) (list 3 4))) (list (list 4 3) (list 2 1)))))
)