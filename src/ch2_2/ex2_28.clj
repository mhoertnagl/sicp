(ns ch2_2.ex2-28 (:require [clojure.test :refer :all]))

(defn fringe [x]
  (cond (nil? x) nil
        (not (list? x)) (list x)
        (empty? x) (list)
        :else (concat (fringe (first x))
                      (fringe (rest x)))))

(deftest tests
  (testing "fringe"
    (is (= (fringe nil)
           nil))
    (is (= (fringe ())
           ()))
    (is (= (fringe (list 1 2 3 4))
           (list 1 2 3 4)))
    (is (= (fringe (list (list 1 2) (list 3 4)))
           (list 1 2 3 4)))
    (is (= (fringe (list (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4))))
           (list 1 2 3 4 1 2 3 4))))
)