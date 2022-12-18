(ns ch2.ex2-30
  (:require [clojure.test :refer :all]))

(defn square-tree [tree]
  (cond (not (list? tree)) (* tree tree)
        :else (map square-tree tree)))

(deftest tests
  (testing "square-tree"
    (is (= (square-tree (list 1
                              (list 2 (list 3 4) 5)
                              (list 6 7)))
           (list 1
                 (list 4 (list 9 16) 25)
                 (list 36 49)))))
)