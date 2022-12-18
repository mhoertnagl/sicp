(ns ch2_2.ex2-31
  (:require [clojure.test :refer :all]))

(defn tree-map [f tree]
  (cond (not (list? tree)) (f tree)
        :else (map (fn [t] (tree-map f t)) tree)))

(defn square-tree [tree] (tree-map (fn [x] (* x x)) tree))

(deftest tests
  (testing "square-tree"
    (is (= (square-tree (list 1
                              (list 2 (list 3 4) 5)
                              (list 6 7)))
           (list 1
                 (list 4 (list 9 16) 25)
                 (list 36 49)))))
)