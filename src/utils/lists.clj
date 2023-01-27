(ns utils.lists
  (:require [clojure.test :refer :all]))

(defn in?
  "true if coll contains elm"
  [coll elm] (some #(= elm %) coll))

(defn pair?
  "true if x is a pair"
  [x] (and (sequential? x) (= (count x) 2)))

(deftest tests
  (testing "pair?"
    (is (= (pair? nil) false))
    (is (= (pair? []) false))
    (is (= (pair? '()) false))
    (is (= (pair? [1]) false))
    (is (= (pair? '(1)) false))
    (is (= (pair? [1 2]) true))
    (is (= (pair? '(1 2)) true))
    (is (= (pair? [1 2 3]) false))
    (is (= (pair? '(1 2 3)) false))
    )
)