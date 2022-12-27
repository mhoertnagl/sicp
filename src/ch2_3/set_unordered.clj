(ns ch2_3.set_unordered
  (:require [clojure.test :refer :all]))

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        :else (element-of-set? x (rest set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn intersection-set [set1 set2]
  (cond (or (empty? set1) (empty? set2)) '()
        (element-of-set? (first set1) set2)
          (cons (first set1)
                (intersection-set (rest set1) set2))
        :else (intersection-set (rest set1) set2)))

; Ex 2.59
(defn union-set [set1 set2]
  (cond (and (empty? set1) (empty? set2)) '()
        (empty? set1) set2
        (empty? set2) set1
        (element-of-set? (first set1) set2)
          (union-set (rest set1) set2)
        :else (cons (first set1)
                    (union-set (rest set1) set2))))

; Ex 2.60
; Improved performance for union-set, intersection-set and adjoin-set.
; Degraded performance for element-of-set?
; In mathematics sets with duplicates are called multi-sets.
;
; Perhaps a better implementation would use tuples (x, n) for each
; entry, where x is the set element and n is the number of occurrences
; of x in this set.

(deftest tests
  (testing "element-of-set?"
    (is (= (element-of-set? 1 '(2 3 4 5)) false))
    (is (= (element-of-set? 5 '(2 3 4 5)) true)))

  (testing "adjoin-set"
    (is (= (adjoin-set 1 '()) '(1)))
    (is (= (adjoin-set 1 '(2 3 4)) '(1 2 3 4))))

  (testing "intersection-set"
    (is (= (intersection-set '(1 2 3) '( 4 5 6)) '()))
    (is (= (intersection-set '(1 2 3 4) '(3 4 5 6)) '(3 4))))

  (testing "union-set"
    (is (= (union-set '(1 2 3) '( 4 5 6)) '(1 2 3 4 5 6)))
    (is (= (union-set '(1 2 3 4) '(3 4 5 6)) '(1 2 3 4 5 6)))
    )
  )


