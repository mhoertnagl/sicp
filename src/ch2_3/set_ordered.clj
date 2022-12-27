(ns ch2_3.set_ordered
  (:require [clojure.test :refer :all]))

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        (< x (first set)) false
        :else (element-of-set? x (rest set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn intersection-set [set1 set2]
  (if (or (empty? set1) (empty? set2))
    '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond (= x1 x2) (cons x1 (intersection-set (rest set1) (rest set2)))
            (< x1 x2) (intersection-set (rest set1) set2)
            :else (intersection-set set1 (rest set2))))))

; Ex 2.61
; This is the same implementation as in the unordered set but
; element-of-set? takes advantage of the speedup by a factor of 2.
(defn union-set [set1 set2]
  (cond (and (empty? set1) (empty? set2)) '()
        (empty? set1) set2
        (empty? set2) set1
        (element-of-set? (first set1) set2)
          (union-set (rest set1) set2)
        :else (cons (first set1)
                    (union-set (rest set1) set2))))

; Ex 2.62
(defn union-set [set1 set2]
  (cond (and (empty? set1) (empty? set2)) '()
        (empty? set1) set2
        (empty? set2) set1
        :else (let [x1 (first set1)
                    x2 (first set2)]
                (cond (= x1 x2)))))

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


