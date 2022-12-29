(ns ch2_3.set_btree
  (:require [clojure.test :refer :all]))

(defn entry [tree] (first tree))

(defn left-branch [tree] (second tree))

(defn right-branch [tree] (nth tree 2))

(defn make-tree [entry left right] (list entry left right))

(defn element-of-set? [x set]
  (cond (empty? set)      false
        (= x (entry set)) true
        (< x (entry set)) (element-of-set? x (left-branch set))
        :else             (element-of-set? x (right-branch set))))

(defn adjoin-set [x set]
  (cond (empty? set)      (make-tree x nil nil)
        (= x (entry set)) set
        (< x (entry set)) (make-tree (entry set)
                                     (adjoin-set x (left-branch set))
                                     (right-branch set))
        :else             (make-tree (entry set)
                                     (left-branch set)
                                     (adjoin-set x (right-branch set)))))

; Ex 2.63
;

;(defn intersection-set [set1 set2]
;  (if (or (empty? set1) (empty? set2))
;    '()
;    (let [x1 (first set1)
;          x2 (first set2)]
;      (cond (= x1 x2) (cons x1 (intersection-set (rest set1) (rest set2)))
;            (< x1 x2) (intersection-set (rest set1) set2)
;            :else (intersection-set set1 (rest set2))))))
;
;; Ex 2.61
;; This is the same implementation as in the unordered set but
;; element-of-set? takes advantage of the speedup by a factor of 2.
;(defn union-set [set1 set2]
;  (cond (and (empty? set1) (empty? set2)) '()
;        (empty? set1) set2
;        (empty? set2) set1
;        (element-of-set? (first set1) set2)
;          (union-set (rest set1) set2)
;        :else (cons (first set1)
;                    (union-set (rest set1) set2))))
;
;; Ex 2.62
;(defn union-set [set1 set2]
;  (cond (and (empty? set1) (empty? set2)) '()
;        (empty? set1) set2
;        (empty? set2) set1
;        :else (let [x1 (first set1)
;                    x2 (first set2)]
;                (cond (= x1 x2) (cons x1 (union-set (rest set1) (rest set2)))
;                      (< x1 x2) (cons x1 (union-set (rest set1) set2))
;                      :else (cons x2 (union-set set1 (rest set2)))))))

(def tree1 '(7 (3 (1 nil nil)
                  (5 nil nil))
               (9 nil
                  (11 nil nil))))

(def tree2 '(3 (1 nil nil)
               (7 (5 nil nil)
                  (9 nil
                     (11 nil nil)))))

(def tree3 '(5 (3 nil
                  (1 nil nil))
               (9 (7 nil nil)
                  (11 nil nil))))

(defn tree->list-1 [tree]
  (if (empty? tree)
    '()
    (concat (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(defn tree->list-2 [tree]
  (letfn [(copy-to-list [tree result]
            (if (empty? tree)
              result
              (copy-to-list (left-branch tree)
                            (cons (entry tree)
                                  (copy-to-list (right-branch tree)
                                                result)))))]
    (copy-to-list tree '())))

(deftest tests
  (testing "element-of-set?"
    (is (= (element-of-set? 1 tree1) true))
    (is (= (element-of-set? 10 tree1) false)))

  (testing "Ex 2.63 a)"
    (is (= (tree->list-1 tree1)
           (tree->list-2 tree1)))
    (is (= (tree->list-1 tree2)
           (tree->list-2 tree2)))
    (is (= (tree->list-1 tree3)
           (tree->list-2 tree3))))

  ; Ex 2.63 b)
  ; tree->list-1 grows more slowly.

  ;(testing "adjoin-set"
  ;  (is (= (adjoin-set 1 '()) '(1)))
  ;  (is (= (adjoin-set 1 '(2 3 4)) '(1 2 3 4))))

  ;(testing "intersection-set"
  ;  (is (= (intersection-set '(1 2 3) '( 4 5 6)) '()))
  ;  (is (= (intersection-set '(1 2 3 4) '(3 4 5 6)) '(3 4))))
  ;
  ;(testing "union-set"
  ;  (is (= (union-set '(1 2 3) '( 4 5 6)) '(1 2 3 4 5 6)))
  ;  (is (= (union-set '(1 2 3 4) '(3 4 5 6)) '(1 2 3 4 5 6)))
  ;  )
  )


