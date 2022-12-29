(ns ch2_3.set_btree
  (:require [clojure.test :refer :all])
  (:require [ch2_3.set_ordered :as ordered]))

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

; Ex 2.64
; Divide the ordered list in halve. The central element will be the root
; entry of the tree. Recursively call partial-tree for the left and right
; sublist. The algorithm runs in O(n).
(defn partial-tree [xs n]
  (if (= n 0)
    (cons nil xs)
    (let [left-size (quot (- n 1) 2)
          right-size (- n (+ left-size 1))
          left-result (partial-tree xs left-size)
          left-tree (first left-result)
          non-left-xs (rest left-result)
          this-entry (first non-left-xs)
          right-result (partial-tree (rest non-left-xs) right-size)
          right-tree (first right-result)
          remaining-xs (rest right-result)]
      (cons (make-tree this-entry left-tree right-tree)
            remaining-xs))))

; Converts an ordered list of elements xs into a balanced binary tree.
(defn list->tree [xs] (first (partial-tree xs (count xs))))

; Ex 2.65
; Turn the trees into ordered lists. Apply the intersection-set/union-set
; procedures for ordered lists. Then turn the result into a balanced tree.
(defn intersection-set [tree1 tree2]
  (list->tree (ordered/intersection-set (tree->list-2 tree1)
                                        (tree->list-2 tree2))))

(defn union-set [tree1 tree2]
  (list->tree (ordered/union-set (tree->list-2 tree1)
                                 (tree->list-2 tree2))))

; Ex 2.66
(defn make-node [key value] (list key value))

(defn node-key [node] (first node))
(defn node-value [node] (second node))

(defn lookup [given-key tree]
  (let [this-key (node-key (entry tree))]
    (cond (empty? tree)          false
          (= given-key this-key) (node-value (entry tree))
          (< given-key this-key) (lookup given-key (left-branch tree))
          :else                  (lookup given-key (right-branch tree)))))

; Test Data
(def tree1 '(7 (3 (1 nil nil)
                  (5 nil nil))
               (9 nil
                  (11 nil nil))))

(def tree2 '(3 (1 nil nil)
               (7 (5 nil nil)
                  (9 nil
                     (11 nil nil)))))

(def tree3 '(5 (3 (1 nil nil)
                  nil)
               (9 (7 nil nil)
                  (11 nil nil))))

(def tree4 '(5 (3 nil nil)
               (8 nil nil)))

(def tree5 '(5 (1 nil nil)
               (8 nil nil)))

(def tree6 '(3 (1 nil nil)
               (5 nil
                  (8 nil nil))))

(def tree7 '(5 nil
               (8 nil nil)))

(def tree8 (make-tree (make-node 5 "5")
                      (make-tree (make-node 3 "3") nil nil)
                      (make-tree (make-node 7 "7") nil nil)))

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

  (testing "list->tree"
    (is (= (list->tree '(1 3 5 7 9 11))
           '(5 (1 nil
                 (3 nil nil))
              (9 (7 nil nil)
                (11 nil nil))))))

  (testing "intersection-set"
    (is (= (intersection-set tree4 tree5) tree7)))

  (testing "union-set"
    (is (= (union-set tree4 tree5) tree6)))

  (testing "lookup"
    (is (= (lookup 5 tree8) "5"))
    (is (= (lookup 3 tree8) "3"))
    (is (= (lookup 7 tree8) "7"))
    (is (= (lookup 2 tree8) false)))
  )


