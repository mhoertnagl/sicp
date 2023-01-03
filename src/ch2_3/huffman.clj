(ns ch2_3.huffman
  (:require [clojure.test :refer :all])
  (:require [utils.lists :refer :all]))

(defn make-leaf [symbol weight] (list :leaf symbol weight))
(defn leaf? [object] (= (first object) :leaf))
(defn symbol-leaf [leaf] (second leaf))
(defn weight-leaf [leaf] (nth leaf 2))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (second tree))
(defn symbols-node [tree] (nth tree 2))
(defn weight-node [tree] (nth tree 3))

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (symbols-node tree)))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (weight-node tree)))

(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn decode [bits tree]
  (letfn [(choose-branch [bit branch]
            (cond (= bit 0) (left-branch branch)
                  (= bit 1) (right-branch branch)
                  :else (println "bad bit -- CHOOSE-BRANCH" bit)))
          (decode-1 [bits current-branch]
            (if (empty? bits)
              []
              (let [next-branch (choose-branch (first bits) current-branch)]
                (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (rest bits) tree))
                  (decode-1 (rest bits) next-branch)))))]
    (decode-1 bits tree)))

(defn adjoin-set [x set]
  (cond (empty? set)                        (list x)
        (< (weight x) (weight (first set))) (cons x set)
        :else                               (cons (first set)
                                                  (adjoin-set x (rest set)))))

(defn make-leaf-set [pairs]
  (if (empty? pairs)
    []
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair) (second pair))
                  (make-leaf-set (rest pairs))))))

; Ex 2.68
(defn encode-symbol [char tree]
  (if (leaf? tree)
    []
    (let [left (left-branch tree)
          right (right-branch tree)]
      (cond (in? (symbols left) char)
              (cons 0 (encode-symbol char left))
            (in? (symbols right) char)
              (cons 1 (encode-symbol char right))
            :else (println "bad character -- ENCODE-SYMBOL" char)))))

(defn encode [message tree]
  (if (empty? message)
    []
    (concat (encode-symbol (first message) tree)
            (encode (rest message) tree))))

; Ex 2.69
(defn successive-merge [set]
  (if (< (count set) 2)
    (first set)
    (let [left (first set)
          right (second set)
          tree (make-code-tree left right)
          remaining-set (nthrest set 2)
          new-set (adjoin-set tree remaining-set)]
      (successive-merge new-set))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

; Test Data.
(def sample-tree
  (make-code-tree
    (make-leaf "A" 4)
    (make-code-tree
      (make-leaf "B" 2)
      (make-code-tree
        (make-leaf "D" 1)
        (make-leaf "C" 1)))))

(def sample-code [0 1 1 0 0 1 0 1 0 1 1 1 0])
(def sample-message ["A" "D" "A" "B" "B" "C" "A"])

(def sample-pairs '(("A" 4), ("B" 2) ("C" 1) ("D" 1)))
(def sample-leaves  '((:leaf "D" 1) (:leaf "C" 1) (:leaf "B" 2) (:leaf "A" 4)))

(deftest tests
  (testing "leaf?"
    (is (= (leaf? (make-leaf "A" 1)) true))
    (is (= (leaf? sample-tree) false)))

  (testing "symbols"
    (is (= (symbols sample-tree) ["A" "B" "D" "C"])))

  (testing "make-leaf-set"
    (is (= (make-leaf-set sample-pairs) sample-leaves)))

  (testing "Ex 2.67"
    (is (= (decode sample-code sample-tree) sample-message)))

  (testing "Ex 2.68"
    (is (= (encode sample-message sample-tree) sample-code)))

  (testing "Ex 2.69"
    (is (= (generate-huffman-tree sample-pairs) sample-tree))
    )
)
