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

(deftest tests
  (testing "leafs"
    (is (= (leaf? (make-leaf "A" 1)) true))
    (is (= (leaf? sample-tree) false)))

  (testing "symbols"
    (is (= (symbols sample-tree) ["A" "B" "D" "C"])))

  (testing "Ex 2.67"
    (is (= (decode sample-code sample-tree) sample-message)))

  (testing "Ex 2.68"
    (is (= (encode sample-message sample-tree) sample-code))
    )
)
