(ns ch2_2.ex2-29
  (:require [clojure.test :refer :all]))

(defn make-mobile [left right] (list left right))
(defn make-branch [length structure] (list length structure))

;; a)
(defn left-branch [mobile] (first mobile))
(defn right-branch [mobile] (second mobile))
(defn branch-length [branch] (first branch))
(defn branch-structure [branch] (second branch))

;; b)
(declare total-weight-branch)

(defn total-weight [mobile]
  (+ (total-weight-branch (left-branch mobile))
     (total-weight-branch (right-branch mobile))))

(defn total-weight-branch [branch]
  (let [structure (branch-structure branch)]
    (cond (not (list? structure)) structure
          (empty? structure) 0
          :else (total-weight structure))))

;; c)
(defn mobile-balanced? [mobile]
  (cond (not (list? mobile)) true
        :else (let [left-branch (left-branch mobile)
                    right-branch (right-branch mobile)
                    left-torque (* (branch-length left-branch)
                                   (total-weight-branch left-branch))
                    right-torque (* (branch-length right-branch)
                                    (total-weight-branch right-branch))]
                (and (= left-torque right-torque)
                     (mobile-balanced? (branch-structure left-branch))
                     (mobile-balanced? (branch-structure right-branch))))))

(deftest tests
  (testing "total weight"
    (let [x (make-mobile (make-branch 1 4)
                         (make-branch 1 (make-mobile (make-branch 1 3)
                                                     (make-branch 1 1))))]
      (is (= (total-weight x)
             8))))

  (testing "not balanced"
    (let [x (make-mobile (make-branch 1 4)
                         (make-branch 1 (make-mobile (make-branch 1 3)
                                                     (make-branch 1 1))))]
      (is (= (mobile-balanced? x)
             false))))

  (testing "balanced"
    (let [x (make-mobile (make-branch 1 4)
                         (make-branch 1 (make-mobile (make-branch 1 3)
                                                     (make-branch 3 1))))]
      (is (= (mobile-balanced? x)
             true))))
)