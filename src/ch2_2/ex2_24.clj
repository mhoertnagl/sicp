(ns ch2.ex2-24 (:require [clojure.test :refer :all]))

(defn count-leaves [x]
  (cond (nil? x) 0
        (not (list? x)) 1
        (empty? x) 0
        :else (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

;; (1 (2 (3 4)))
;;  /  \
;; 1   (2 (3 4))
;;     /   \
;;    2    (3 4)
;;         /   \
;;        3     4

;; ->[o|o]->[o|x]
;;    |      |
;;    1     [o|o]->[o|x]
;;           |      |
;;           2     [o|o]->[o|x]
;;                  |      |
;;                  3      4

(deftest tests
  (testing "count leaves"
    (is (= (count-leaves nil) 0))
    (is (= (count-leaves (list)) 0))
    (is (= (count-leaves (list 1 2 3 4)) 4))
    (is (= (count-leaves (list (list 1 2) 3 4)) 4))
    (is (= (count-leaves (list 1 2 (list 3 4))) 4))
    (is (= (count-leaves (list (list 1 2) (list 3 4))) 4)))
    ;; Evaluator prints 4 as expected.
    (println (count-leaves (list 1 (list 2 (list 3 4)))))
)