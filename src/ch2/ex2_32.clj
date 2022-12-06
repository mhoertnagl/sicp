(ns ch2.ex2-32
  (:require [clojure.test :refer :all]))

;; We concatenate recursively two subsets. On the one hand
;; the subsets of the set without the first element and on
;; the other hand this subset concatenated with the first
;; element.
(defn subsets [s]
  (if (empty? s)
    (list nil)
    (let [fst (first s)
          rst (subsets (rest s))]
      (concat rst (map (fn [t] (conj t fst)) rst)))))

(deftest tests
  (testing "subsets"
    (is (= (subsets (list 1 2 3))
           (list nil
                 (list 3)
                 (list 2)
                 (list 2 3)
                 (list 1)
                 (list 1 3)
                 (list 1 2)
                 (list 1 2 3)))))
)