(ns ch3_5.streams
  (:require [clojure.test :refer :all]))

;; https://clojuredocs.org/clojure.core/delay

;(defn stream-of [func args]
;  (let [res (apply func args)]
;    (if (nil? res)
;      nil
;      [(first res)
;       (delay (stream-of func (rest res)))])))
;
;(defn stream-interval [a b]
;  (letfn [(func [x y] (if (> x y) nil [x (inc x) y]))]
;    (stream-of func [a b])))

(defn stream-of [f & args]
  (let [nxt (apply f args)]
    (if (nil? nxt) nil
                   [(first nxt)
                    (delay (apply stream-of f (rest nxt)))])))

(defn stream-interval [a b]
  (letfn [(func [x y] (if (> x y) nil [x (inc x) y]))]
    (stream-of func a b)))

;(defn stream-interval [a b]
;  (if (> a b) nil [a (delay (stream-interval (inc a) b))]))

(defn stream-first [s] (first s))
(defn stream-rest [s] @(second s))

(defn stream-map [f s]
  (if (nil? s) nil
               [(f (stream-first s))
                (delay (stream-map f (stream-rest s)))]))

(defn stream-filter [f s]
  (cond (nil? s) nil
        (f (stream-first s)) [(stream-first s)
                              (delay (stream-filter f (stream-rest s)))]
        :else (stream-filter f (stream-rest s))))

; Ex 3.50
; TODO: Unit tests.
(defn stream-map [f & streams]
  (if (some nil? streams) nil
               [(apply f (map stream-first streams))
                (delay (apply stream-map f (map stream-rest streams)))]))

(deftest tests
  (testing "stream-interval"
    (let [s (stream-interval 1 3)]
      (is (= (stream-first s) 1))
      (is (= (stream-first (stream-rest s)) 2))
      (is (= (stream-first (stream-rest (stream-rest s))) 3))
      (is (= (stream-first (stream-rest (stream-rest (stream-rest s)))) nil)))
    )

  (testing "stream-map"
    (let [s (stream-map inc (stream-interval 1 3))]
      (is (= (stream-first s) 2))
      (is (= (stream-first (stream-rest s)) 3))
      (is (= (stream-first (stream-rest (stream-rest s))) 4))
      (is (= (stream-first (stream-rest (stream-rest (stream-rest s)))) nil)))
    )

  (testing "stream-filter"
    (let [s (stream-filter even? (stream-interval 1 7))]
      (is (= (stream-first s) 2))
      (is (= (stream-first (stream-rest s)) 4))
      (is (= (stream-first (stream-rest (stream-rest s))) 6))
      (is (= (stream-first (stream-rest (stream-rest (stream-rest s)))) nil)))
    )
  )

