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
  (stream-of (fn [x y] (if (> x y) nil [x (inc x) y])) a b))

;(defn stream-interval [a b]
;  (if (> a b) nil [a (delay (stream-interval (inc a) b))]))

(defn stream-first [s] (first s))
(defn stream-rest [s] @(second s))

(defn stream-nth [s n]
  (if (zero? n)
    (stream-first s)
    (stream-nth (stream-rest s) (dec n))))

;(defn stream-map [f s]
;  (if (nil? s) nil
;               [(f (stream-first s))
;                (delay (stream-map f (stream-rest s)))]))

(defn stream-filter [f s]
  (cond (nil? s) nil
        (f (stream-first s)) [(stream-first s)
                              (delay (stream-filter f (stream-rest s)))]
        :else (stream-filter f (stream-rest s))))

; Ex 3.50
(defn stream-map [f & streams]
  (if (some nil? streams) nil
               [(apply f (map stream-first streams))
                (delay (apply stream-map f (map stream-rest streams)))]))

(defn integers-starting-from [n] (stream-of (fn [x] [x (inc x)]) n))
(def integers (integers-starting-from 1))

(defn divisible? [x y] (zero? (rem x y)))

(defn fib-gen [a b] (stream-of (fn [x y] [x y (+ x y)]) a b))
(def fibs (fib-gen 0 1))

(defn sieve [s]
  [(stream-first s)
   (delay (sieve (stream-filter (fn [x] (not (divisible? x (stream-first s))))
                                (stream-rest s))))])

(def primes (sieve (integers-starting-from 2)))

(defn spy [x]
  (println x)
  x)

(deftest tests
  (testing "stream-interval"
    (let [s (stream-interval 1 3)]
      (is (= (stream-first s) 1))
      (is (= (stream-first (stream-rest s)) 2))
      (is (= (stream-first (stream-rest (stream-rest s))) 3))
      (is (= (stream-first (stream-rest (stream-rest (stream-rest s)))) nil))))

  (testing "stream-nth"
    (let [s (stream-interval 1 10)]
      (is (= (stream-nth s 0) 1))
      (is (= (stream-nth s 3) 4))
      (is (= (stream-nth s 9) 10))
      (is (= (stream-nth s 2) 3))))

  (testing "stream-map"
    (let [s (stream-map inc (stream-interval 1 3))]
      (is (= (stream-first s) 2))
      (is (= (stream-first (stream-rest s)) 3))
      (is (= (stream-first (stream-rest (stream-rest s))) 4))
      (is (= (stream-first (stream-rest (stream-rest (stream-rest s)))) nil))))

  (testing "stream-map multi"
    (let [s (stream-map +
                        (stream-interval 1 3)
                        (stream-interval 2 5))]
      (is (= (stream-first s) 3))
      (is (= (stream-first (stream-rest s)) 5))
      (is (= (stream-first (stream-rest (stream-rest s))) 7))
      (is (= (stream-first (stream-rest (stream-rest (stream-rest s)))) nil))))

  (testing "stream-filter"
    (let [s (stream-filter even? (stream-interval 1 7))]
      (is (= (stream-first s) 2))
      (is (= (stream-first (stream-rest s)) 4))
      (is (= (stream-first (stream-rest (stream-rest s))) 6))
      (is (= (stream-first (stream-rest (stream-rest (stream-rest s)))) nil))))

  ; Ex 3.51
  ; Displays
  ;   0
  ;   1
  ;   2
  ;   3
  ;   4
  ;   5
  ;   6
  ;   7
  ; The values 0 through 5 are not re-evaluated.
  ; In Clojure, delay implements a memoization mechanism.
  ;(testing "Ex 3.51"
  ;  (let [s (stream-map spy (stream-interval 0 10))]
  ;    (stream-nth s 5)
  ;    (stream-nth s 7)))

  ; Ex 3.52
  ; Skipped ;)

  (testing "primes"
    (is (= (stream-nth primes 50) 233)))
  )

