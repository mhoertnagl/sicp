(ns ch3_5.streams
  (:require [clojure.test :refer :all]))

;; TODO: issue with stream-interval. Returns a lazy-seq of nil instead of nil.

(defn stream-interval [a b]
  (if (> a b)
    nil
    (cons a (lazy-seq (stream-interval (inc a) b)))))

(defn stream-nth [s n]
  (if (zero? n)
    (first s)
    (stream-nth (lazy-seq (rest s))
                (dec n))))

(defn stream-filter [f s]
  (cond (nil? s) nil
        (nil? (first s)) nil
        (f (first s)) (cons (first s)
                            (lazy-seq (stream-filter f (rest s))))
        :else (stream-filter f (rest s))))

; Ex 3.50
(defn stream-map [f & streams]
  (if (some nil? streams)
    nil
    (let [vs (map first streams)]
      (if (some nil? vs)
        nil
        (cons (apply f vs)
              (lazy-seq (apply stream-map f (map rest streams))))))))

(defn integers
  ([] (integers 1))
  ([n] (lazy-seq (cons n (integers (inc n))))))

(defn divisible? [x y] (zero? (rem x y)))

(defn fibs
  ([] (fibs 0 1))
  ([a b] (lazy-seq (cons a (fibs b (+ a b))))))

(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (stream-filter #(not (divisible? % (first s)))
                                        (rest s))))))

(def primes (sieve (integers 2)))

(defn spy [x]
  (println x)
  x)

(deftest tests
  (testing "primes"
    (is (= (first (integers)) 1))
    (is (= (first (rest (integers))) 2)))

  (testing "stream-interval"
    (let [s (stream-interval 1 3)]
      (is (= (first s) 1))
      (is (= (first (rest s)) 2))
      (is (= (first (rest (rest s))) 3))
      (is (= (first (rest (rest (rest s)))) nil))))

  (testing "stream-nth"
    (let [s (stream-interval 1 10)]
      (is (= (stream-nth s 0) 1))
      (is (= (stream-nth s 3) 4))
      (is (= (stream-nth s 9) 10))
      (is (= (stream-nth s 2) 3))
      (is (= (stream-nth s 10) nil))))

  (testing "stream-nth of integers"
    (let [s (integers)]
      (is (= (stream-nth s 0) 1))
      (is (= (stream-nth s 3) 4))
      (is (= (stream-nth s 9) 10))
      (is (= (stream-nth s 2) 3))))

  (testing "stream-map"
    (let [s (stream-map inc (stream-interval 1 3))]
      (is (= (stream-nth s 0) 2))
      (is (= (stream-nth s 1) 3))
      (is (= (stream-nth s 2) 4))
      (is (= (stream-nth s 3) nil))
      ))

  (testing "stream-map multi"
    (let [s (stream-map +
                        (stream-interval 1 3)
                        (stream-interval 2 5))]
      (is (= (stream-nth s 0) 3))
      (is (= (stream-nth s 1) 5))
      (is (= (stream-nth s 2) 7))
      (is (= (stream-nth s 3) nil))
      ))
  ;
  (testing "stream-filter"
    (let [s (stream-filter even? (stream-interval 1 7))]
      (is (= (stream-nth s 0) 2))
      (is (= (stream-nth s 1) 4))
      (is (= (stream-nth s 2) 6))
      (is (= (stream-nth s 3) nil))
      ))

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
  ; In Clojure, lazy-val implements a memoization mechanism.
  ;(testing "Ex 3.51"
  ;  (let [s (stream-map spy (stream-interval 0 10))]
  ;    (stream-nth s 5)
  ;    (stream-nth s 7)))

  ; Ex 3.52
  ; Skipped ;)

  (testing "primes"
    (is (= (stream-nth primes 50) 233)))

  )

