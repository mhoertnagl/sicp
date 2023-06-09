(ns ch3_5.streams
  (:require [clojure.test :refer :all]))

;; TODO: issue with stream-interval. Returns a lazy-seq of nil instead of nil.

(defn stream-nil? [s]
  (or (nil? s) (nil? (first s))))

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
  (cond (stream-nil? s) nil
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

(def ones (cons 1 (lazy-seq ones)))

(defn stream-sum [& streams]
  (apply stream-map + streams))

(def integers-2
  (cons 1
        (lazy-seq (stream-sum ones
                              integers-2))))

(def fibs-2
  (cons 0
        (cons 1
              (lazy-seq (stream-sum (rest fibs-2)
                                    fibs-2)))))

(defn stream-scale [stream factor]
  (stream-map #(* % factor) stream))

; Ex 3.53
; Defines the stream of powers of 2.

(defn stream-mul [& streams]
  (apply stream-map * streams))

; Ex 3.54
(def factorials
  (cons 1
        (lazy-seq (stream-mul factorials
                              (integers 2)))))

; Ex 3.55
(defn stream-partial-sums [stream]
  (cons (first stream)
        (lazy-seq (stream-sum (rest stream)
                              (stream-partial-sums stream)))))

; Ex 3.56
(defn stream-merge [s1 s2]
  (cond (stream-nil? s1) s2
        (stream-nil? s2) s1
        :else (let [v1 (first s1)
                    v2 (first s2)]
                (cond (< v1 v2) (cons v1
                                      (lazy-seq (stream-merge (rest s1) s2)))
                      (> v1 v2) (cons v2
                                      (lazy-seq (stream-merge s1 (rest s2))))
                      :else (cons v1
                                  (lazy-seq (stream-merge (rest s1) (rest s2))))))))

(def S (cons 1
             (lazy-seq (stream-merge (stream-merge (stream-scale S 2)
                                                   (stream-scale S 3))
                                     (stream-scale S 5)))))

; Ex 3.57
; We only need O(n) additions, since lazy-stream memoizes
; already realized elements of the stream.
; Using (fn [] ...) instead would result in exponential
; blow-up as we need to recalculate the streams F(n-2)
; and F(n-1) to yield the stream for F(n).

; Ex 3.58
; Computes the stream of decimal digits of num/den in
; the given radix.
; (expand 1 7 10) would thus be the stream [1 4 2 8 ...]
; (expand 3 8 10) is the finite stream [3 7 5]

; Ex 3.59 a)
(defn integrate-series [s]
  (letfn [(integrate [s n]
            (cons (/ (first s) n)
                  (lazy-seq (integrate (rest s) (inc n)))))]
    (integrate s 1)))

; Ex 3.59 b)
(def exp-series
  (cons 1
        (lazy-seq (integrate-series exp-series))))

; Clojure's way of doing mutual recursion.
(declare sine-series)

(def cosine-series
  (cons 1
        (lazy-seq (stream-scale (integrate-series sine-series) (- 1)))))

(def sine-series
  (cons 0
        (lazy-seq (integrate-series cosine-series))))

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
      (is (= (stream-nth s 3) nil))))

  (testing "stream-map multi"
    (let [s (stream-map +
                        (stream-interval 1 3)
                        (stream-interval 2 5))]
      (is (= (stream-nth s 0) 3))
      (is (= (stream-nth s 1) 5))
      (is (= (stream-nth s 2) 7))
      (is (= (stream-nth s 3) nil))))

  (testing "stream-filter"
    (let [s (stream-filter even? (stream-interval 1 7))]
      (is (= (stream-nth s 0) 2))
      (is (= (stream-nth s 1) 4))
      (is (= (stream-nth s 2) 6))
      (is (= (stream-nth s 3) nil))))

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

  (testing "ones"
    (is (= (stream-nth ones 0) 1))
    (is (= (stream-nth ones 1) 1))
    (is (= (stream-nth ones 2) 1)))

  (testing "stream-sum"
    (let [s1 (stream-interval 1 7)
          s2 (stream-interval 1 7)
          s (stream-sum s1 s2)]
      (is (= (stream-nth s 0) 2))
      (is (= (stream-nth s 1) 4))
      (is (= (stream-nth s 2) 6))
      (is (= (stream-nth s 3) 8))))

  (testing "integers-2"
    (is (= (stream-nth integers-2 0) 1))
    (is (= (stream-nth integers-2 1) 2))
    (is (= (stream-nth integers-2 2) 3)))

  (testing "fibs-2"
    (is (= (stream-nth fibs-2 0) 0))
    (is (= (stream-nth fibs-2 1) 1))
    (is (= (stream-nth fibs-2 2) 1))
    (is (= (stream-nth fibs-2 3) 2))
    (is (= (stream-nth fibs-2 4) 3))
    (is (= (stream-nth fibs-2 5) 5))
    (is (= (stream-nth fibs-2 50) 12586269025)))

  (testing "stream-scale"
    (let [t (stream-interval 1 10)
          s (stream-scale t 3)]
      (is (= (stream-nth s 0) 3))
      (is (= (stream-nth s 3) 12))
      (is (= (stream-nth s 9) 30))
      (is (= (stream-nth s 2) 9))
      (is (= (stream-nth s 10) nil))))

  (testing "factorials"
    (is (= (stream-nth factorials 0) 1))
    (is (= (stream-nth factorials 1) 2))
    (is (= (stream-nth factorials 2) 6))
    (is (= (stream-nth factorials 3) 24))
    (is (= (stream-nth factorials 4) 120))
    (is (= (stream-nth factorials 5) 720)))

  (testing "stream-partial-sums integers"
    (let [s (stream-partial-sums (integers))]
      (is (= (stream-nth s 0) 1))
      (is (= (stream-nth s 1) 3))
      (is (= (stream-nth s 2) 6))
      (is (= (stream-nth s 3) 10))
      (is (= (stream-nth s 4) 15))))

  (testing "S"
    (is (= (stream-nth S 0) 1))
    (is (= (stream-nth S 1) 2))
    (is (= (stream-nth S 2) 3))
    (is (= (stream-nth S 3) 4))
    (is (= (stream-nth S 4) 5))
    (is (= (stream-nth S 5) 6))
    (is (= (stream-nth S 6) 8))
    (is (= (stream-nth S 7) 9))
    (is (= (stream-nth S 8) 10))
    (is (= (stream-nth S 9) 12))
    (is (= (stream-nth S 10) 15)))

  (testing "integrate-series"
    (let [s (integrate-series ones)]
      (is (= (stream-nth s 0) (/ 1)))
      (is (= (stream-nth s 1) (/ 2)))
      (is (= (stream-nth s 2) (/ 3)))
      (is (= (stream-nth s 3) (/ 4)))
      (is (= (stream-nth s 4) (/ 5)))))

  (testing "exp-series"
    (let [s exp-series]
      (is (= (stream-nth s 0) (/ 1)))
      (is (= (stream-nth s 1) (/ 1)))
      (is (= (stream-nth s 2) (/ 2)))
      (is (= (stream-nth s 3) (/ 6)))
      (is (= (stream-nth s 4) (/ 24)))))

  (testing "cosine-series"
    (let [s cosine-series]
      (is (= (stream-nth s 0) 1))                           ; 1
      (is (= (stream-nth s 1) 0))                           ; 0*x
      (is (= (stream-nth s 2) (/ (- 1) (* 2))))             ; -1/2*x^2
      (is (= (stream-nth s 3) 0))                           ; 0*x^3
      (is (= (stream-nth s 4) (/ (* 4 3 2))))               ; 1/24*x^4
      (is (= (stream-nth s 5) 0))
      (is (= (stream-nth s 6) (/ (- 1) (* 6 5 4 3 2))))     ; -1/720*x^6
      (is (= (stream-nth s 7) 0))
      (is (= (stream-nth s 8) (/ (* 8 7 6 5 4 3 2))))))

  (testing "sine-series"
    (let [s sine-series]
      (is (= (stream-nth s 0) 0))                           ; 0
      (is (= (stream-nth s 1) 1))                           ; x
      (is (= (stream-nth s 2) 0))                           ; 0*x^2
      (is (= (stream-nth s 3) (/ (- 1) (* 3 2))))           ; -1/6*x^3
      (is (= (stream-nth s 4) 0))                           ; 0*x^4
      (is (= (stream-nth s 5) (/ (* 5 4 3 2))))             ; 1/120*x^5
      (is (= (stream-nth s 6) 0))
      (is (= (stream-nth s 7) (/ (- 1) (* 7 6 5 4 3 2))))
      (is (= (stream-nth s 8) 0))))

  )

