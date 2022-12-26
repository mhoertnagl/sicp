(ns ch2_3.sym_deriv
  (:require [clojure.test :refer :all]))

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

; (defn make-sum [a1 a2] '(+ ~a1 ~a2))
(defn make-sum [a1 a2]
  (cond
    (= a1 0) a2
    (= a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond
    (or (= m1 0) (= m2 0)) 0
    (= m1 1) m2
    (= m2 1) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))

(defn sum? [exp] (and (seq? exp) (= (first exp) '+)))

(defn addend [s] (second s))

; Ex 2.57
(defn augend [s]
  (if (= (count s) 3)
    (nth s 2)
    (cons '+ (nthrest s 2))))

(defn product? [exp] (and (seq? exp) (= (first exp) '*)))

(defn multiplier [p] (second p))

; Ex 2.57
(defn multiplicand [p]
  (if (= (count p) 3)
    (nth p 2)
    (cons '* (nthrest p 2))))

; Ex 2.56
(defn make-exponentiation [b e]
  (cond
    (= e 0) 1
    (= e 1) b
    :else (list '** b e)))

(defn exponentiation? [exp]
  (and (list? exp) (= (first exp) '**)))

(defn base [e] (second e))

(defn exponent [e] (nth e 2))

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (variable? exp)
      (if (same-variable? exp var) 1 0)
    (sum? exp)
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var))
    (product? exp)
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp)))
    (exponentiation? exp)
      (let [b (base exp)
            e (exponent exp)]
        (make-product e
                      (make-product (make-exponentiation b (make-sum e -1))
                                    (deriv b var))))
    :else (println "unknown expression type -- DERIV" exp)))

(deftest tests
  (testing "same-variable?"
    (is (= (same-variable? 'x 'y) false))
    (is (= (same-variable? 'x 'x) true)))

  (testing "make-sum"
    (is (= (make-sum 'x 'y) '(+ x y))))

  (testing "make-product"
    (is (= (make-product 'x 'y) '(* x y))))

  (testing "sum?"
    (is (= (sum? '(* 1 2)) false))
    (is (= (sum? '(+ 1 2)) true)))

  (testing "addend,augend"
    (is (= (addend '(+ 1 2)) 1))
    (is (= (augend '(+ 1 2)) 2))
    (is (= (addend '(+ x y)) 'x))
    (is (= (augend '(+ x y)) 'y)))

  (testing "product?"
    (is (= (product? '(* 1 2)) true))
    (is (= (product? '(+ 1 2)) false)))

  (testing "multiplier,multiplicand"
    (is (= (multiplier '(* 1 2)) 1))
    (is (= (multiplicand '(* 1 2)) 2)))

  (testing "deriv"
    (is (= (deriv 1 'x) 0))
    (is (= (deriv '(+ x 3) 'x) 1))
    (is (= (deriv '(* x y) 'x) 'y))
    (is (= (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))))

  (testing "ex2.56"
    (is (= (deriv '(** x 3) 'x) '(* 3 (** x 2))))
    (is (= (deriv '(** x 2) 'x) '(* 2 x)))
    (is (= (deriv '(** x 1) 'x) 1)))

  (testing "ex2.57"
    (is (= (deriv '(+ (* 3 x) (* 2 x) x) 'x) 6))
    (is (= (deriv '(* 3 2 x) 'x) 6))
    )
)
