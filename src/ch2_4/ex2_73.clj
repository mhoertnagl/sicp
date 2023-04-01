(ns ch2_4.ex2_73
  (:require [clojure.test :refer :all])
  (:require [utils.lists :refer :all]))

; Ex 2.73 a)
; Numbers and variables are entered as-is without any tags associated.
; Moreover, we cannot define an entry for every possible number or
; variable in the dispatch table.

; Ex 2.73 b)
(defn install-deriv-package []
  (letfn [(sum [exp var] (make-sum (deriv (addend exp) var)
                                   (deriv (augend exp) var)))

          (prod [exp var] (make-sum (make-product (multiplier exp)
                                                  (deriv (multiplicand exp) var))
                                    (make-product (deriv (multiplier exp) var)
                                                  (multiplicand exp))))]

    (put :deriv :+ sum)
    (put :deriv :* prod)

    :done))

; Ex 2.73 c)
(defn install-exp-deriv-package []
  (letfn [ (exponent [exp var] (let [b (base exp)
                                     e (exponent exp)]
                                 (make-product e
                                               (make-product (make-exponentiation b
                                                                                  (make-sum e -1))
                                                             (deriv b var)))))
          ]

    (put :deriv :** exponent)

    :done))

; Ex 2.73 d)
; We'd be required to swap the operations in all the (put ...) expressions.
; For instance for the exponentiation we'd have to change
; (put :deriv :** exponent) to (put :** :deriv exponent).
