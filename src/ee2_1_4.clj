(ns ee2-1-4)

;; Exercise 2.7
(defn make-interval [a b] [a b])
(defn lower-bound [x] (first x))
(defn upper-bound [x] (second x))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; Exercise 2.10
;; Throwing exceptions at users is not nice ;)
(defn make-interval [a b]
  (if (= a b)
    (throw (IllegalArgumentException. "Interval spans zero"))
    [a b]))

;; Exercise 2.8
;; Suppose x = [1.1 1.7] and y = [0.3 0.5].
;; The lowest value of x - y = [1.1 1.7] - [0.3 0.5] would then be
;; the lower bound of x minus the upper bound of y. Likewise, the
;; highest attainable value would be the upper bound of x minus the
;; lower bound of y.
(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; Exercise 2.11
;; Sorry, this exercise sucks.

;; Note that lower and upper bounds swap places.
;(defn inv-interval [x]
;  (make-interval (/ 1.0 (upper-bound x))
;                 (/ 1.0 (lower-bound x))))
(defn inv-interval [x]
  (make-interval (/ (upper-bound x))
                 (/ (lower-bound x))))

(defn div-interval [x y]
  (mul-interval x (inv-interval y)))

(defn interval-str [x]
  (str "[" (lower-bound x) ", " (upper-bound x) "]"))

(defn print-interval [x] (println (interval-str x)))

;; Exercise 2.9
;; Define the width of an interval x = [x1, x2] to w(x) = (x2 - x1)/2
;; We will prove that w(x + y) = w(x) + w(y).
;;
;; Proof. Let x = [x1, x2] and y = [y1, y2] be two intervals. Then
;;
;;   w(x + y) = ((x2 + y2) - (x1 + y1))/2
;;            = ((x2 - x1) + (y2 - y1))/2
;;            = (x2 - x1)/2 + (y2 - y1)/2
;;            = w(x) + w(y)                                         □

(defn avg [& xs] (/ (reduce + xs) (count xs)))

(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [i]
  (avg (lower-bound i) (upper-bound i)))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.12
(defn make-center-percent [c p]
  (let [w (* c (/ p 100))]
    (make-center-width c w)))

(defn percent [i]
  (* 100 (/ (width i) (center i))))

;; Exercise 2.13
;; unit test "multiply intervals with small percentages"
;; indicates this simple law for the percentage p of two
;; products with percentage p₁ and p₂ respectively:
;;
;;                  p ≈ p₁ + p₂
;;
;; Proof. Let c₁ ± p₁ and c₂ ± p₂ be positive intervals.
;; We can rewrite every percentage interval to a range
;; interval such that
;;
;;        c ± p ≡ [c(1 - p/100), c(1 + p/100)]
;;
;; Since intervals are symmetric, we need only show the
;; result for the lower or upper bound. Let
;;
;;        c₁(1 + p₁/100) and c₂(1 + p₂/100)
;;
;; be the upper bounds of the intervals. Then the upper
;; bound of the product of these two intervals equals to
;;
;;        c₁(1 + p₁/100)c₂(1 + p₂/100)
;;           = c₁c₂(1 + p₁/100)(1 + p₂/100)
;;           = c₁c₂[1 + p₁/100 + p₂/100 + p₁p₂/10000]
;;
;; The term p₁p₂/10000 ≪ 1 for small p₁ and/or p₂.
;;
;;        c₁c₂[1 + p₁/100 + p₂/100 + p₁p₂/10000]
;;           ≈ c₁c₂[1 + p₁/100 + p₂/100]
;;           ≈ c₁c₂[1 + (p₁ + p₂)/100]
;;
;; The same is true for the lower bound.
;; This proves p ≈ p₁ + p₂.                            □

(defn interval-percent-str [x]
  (str (center x) " ± " (percent x) "%"))

(defn print-interval-percent [x]
  (println (interval-percent-str x)))

;; Exercise 2.14
;; unit test "parallel resistances results differ"
;; documents the different results for par1 and par2.

;; Exercise 2.15
;; As proved in E2.13, the percentage of the product of two
;; intervals equals approximately the sum of the two
;; percentages of the factors. The same is true for the
;; quotient.
;; In the case of resistors, a resistors' tolerance should
;; be accounted for only once, even if the algebraic
;; expression does not allow it to be reduced to a single
;; occurrence.

;; Exercise 2.15
;; In order to avoid repeated application of an interval's
;; bounds, every interval is assigned a unique identifier.
;; While evaluating an expression, record the first
;; application of an interval's bounds. If an interval has
;; been encountered already continue computation with the
;; center value and a bound width of 0.
