(ns ee2-1-4_m2)
;; Alternative implementation that addresses issues raised in
;; exercises 2.14, 2.15 and 2.16.
;; Every interval contains a unique identifier. There are two
;; cases to consider:
;;
;;  1) The interval with this identifier occurs for the first
;;     time in the expression. Then continue with bounded
;;     calculations.
;;  2) The interval with this identifier has already occurred
;;     in the expression. Discard the bounds, and continue
;;     with bounds of width zero.

(defn avg [& xs] (/ (reduce + xs) (count xs)))

(defn make-interval [n a b] [n a b])

(defn make-center-width [n c w]
  (make-interval n (- c w) (+ c w)))

(defn make-center-percent [n c p]
  (let [w (* c (/ p 100))]
    (make-center-width n c w)))

(defn interval-id [x] (first x))
(defn lower-bound [x] (nth x 1))
(defn upper-bound [x] (nth x 2))

(defn center [i]
  (avg (lower-bound i) (upper-bound i)))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn percent [i]
  (* 100 (/ (width i) (center i))))


(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

(defn inv-interval [x]
  (make-interval (/ (upper-bound x))
                 (/ (lower-bound x))))

(defn div-interval [x y]
  (mul-interval x (inv-interval y)))

(defn interval-str [x]
  (str "[" (lower-bound x) ", " (upper-bound x) "]"))

(defn print-interval [x] (println (interval-str x)))

(defn interval-percent-str [x]
  (str (center x) " ± " (percent x) "%"))

(defn print-interval-percent [x]
  (println (interval-percent-str x)))

;; user=> (conj #{1 2 3} 3 4)
;; #{1 4 3 2}
;; (conj [1 2 3] 4) yields [1 2 3 4]