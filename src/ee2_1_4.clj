(ns ee2-1-4)

;; Exercise 2.7
(defn make-interval [a b] [a b])
(defn lower-bound [x] (first x))
(defn upper-bound [x] (second x))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

(defn inv-interval [x]
  (make-interval (/ 1.0 (upper-bound x))
                 (/ 1.0 (lower-bound x))))

(defn div-interval [x y] (mul-interval x (inv-interval y)))

(defn interval-str [x] (str "[" (lower-bound x) ", " (upper-bound x) "]"))
(defn print-interval [x] (println (interval-str x)))


