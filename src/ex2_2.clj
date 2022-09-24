(ns ex2-2)

(defn avg [a b] (/ (+ a b) 2))

(defn make-point [x y] [x y])
(defn x-point [p] (first p))
(defn y-point [p] (second p))
(defn point-str [p] (str "(" (x-point p) "," (y-point p) ")"))
(defn print-point [p] (println (point-str p)))

(defn make-segment [p q] [p q])
(defn start-segment [s] (first s))
(defn end-segment [s] (second s))

(defn midpoint-segment [s]
  (let [p (start-segment s)
        q (end-segment s)]
    (make-point (avg (x-point p) (x-point q))
                (avg (y-point p) (y-point q)))))