(ns ex2-17)

(defn last-pair [xs]
  (case (count xs)
    0 (throw (IllegalArgumentException. "Vector is empty"))
    1 xs
    (last-pair (rest xs))))