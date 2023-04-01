(ns ch2_4.complex
  (:require [clojure.test :refer :all])
  (:require [utils.lists :refer :all]))

(defn square [x] (* x x))

; TAGS

(defn attach-tag [type-tag contents]
  (list type-tag contents))

(defn type-tag [datum]
  (if (pair? datum)
    (first datum)
    (println "Bad tagged datum -- TYPE-TAG" datum)))

(defn contents [datum]
  (if (pair? datum)
    (second datum)
    (println "Bad tagged datum -- CONTENTS" datum)))

; RECTANGULAR

(defn rectangular? [z]
  (= (type-tag z) :rectangular))

(defn make-from-real-imag-rectangular [x y]
  (attach-tag :rectangular (list x y)))

(defn make-from-mag-ang-rectangular [r a]
  (attach-tag :rectangular (list (* r (Math/cos a))
                                 (* r (Math/sin a)))))

(defn real-part-rectangular [z] (first z))
(defn imag-part-rectangular [z] (second z))

(defn magnitude-rectangular [z]
  (Math/sqrt (+ (square (real-part-rectangular z))
                (square (imag-part-rectangular z)))))

(defn angle-rectangular [z]
  (Math/atan2 (imag-part-rectangular z)
              (real-part-rectangular z)))

; POLAR

(defn polar? [z]
  (= (type-tag z) :polar))

(defn make-from-real-imag-polar [x y]
  (attach-tag :polar (list (Math/sqrt (+ (square x) (square y)))
                           (Math/atan2 y x))))

(defn make-from-mag-ang-polar [r a]
  (attach-tag :polar ((list r a))))

(defn magnitude-polar [z] (first z))
(defn angle-polar [z] (second z))

(defn real-part-polar [z]
  (* (magnitude-polar z) (Math/cos (angle-polar z))))

(defn imag-part-polar [z]
  (* (magnitude-polar z) (Math/sin (angle-polar z))))

; GENERIC SELECTORS

(defn make-from-real-imag [x y]
  (make-from-real-imag-rectangular x y))

(defn make-from-mag-ang [r a]
  (make-from-mag-ang-polar r a))

(defn real-part [z]
  (cond (rectangular? z) (real-part-rectangular (contents z))
        (polar? z) (real-part-polar (contents z))
        :else (println "Unknown type -- REAL-PART" z)))

(defn imag-part [z]
  (cond (rectangular? z) (imag-part-rectangular (contents z))
        (polar? z) (imag-part-polar (contents z))
        :else (println "Unknown type -- IMAG-PART" z)))

(defn magnitude [z]
  (cond (rectangular? z) (magnitude-rectangular (contents z))
        (polar? z) (magnitude-polar (contents z))
        :else (println "Unknown type -- MAGNITUDE" z)))

(defn angle [z]
  (cond (rectangular? z) (angle-rectangular (contents z))
        (polar? z) (angle-polar (contents z))
        :else (println "Unknown type -- ANGLE" z)))

; ARITHMETIC

(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
