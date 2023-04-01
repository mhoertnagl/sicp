(ns ch2_4.complex_ddp
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

(defn install-rectangular-package []
  (letfn [(real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [x y]
            (attach-tag :rectangular (list x y)))
          (make-from-mag-ang [r a]
            (attach-tag :rectangular (list (* r (Math/cos a))
                                           (* r (Math/sin a)))))
          (magnitude [z]
            (Math/sqrt (+ (square (real-part z))
                          (square (imag-part z)))))
          (angle [z]
            (Math/atan2 (imag-part z)
                        (real-part z)))]

    (put :real-part '(:rectangular) real-part)
    (put :imag-part '(:rectangular) imag-part)
    (put :magnitude '(:rectangular) magnitude)
    (put :angle '(:rectangular) angle)
    (put :make-from-real-imag :rectangular make-from-real-imag)
    (put :make-from-mag-ang :rectangular make-from-mag-ang)

    :done))

(defn install-polar-package []
  (letfn [(magnitude [z] (first z))
          (angle [z] (second z))
          (real-part [z]
            (* (magnitude z) (Math/cos (angle z))))
          (imag-part [z]
            (* (magnitude z) (Math/sin (angle z))))
          (make-from-real-imag [x y]
            (attach-tag :polar (list (Math/sqrt (+ (square x) (square y)))
                                     (Math/atan2 y x))))
          (make-from-mag-ang [r a]
            (attach-tag :polar ((list r a))))]

    (put :real-part '(:polar) real-part)
    (put :imag-part '(:polar) imag-part)
    (put :magnitude '(:polar) magnitude)
    (put :angle '(:polar) angle)
    (put :make-from-real-imag :polar make-from-real-imag)
    (put :make-from-mag-ang :polar make-from-mag-ang)

    :done))

; GENERIC SELECTORS

(defn make-from-real-imag [x y]
  (get :make-from-real-imag :rectangular x y))

(defn make-from-mag-ang [r a]
  (get :make-from-mag-ang :polar r a))

(defn real-part [z] (apply-generic :real-part z))
(defn imag-part [z] (apply-generic :imag-part z))
(defn magnitude [z] (apply-generic :magnitude z))
(defn angle [z] (apply-generic :angle z))

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
