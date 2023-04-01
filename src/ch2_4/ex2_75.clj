(ns ch2_4.ex2_75
  (:require [clojure.test :refer :all])
  (:require [utils.lists :refer :all]))

(defn make-from-mag-ang [r a]
  (fn [op]
    (cond
      (= op :real-part) (* r (Math/cos a))
      (= op :imag-part) (* r (Math/sin a))
      (= op :magnitude) r
      (= op :angle)     a
      :else             (println "unknown operation type -- MAKE-FROM-MAG-ANG" op))))
