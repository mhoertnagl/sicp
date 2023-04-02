(ns ch3_3.circuit
  (:require [clojure.test :refer :all])
  (:require [utils.lists :refer :all]))

(def NOT-GATE-DELAY 3)
(def AND-GATE-DELAY 5)
(def OR-GATE-DELAY 5)

(defprotocol IWire
  (get-signal [wire])
  (set-signal! [wire new-value])
  (add-action! [wire action]))

(defrecord Wire [value actions]
  IWire
  (get-signal [this] (:value @this))
  (set-signal! [this new-value] (swap! this assoc :value new-value)))

(defn make-wire []
  (atom (map->Wire { :value 0 :actions [] })))

(defn not-gate [input output]
  (letfn [(logical-not [s] (if (= s 0) 1 0))
          (action []
            (let [new-value (logical-not (get-signal input))]
              (after-delay NOT-GATE-DELAY
                           (fn [] (set-signal! output new-value)))))]
    (add-action! input action)
    :ok))

(defn and-gate [a1 a2 output]
  (letfn [(logical-and [s1 s2] (if (and (= s1 1) (= s2 1)) 1 0))
          (action []
            (let [new-value (logical-and (get-signal a1)
                                         (get-signal a2))]
              (after-delay AND-GATE-DELAY
                           (fn [] (set-signal! output new-value)))))]
    (add-action! a1 action)
    (add-action! a2 action)
    :ok))

; Ex 3.28
(defn or-gate [a1 a2 output]
  (letfn [(logical-or [s1 s2] (if (or (= s1 1) (= s2 1)) 1 0))
          (action []
            (let [new-value (logical-or (get-signal a1)
                                        (get-signal a2))]
              (after-delay OR-GATE-DELAY
                           (fn [] (set-signal! output new-value)))))]
    (add-action! a1 action)
    (add-action! a2 action)
    :ok))

; Ex 3.29
; Use the identity: A ∨ B ≡ ¬(¬A ∧ ¬B)

; Ex 3.30


(defn half-adder [a b s c]
  (let [(d (make-wire))
        (e (make-wire))]
    (or-gate a b d)
    (and-gate a b c)
    (not-gate c e)
    (and-gate d e s)))

(defn full-adder [a b c-in sum c-out]
  (let [(s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire))]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))