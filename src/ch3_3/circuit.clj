(ns ch3_3.circuit
  (:require [clojure.test :refer :all]))

(def NOT-GATE-DELAY 2)
(def AND-GATE-DELAY 3)
(def OR-GATE-DELAY 5)

(defprotocol IAgenda
  (agenda-empty? [agenda])
  (first-agenda-item [agenda])
  (remove-first-agenda-item! [agenda])
  (add-to-agenda! [agenda delay action])
  (current-time [agenda]))

(defrecord Agenda [time queue]
  IAgenda
  (agenda-empty? [_] (empty? @queue))
  (first-agenda-item [_] (second (first @queue)))
  (remove-first-agenda-item! [_]
    (swap! queue rest))
  (add-to-agenda! [_ delay action]
    (swap! queue assoc (+ @time delay) action))
  (current-time [_] @time))

(defn make-agenda [] (->Agenda (atom 0) (atom (sorted-map))))

(defn propagate [agenda]
  (while (not (agenda-empty? agenda))
    (let [first-item (first-agenda-item agenda)]
      (first-item)
      (remove-first-agenda-item! agenda))))

(def the-agenda (make-agenda))

; TODO: Unsatisfactory. Couples the circuit to a global agenda instance.
(defn after-delay [delay action]
  (add-to-agenda! the-agenda delay action))

(defprotocol IWire
  (get-signal [wire])
  (set-signal! [wire new-value])
  (add-action! [wire action]))

(defrecord Wire [value actions]
  IWire
  (get-signal [_] @value)
  (set-signal! [_ new-value]
    (reset! value new-value)
    (doseq [action @actions] (action)))
  (add-action! [_ action]
    (swap! actions conj action)
    (action)))

(defn make-wire [] (->Wire (atom 0) (atom [])))

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
  (let [d (make-wire)
        e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (not-gate c e)
    (and-gate d e s)))

(defn full-adder [a b c-in sum c-out]
  (let [s (make-wire)
        c1 (make-wire)
        c2 (make-wire)]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

(deftest tests
  (testing "set signal"
    (let [wire (make-wire)]
      (is (= (get-signal wire) 0))
      (set-signal! wire 1)
      (is (= (get-signal wire) 1)))
    )

  (testing "test not-gate"
    (let [input (make-wire)
          output (make-wire)]
      (not-gate input output)
      (is (= (first @my-map) [2 3])))
    )
  )