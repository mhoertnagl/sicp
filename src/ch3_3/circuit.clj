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
    (swap! queue (fn [elems] (into (sorted-map) (rest elems)))))
  (add-to-agenda! [_ delay action]
    (swap! queue assoc (+ @time delay) action))
  (current-time [_] @time))

(defn make-agenda []
  (->Agenda (atom 0) (atom (sorted-map))))

(defn propagate [agenda]
  (while (not (agenda-empty? agenda))
    (let [first-item (first-agenda-item agenda)]
      (first-item)
      (remove-first-agenda-item! agenda))))

(defprotocol IWire
  (get-signal [wire])
  (set-signal! [wire new-value])
  (add-action! [wire action]))

(defrecord Wire [agenda value actions]
  IWire
  (get-signal [_] @value)
  (set-signal! [_ new-value]
    (reset! value new-value)
    (doseq [action @actions] (action agenda)))
  (add-action! [_ action]
    (swap! actions conj action)
    (action agenda)))

(defn make-wire [agenda]
  (->Wire agenda (atom 0) (atom [])))

(defn not-gate [input output]
  (letfn [(logical-not [s] (if (= s 0) 1 0))
          (action [agenda]
            (let [new-value (logical-not (get-signal input))]
              (add-to-agenda! agenda
                              NOT-GATE-DELAY
                              (fn [] (set-signal! output new-value)))))]
    (add-action! input action)
    :ok))

(defn and-gate [a1 a2 output]
  (letfn [(logical-and [s1 s2] (if (and (= s1 1) (= s2 1)) 1 0))
          (action [agenda]
            (let [new-value (logical-and (get-signal a1)
                                         (get-signal a2))]
              (add-to-agenda! agenda
                              AND-GATE-DELAY
                              (fn [] (set-signal! output new-value)))))]
    (add-action! a1 action)
    (add-action! a2 action)
    :ok))

; Ex 3.28
(defn or-gate [a1 a2 output]
  (letfn [(logical-or [s1 s2] (if (or (= s1 1) (= s2 1)) 1 0))
          (action [agenda]
            (let [new-value (logical-or (get-signal a1)
                                        (get-signal a2))]
              (add-to-agenda! agenda
                              OR-GATE-DELAY
                              (fn [] (set-signal! output new-value)))))]
    (add-action! a1 action)
    (add-action! a2 action)
    :ok))

; Ex 3.29
; Use the identity: A ∨ B ≡ ¬(¬A ∧ ¬B)

(defn half-adder [agenda a b s c]
  (let [d (make-wire agenda)
        e (make-wire agenda)]
    (or-gate a b d)
    (and-gate a b c)
    (not-gate c e)
    (and-gate d e s)))

(defn full-adder [agenda a b c-in sum c-out]
  (let [s (make-wire agenda)
        c1 (make-wire agenda)
        c2 (make-wire agenda)]
    (half-adder agenda b c-in s c1)
    (half-adder agenda a s sum c2)
    (or-gate c1 c2 c-out)))

; Ex 3.30
(defn make-wires [agenda n]
  (for [_ (range n)] (make-wire agenda)))

(defn get-signals [wires]
  (map get-signal wires))

(defn set-signals! [wires new-values]
  (map set-signal! wires new-values))

; TODO: Implement this.
; (defn ripple-carry-adder [agenda as bs c-in sum c-out])

(deftest tests
  (testing "set signal without actions"
    (let [agenda (make-agenda)
          wire (make-wire agenda)]
      (is (= (get-signal wire) 0))
      (set-signal! wire 1)
      (is (= (get-signal wire) 1))
      (set-signal! wire 0)
      (is (= (get-signal wire) 0))
      ))

  (testing "set signal with actions"
    (let [agenda (make-agenda)
          wire (make-wire agenda)]
      (add-action! wire (fn [agenda] 0))
      (is (= (get-signal wire) 0))
      (set-signal! wire 1)
      (is (= (get-signal wire) 1))
      (set-signal! wire 0)
      (is (= (get-signal wire) 0))
      ))

  (testing "test not-gate"
    (let [agenda (make-agenda)
          input (make-wire agenda)
          output (make-wire agenda)]
      (not-gate input output)

      (set-signal! input 0)
      (propagate agenda)
      (is (= (get-signal output) 1))

      (set-signal! input 1)
      (propagate agenda)
      (is (= (get-signal output) 0))
      ))

  (testing "test and-gate"
    (let [agenda (make-agenda)
          input1 (make-wire agenda)
          input2 (make-wire agenda)
          output (make-wire agenda)]
      (and-gate input1 input2 output)

      (set-signal! input1 0)
      (set-signal! input2 0)
      (propagate agenda)
      (is (= (get-signal output) 0))

      (set-signal! input1 0)
      (set-signal! input2 1)
      (propagate agenda)
      (is (= (get-signal output) 0))

      (set-signal! input1 1)
      (set-signal! input2 0)
      (propagate agenda)
      (is (= (get-signal output) 0))

      (set-signal! input1 1)
      (set-signal! input2 1)
      (propagate agenda)
      (is (= (get-signal output) 1))
      ))

  (testing "test or-gate"
    (let [agenda (make-agenda)
          input1 (make-wire agenda)
          input2 (make-wire agenda)
          output (make-wire agenda)]
      (or-gate input1 input2 output)

      (set-signal! input1 0)
      (set-signal! input2 0)
      (propagate agenda)
      (is (= (get-signal output) 0))

      (set-signal! input1 0)
      (set-signal! input2 1)
      (propagate agenda)
      (is (= (get-signal output) 1))

      (set-signal! input1 1)
      (set-signal! input2 0)
      (propagate agenda)
      (is (= (get-signal output) 1))

      (set-signal! input1 1)
      (set-signal! input2 1)
      (propagate agenda)
      (is (= (get-signal output) 1))
      ))
  )