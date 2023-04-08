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
    (reset! time (+ @time delay))
    (swap! queue assoc @time action))
  (current-time [_] @time))

(defn make-agenda []
  (->Agenda (atom 0) (atom (sorted-map))))

(defn propagate [agenda]
  (while (not (agenda-empty? agenda))
    (let [first-item (first-agenda-item agenda)]
      ;(println agenda)
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
  (->Wire agenda (atom false) (atom [])))

(defn probe [name wire]
  (add-action! wire (fn [agenda] (println (current-time agenda)
                                          "-"
                                          name
                                          ":"
                                          (get-signal wire)))))

;(defn propagate-signal [agenda delay new-value output]
;  (if (not= (get-signal output) new-value)
;    (add-to-agenda! agenda
;                    delay
;                    (fn [] (set-signal! output new-value)))))

(defn propagate-signal [agenda delay new-value output]
  (add-to-agenda! agenda
                  delay
                  (fn [] (set-signal! output new-value))))

(defn not-gate [input output]
  (letfn [(action [agenda]
            (propagate-signal agenda
                              NOT-GATE-DELAY
                              (not (get-signal input))
                              output))]
    (add-action! input action)
    :ok))

(defn and-gate [a1 a2 output]
  (letfn [(action [agenda]
            (propagate-signal agenda
                              AND-GATE-DELAY
                              (and (get-signal a1) (get-signal a2))
                              output))]
    (add-action! a1 action)
    (add-action! a2 action)
    :ok))

; Ex 3.28
(defn or-gate [a1 a2 output]
  (letfn [(action [agenda]
            (propagate-signal agenda
                              OR-GATE-DELAY
                              (or (get-signal a1) (get-signal a2))
                              output))]
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
    (and-gate d e s)
    :ok))

(defn full-adder [agenda a b c-in sum c-out]
  (let [s (make-wire agenda)
        c1 (make-wire agenda)
        c2 (make-wire agenda)]
    (half-adder agenda b c-in s c1)
    (half-adder agenda a s sum c2)
    (or-gate c1 c2 c-out)
    :ok))

; Ex 3.3false
(defn make-wires [agenda n]
  (for [_ (range n)] (make-wire agenda)))

(defn get-signals [wires]
  (map get-signal wires))

(defn set-signals! [wires new-values]
  (map set-signal! wires new-values))

(defn ripple-carry-adder [agenda as bs c-in sum c-out]
  (let [cs (make-wires agenda (count as))
        cs-in (cons c-in cs)
        cs-out (conj cs c-out)
        units (map vector as bs cs-in sum cs-out)]
    (doseq [unit units] (apply full-adder agenda unit))
    :ok))

; Ex 3.31
; We need to propagate the initial state to the output wires.
; If we skip this initial call of the action, the circuit is
; in an inconsistent state.

; Ex 3.32
; The correct way is to use a priority queue to account for
; the fact that the gates have differing delays.
; Without this ordering the simulation would not be accurate
; in its time evolution.

(deftest tests
  ;(testing "set signal without actions"
  ;  (let [agenda (make-agenda)
  ;        wire (make-wire agenda)]
  ;
  ;    (set-signal! wire true)
  ;    (is (= (get-signal wire) true))
  ;
  ;    (set-signal! wire false)
  ;    (is (= (get-signal wire) false))
  ;    ))
  ;
  ;(testing "set signal with actions"
  ;  (let [agenda (make-agenda)
  ;        wire (make-wire agenda)]
  ;    (add-action! wire (fn [agenda] false))
  ;
  ;    (set-signal! wire true)
  ;    (is (= (get-signal wire) true))
  ;
  ;    (set-signal! wire false)
  ;    (is (= (get-signal wire) false))
  ;    ))
  ;
  ;(testing "test not-gate"
  ;  (let [agenda (make-agenda)
  ;        input (make-wire agenda)
  ;        output (make-wire agenda)]
  ;    (not-gate input output)
  ;
  ;    (set-signal! input false)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) true))
  ;
  ;    (set-signal! input true)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) false))
  ;    ))
  ;
  ;(testing "test and-gate"
  ;  (let [agenda (make-agenda)
  ;        input1 (make-wire agenda)
  ;        input2 (make-wire agenda)
  ;        output (make-wire agenda)]
  ;    (and-gate input1 input2 output)
  ;
  ;    (set-signal! input1 false)
  ;    (set-signal! input2 false)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) false))
  ;
  ;    (set-signal! input1 false)
  ;    (set-signal! input2 true)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) false))
  ;
  ;    (set-signal! input1 true)
  ;    (set-signal! input2 false)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) false))
  ;
  ;    (set-signal! input1 true)
  ;    (set-signal! input2 true)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) true))
  ;    ))
  ;
  ;(testing "test or-gate"
  ;  (let [agenda (make-agenda)
  ;        input1 (make-wire agenda)
  ;        input2 (make-wire agenda)
  ;        output (make-wire agenda)]
  ;    (or-gate input1 input2 output)
  ;
  ;    (set-signal! input1 false)
  ;    (set-signal! input2 false)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) false))
  ;
  ;    (set-signal! input1 false)
  ;    (set-signal! input2 true)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) true))
  ;
  ;    (set-signal! input1 true)
  ;    (set-signal! input2 false)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) true))
  ;
  ;    (set-signal! input1 true)
  ;    (set-signal! input2 true)
  ;    (propagate agenda)
  ;    (is (= (get-signal output) true))
  ;    ))

  (testing "test composite not"
    (let [agenda (make-agenda)
          a (make-wire agenda)
          b (make-wire agenda)
          c (make-wire agenda)
          d (make-wire agenda)]
      (not-gate a b)
      (not-gate b c)
      (not-gate c d)

      ;(probe "a" a)
      ;(probe "b" b)
      ;(probe "c" c)
      ;(probe "d" d)

      (set-signal! a false)
      (propagate agenda)
      (is (= (get-signal b) true))
      (is (= (get-signal c) false))
      (is (= (get-signal d) true))

      (set-signal! a true)
      (propagate agenda)
      (is (= (get-signal b) false))
      (is (= (get-signal c) true))
      (is (= (get-signal d) false))
      ))

  (testing "test composite and"
    (let [agenda (make-agenda)
          a (make-wire agenda)
          b (make-wire agenda)
          c (make-wire agenda)
          s1 (make-wire agenda)
          s2 (make-wire agenda)]
      (and-gate a b s1)
      (and-gate s1 c s2)

      ;(probe "a" a)
      ;(probe "b" b)
      ;(probe "c" c)
      ;(probe "s1" s1)
      ;(probe "s2" s2)

      (set-signal! a false)
      (set-signal! b false)
      (set-signal! c false)
      (propagate agenda)
      (is (= (get-signal s1) false))
      (is (= (get-signal s2) false))

      (set-signal! a true)
      (set-signal! b true)
      (set-signal! c true)
      (propagate agenda)
      (is (= (get-signal s1) true))
      (is (= (get-signal s2) true))
      ))

  (testing "test composite or"
    (let [agenda (make-agenda)
          a (make-wire agenda)
          b (make-wire agenda)
          c (make-wire agenda)
          s1 (make-wire agenda)
          s2 (make-wire agenda)]
      (or-gate a b s1)
      (or-gate s1 c s2)

      ;(probe "a" a)
      ;(probe "b" b)
      ;(probe "c" c)
      ;(probe "s1" s1)
      ;(probe "s2" s2)

      (set-signal! a false)
      (set-signal! b false)
      (set-signal! c false)
      (propagate agenda)
      (is (= (get-signal s1) false))
      (is (= (get-signal s2) false))

      (set-signal! a true)
      (set-signal! b false)
      (set-signal! c false)
      (propagate agenda)
      (is (= (get-signal s1) true))
      (is (= (get-signal s2) true))

      (set-signal! a false)
      (set-signal! b true)
      (set-signal! c false)
      (propagate agenda)
      (is (= (get-signal s1) true))
      (is (= (get-signal s2) true))

      (set-signal! a false)
      (set-signal! b false)
      (set-signal! c true)
      (propagate agenda)
      (is (= (get-signal s1) false))
      (is (= (get-signal s2) true))
      ))

  (testing "test half-adder"
    (let [agenda (make-agenda)
          a (make-wire agenda)
          b (make-wire agenda)
          s (make-wire agenda)
          c (make-wire agenda)]
      (half-adder agenda a b s c)

      (set-signal! a false)
      (set-signal! b false)
      (propagate agenda)
      (is (= (get-signal s) false))
      (is (= (get-signal c) false))

      (set-signal! a false)
      (set-signal! b true)
      (propagate agenda)
      (is (= (get-signal s) true))
      (is (= (get-signal c) false))

      (set-signal! a true)
      (set-signal! b false)
      (propagate agenda)
      (is (= (get-signal s) true))
      (is (= (get-signal c) false))

      (set-signal! a true)
      (set-signal! b true)
      (propagate agenda)
      (is (= (get-signal s) true))
      (is (= (get-signal c) true))
      ))

  ;(testing "ripple carry adder"
  ;  (let [agenda (make-agenda)
  ;        as (make-wires agenda 4)
  ;        bs (make-wires agenda 4)
  ;        c-in (make-wire agenda)
  ;        sum (make-wires agenda 4)
  ;        c-out (make-wire agenda)]
  ;    (ripple-carry-adder agenda as bs c-in sum c-out)
  ;
  ;    ;(set-signal! c-in false)
  ;    ;(set-signals! as [false false false false])
  ;    ;(set-signals! bs [false false false false])
  ;    ;(propagate agenda)
  ;    ;(is (= (get-signals sum) [false false false false]))
  ;    ;(is (= (get-signal c-out) false))
  ;
  ;    ;(set-signal! c-in false)
  ;    ;(set-signals! as [false false false true])
  ;    ;(set-signals! bs [false false false true])
  ;    ;(propagate agenda)
  ;    ;(is (= (get-signals sum) [false false true false]))
  ;    ;(is (= (get-signal c-out) false))
  ;    ))
  )