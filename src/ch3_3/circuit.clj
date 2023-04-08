(ns ch3_3.circuit
  (:require [clojure.test :refer :all]))

(def NOT-GATE-DELAY 2)
(def AND-GATE-DELAY 3)
(def OR-GATE-DELAY 5)

(defprotocol IAgenda
  (agenda-empty? [agenda])
  (agenda-size [agenda])
  (first-agenda-item [agenda])
  (remove-first-agenda-item! [agenda])
  (add-to-agenda! [agenda delay action])
  (set-time! [agenda time])
  (current-time [agenda]))

(defn agenda-item-time [item] (ffirst item))
(defn agenda-item-action [item] (second item))

(defrecord Agenda [time queue counter]
  IAgenda
  (agenda-empty? [_] (empty? @queue))
  (agenda-size [_] (count @queue))
  (first-agenda-item [_] (first @queue))
  (remove-first-agenda-item! [_]
    (swap! queue (fn [xs] (into (sorted-map) (rest xs)))))
  (add-to-agenda! [_ delay action]
    (swap! queue assoc [(+ @time delay) @counter] action)
    (swap! counter inc))
  (set-time! [_ new-time] (reset! time new-time))
  (current-time [_] @time))

(defn make-agenda []
  (->Agenda (atom 0) (atom (sorted-map)) (atom 0)))

(defn propagate [agenda]
  (while (not (agenda-empty? agenda))
    (let [item (first-agenda-item agenda)
          time (agenda-item-time item)
          action (agenda-item-action item)]
      (set-time! agenda time)
      (action)
      (remove-first-agenda-item! agenda))))

(defprotocol IWire
  (get-signal [wire])
  (set-signal! [wire new-value])
  (num-of-actions [wire])
  (add-action! [wire action]))

(defrecord Wire [agenda value actions]
  IWire
  (get-signal [_] @value)
  (set-signal! [_ new-value]
    (reset! value new-value)
    (doseq [action @actions] (action agenda)))
  (num-of-actions [_] (count @actions))
  (add-action! [_ action]
    (swap! actions conj action)
    (action agenda)))

(defn make-wire [agenda]
  (->Wire agenda (atom false) (atom ())))

(defn probe [name wire]
  (add-action! wire (fn [agenda]
                      (println (current-time agenda)
                               "-"
                               name
                               ":"
                               (get-signal wire)))))

(defn not-gate [input output]
  (letfn [(action [agenda]
            (add-to-agenda! agenda
                            NOT-GATE-DELAY
                            (fn [] (set-signal! output
                                                (not (get-signal input))))))]
    (add-action! input action)
    :ok))

(defn and-gate [a1 a2 output]
  (letfn [(action [agenda]
            (add-to-agenda! agenda
                            AND-GATE-DELAY
                            (fn [] (set-signal! output
                                                (and (get-signal a1) (get-signal a2))))))]
    (add-action! a1 action)
    (add-action! a2 action)
    :ok))

; Ex 3.28
(defn or-gate [a1 a2 output]
  (letfn [(action [agenda]
            (add-to-agenda! agenda
                            OR-GATE-DELAY
                            (fn [] (set-signal! output
                                                (or (get-signal a1) (get-signal a2))))))]
    (add-action! a1 action)
    (add-action! a2 action)
    :ok))

; Ex 3.29
; Use the identity: A ∨ B ≡ ¬(¬A ∧ ¬B)

(defn half-adder [agenda a b s c]
  (let [d (make-wire agenda)
        e (make-wire agenda)]
    (and-gate a b c)
    (not-gate c e)
    (or-gate a b d)
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

; Ex 3.30
(defn make-wires [agenda n]
  (for [_ (range n)] (make-wire agenda)))

(defn get-signals [wires]
  (map get-signal wires))

(defn set-signals! [wires new-values]
  (doseq [pair (map vector wires new-values)]
    (apply set-signal! pair)))

(defn ripple-carry-adder [agenda as bs c-in sum c-out]
  (let [n (dec (count as))
        cs (make-wires agenda n)
        cs-in (cons c-in cs)
        cs-out (conj cs c-out)
        units (map vector as bs cs-in sum cs-out)]
    (println "as" (count as))
    (println "bs" (count bs))
    (println "cs" (count cs))
    (println "cs-in" (count cs-in))
    (println "cs-out" (count cs-out))
    (doseq [unit units]
      ;(println "len unit" (count unit))
      (println (get-signals unit))
      ;(println "a" (get-signal (first unit))  "|"
      ;         "b" (get-signal (second unit)) "|"
      ;         "c-in" (get-signal (nth unit 2)) "|"
      ;         "s" (get-signal (nth unit 3)) "|"
      ;         "c-out" (get-signal (nth unit 4)))
      (apply full-adder agenda unit))
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

(deftest test-wires
  (testing "set signal without actions"
    (let [agenda (make-agenda)
          wire (make-wire agenda)]

      (set-signal! wire true)
      (is (= (get-signal wire) true))

      (set-signal! wire false)
      (is (= (get-signal wire) false))
      ))

  (testing "set signal with actions"
    (let [agenda (make-agenda)
          wire (make-wire agenda)]
      (add-action! wire (fn [agenda] false))

      (set-signal! wire true)
      (is (= (get-signal wire) true))

      (set-signal! wire false)
      (is (= (get-signal wire) false))
      ))
  )

(deftest test-agenda
  (testing "add to agenda"
    (let [agenda (make-agenda)]
      (is (= (agenda-size agenda) 0))

      (add-to-agenda! agenda 1 (fn [] 0))
      (is (= (agenda-size agenda) 1))
      (is (= (agenda-item-time (first-agenda-item agenda)) 1))
      (is (= ((agenda-item-action (first-agenda-item agenda))) 0))

      (add-to-agenda! agenda 2 (fn [] 1))
      (is (= (agenda-size agenda) 2))
      (is (= (agenda-item-time (first-agenda-item agenda)) 1))
      (is (= ((agenda-item-action (first-agenda-item agenda))) 0))

      (add-to-agenda! agenda 3 (fn [] 2))
      (is (= (agenda-size agenda) 3))
      (is (= (agenda-item-time (first-agenda-item agenda)) 1))
      (is (= ((agenda-item-action (first-agenda-item agenda))) 0))
      ))

  (testing "add same time to agenda"
    (let [agenda (make-agenda)]
      (is (= (agenda-size agenda) 0))

      (add-to-agenda! agenda 1 (fn [] 0))
      (is (= (agenda-size agenda) 1))

      (add-to-agenda! agenda 1 (fn [] 1))
      (is (= (agenda-size agenda) 2))

      (add-to-agenda! agenda 1 (fn [] 2))
      (is (= (agenda-size agenda) 3))
      ))

  (testing "add to agenda reverse"
    (let [agenda (make-agenda)]
      (is (= (agenda-size agenda) 0))

      (add-to-agenda! agenda 3 (fn [] 2))
      (is (= (agenda-size agenda) 1))
      (is (= (agenda-item-time (first-agenda-item agenda)) 3))
      (is (= ((agenda-item-action (first-agenda-item agenda))) 2))

      (add-to-agenda! agenda 2 (fn [] 1))
      (is (= (agenda-size agenda) 2))
      (is (= (agenda-item-time (first-agenda-item agenda)) 2))
      (is (= ((agenda-item-action (first-agenda-item agenda))) 1))

      (add-to-agenda! agenda 1 (fn [] 0))
      (is (= (agenda-size agenda) 3))
      (is (= (agenda-item-time (first-agenda-item agenda)) 1))
      (is (= ((agenda-item-action (first-agenda-item agenda))) 0))
      ))

  ;(testing "remove from agenda"
  ;  (let [agenda (make-agenda)]
  ;    (add-to-agenda! agenda 1 (fn [] 0))
  ;    (add-to-agenda! agenda 2 (fn [] 1))
  ;    (add-to-agenda! agenda 3 (fn [] 2))
  ;    (is (= (agenda-size agenda) 3))
  ;
  ;    ))
  )

(deftest test-not-gate
  (testing "test not-gate"
    (let [agenda (make-agenda)
          input (make-wire agenda)
          output (make-wire agenda)]
      (not-gate input output)

      (set-signal! input false)
      (propagate agenda)
      (is (= (get-signal output) true))

      (set-signal! input true)
      (propagate agenda)
      (is (= (get-signal output) false))
      ))
  )

(deftest test-and-gate
  (testing "test and-gate 0 0"
    (let [agenda (make-agenda)
          input1 (make-wire agenda)
          input2 (make-wire agenda)
          output (make-wire agenda)]
      (and-gate input1 input2 output)

      (set-signal! input1 false)
      (set-signal! input2 false)
      (propagate agenda)
      (is (= (get-signal output) false))
      ))

  (testing "test and-gate 0 1"
    (let [agenda (make-agenda)
          input1 (make-wire agenda)
          input2 (make-wire agenda)
          output (make-wire agenda)]
      (and-gate input1 input2 output)

      (set-signal! input1 false)
      (set-signal! input2 true)
      (propagate agenda)
      (is (= (get-signal output) false))
      ))

  (testing "test and-gate 1 0"
    (let [agenda (make-agenda)
          input1 (make-wire agenda)
          input2 (make-wire agenda)
          output (make-wire agenda)]
      (and-gate input1 input2 output)

      (set-signal! input1 true)
      (set-signal! input2 false)
      (propagate agenda)
      (is (= (get-signal output) false))

      ))

  (testing "test and-gate 1 1"
    (let [agenda (make-agenda)
          input1 (make-wire agenda)
          input2 (make-wire agenda)
          output (make-wire agenda)]
      (and-gate input1 input2 output)

      (set-signal! input1 true)
      (set-signal! input2 true)
      (propagate agenda)
      (is (= (get-signal output) true))
      ))
  )

(deftest test-or-gate
  (testing "test or-gate"
    (let [agenda (make-agenda)
          input1 (make-wire agenda)
          input2 (make-wire agenda)
          output (make-wire agenda)]
      (or-gate input1 input2 output)

      (set-signal! input1 false)
      (set-signal! input2 false)
      (propagate agenda)
      (is (= (get-signal output) false))

      (set-signal! input1 false)
      (set-signal! input2 true)
      (propagate agenda)
      (is (= (get-signal output) true))

      (set-signal! input1 true)
      (set-signal! input2 false)
      (propagate agenda)
      (is (= (get-signal output) true))

      (set-signal! input1 true)
      (set-signal! input2 true)
      (propagate agenda)
      (is (= (get-signal output) true))
      ))
  )

(deftest test-composite-not-gate
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
  )

(deftest test-composite-and-gate
  (testing "test composite and 0 0 0"
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
      ))

  (testing "test composite and 1 1 1"
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

      (set-signal! a true)
      (set-signal! b true)
      (set-signal! c true)
      (propagate agenda)
      (is (= (get-signal s1) true))
      (is (= (get-signal s2) true))
      ))
  )

(deftest test-composite-or-gate
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
  )

(deftest test-half-adder
  (testing "test half-adder"
    (let [agenda (make-agenda)
          a (make-wire agenda)
          b (make-wire agenda)
          s (make-wire agenda)
          c (make-wire agenda)]

      ;(probe "a" a)
      ;(probe "b" b)
      ;(probe "s" s)
      ;(probe "c" c)

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
      (is (= (get-signal s) false))
      (is (= (get-signal c) true))
      ))
  )

(deftest test-full-adder
  (testing "test full-adder"
    (let [agenda (make-agenda)
          a (make-wire agenda)
          b (make-wire agenda)
          c-in (make-wire agenda)
          s (make-wire agenda)
          c-out (make-wire agenda)]

      ;(probe "a" a)
      ;(probe "b" b)
      ;(probe "s" s)
      ;(probe "c" c)

      (full-adder agenda a b c-in s c-out)

      (set-signal! a false)
      (set-signal! b false)
      (set-signal! c-in false)
      (propagate agenda)
      (is (= (get-signal s) false))
      (is (= (get-signal c-out) false))

      (set-signal! a false)
      (set-signal! b false)
      (set-signal! c-in true)
      (propagate agenda)
      (is (= (get-signal s) true))
      (is (= (get-signal c-out) false))

      (set-signal! a true)
      (set-signal! b true)
      (set-signal! c-in false)
      (propagate agenda)
      (is (= (get-signal s) false))
      (is (= (get-signal c-out) true))

      (set-signal! a true)
      (set-signal! b true)
      (set-signal! c-in true)
      (propagate agenda)
      (is (= (get-signal s) true))
      (is (= (get-signal c-out) true))
      ))
  )

(deftest test-ripple-carry-adder
  (testing "vector operations"
    (let [agenda (make-agenda)
          as (make-wires agenda 4)]

      (set-signals! as [true true true true])
      (is (= (get-signals as) [true true true true]))
      ))

  ;(testing "ripple carry adder 1"
  ;  (let [agenda (make-agenda)
  ;        as (make-wires agenda 1)
  ;        bs (make-wires agenda 1)
  ;        c-in (make-wire agenda)
  ;        sum (make-wires agenda 1)
  ;        c-out (make-wire agenda)]
  ;    (ripple-carry-adder agenda as bs c-in sum c-out)
  ;
  ;    (set-signal! c-in false)
  ;    (set-signals! as [false])
  ;    (set-signals! bs [false])
  ;    (propagate agenda)
  ;    (is (= (get-signals sum) [false]))
  ;    (is (= (get-signal c-out) false))
  ;
  ;    (set-signal! c-in false)
  ;    (set-signals! as [true])
  ;    (set-signals! bs [false])
  ;    (propagate agenda)
  ;    (is (= (get-signals sum) [true]))
  ;    (is (= (get-signal c-out) false))
  ;
  ;    (set-signal! c-in false)
  ;    (set-signals! as [true])
  ;    (set-signals! bs [true])
  ;    (propagate agenda)
  ;    (is (= (get-signals sum) [false]))
  ;    (is (= (get-signal c-out) true))
  ;
  ;    (set-signal! c-in true)
  ;    (set-signals! as [true])
  ;    (set-signals! bs [true])
  ;    (propagate agenda)
  ;    (is (= (get-signals sum) [true]))
  ;    (is (= (get-signal c-out) true))
  ;    ))

  (testing "ripple carry adder 2"
    (let [agenda (make-agenda)
          as (make-wires agenda 2)
          bs (make-wires agenda 2)
          c-in (make-wire agenda)
          sum (make-wires agenda 2)
          c-out (make-wire agenda)]
      (ripple-carry-adder agenda as bs c-in sum c-out)

      (set-signal! c-in false)
      (set-signals! as [false false])
      (set-signals! bs [false false])
      (propagate agenda)
      (is (= (get-signals sum) [false false]))
      (is (= (get-signal c-out) false))

      ;(set-signal! c-in false)
      ;(set-signals! as [true false])
      ;(set-signals! bs [false])
      ;(propagate agenda)
      ;(is (= (get-signals sum) [true false]))
      ;(is (= (get-signal c-out) false))
      ;
      ;(set-signal! c-in false)
      ;(set-signals! as [false true])
      ;(set-signals! bs [false true])
      ;(propagate agenda)
      ;(is (= (get-signals sum) [true false]))
      ;(is (= (get-signal c-out) false))
      ;
      ;(set-signal! c-in true)
      ;(set-signals! as [true true])
      ;(set-signals! bs [true true])
      ;(propagate agenda)
      ;(is (= (get-signals sum) [true true]))
      ;(is (= (get-signal c-out) true))
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
  ;    (set-signal! c-in false)
  ;    (set-signals! as [false false false false])
  ;    (set-signals! bs [false false false false])
  ;    (propagate agenda)
  ;    (is (= (get-signals sum) [false false false false]))
  ;    (is (= (get-signal c-out) false))
  ;
  ;    ;(set-signal! c-in false)
  ;    ;(set-signals! as [false false false true])
  ;    ;(set-signals! bs [false false false true])
  ;    ;(propagate agenda)
  ;    ;(is (= (get-signals sum) [false false true false]))
  ;    ;(is (= (get-signal c-out) false))
  ;    ))
  )