(ns ch2_4.ex2_76
  (:require [clojure.test :refer :all])
  (:require [utils.lists :refer :all]))

; # Explicit-dispatch style
; Adding a new representation requires an explicit dispatch in all existing
; operations. Adding a new operation requires no changes to existing
; representations or operations.

; # Data-directed style
; A new representation does not require changes to existing code. The users
; are only required to invoke the package installation routine. A new
; operation requires an update of all existing packages.

; # Message-passing style
; Same as with Data-directed style.