(ns ch2_2.ex2-6)

; (define zero (lambda (f) (lambda (x) x)))
; (define (add-1 n) (lambda (f) (lambda (x) (f (n f) x))))
;
; (add-1 zero)
; (lambda (f) (lambda (x) (f (zero f) x)))
; (lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) f) x)))
; (lambda (f) (lambda (x) (f (lambda (x) x) x)))
; (lambda (f) (lambda (x) (f x)))
; (define one (lambda (f) (lambda (x) (f x)))
;
; (add-1 one)
; (lambda (f) (lambda (x) (f (one f) x)))
; (lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) (f x))) f) x)))
; (lambda (f) (lambda (x) (f (lambda (x) (f x)) x)))
; (lambda (f) (lambda (x) (f (f x))))
; (define two (lambda (f) (lambda (x) (f (f x)))))
;
; ;; Adds two Church numerals without repeated application of add-1.
; (define (add n m) (lambda (f) (lambda (x) ((n f) ((m f) x)))))
;
; (add two one)
; (lambda (f) (lambda (x) ((two f) ((one f) x))))
; (lambda (f) (lambda (x) ((two f) (((lambda (f) (lambda (x) (f x)) f) x))))
; (lambda (f) (lambda (x) ((two f) ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) ((two f) (f x))))
; (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f (f x)))) f) (f x))))
; (lambda (f) (lambda (x) ((lambda (x) (f (f x))) (f x))))
; (lambda (f) (lambda (x) (f (f (f x)))))