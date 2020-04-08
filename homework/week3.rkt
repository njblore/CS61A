#lang simply-scheme

; Exercise 1.35.  
; Show that the golden ratio (section 1.2.2) is a fixed point of the transformation x |-> 1 + 1/x, 
; and use this fact to compute by means of the fixed-point procedure.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden x)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(golden 4)

; Exercise 1.37

;;; (define (cont-frac n d k)
;;;     )

; 2: Write a procedure (next-perf n) that tests numbers starting with n and continuing with n+1, n+2, etc. until a perfect number is 
; found. Then find the next perfect number after 28.

(define (sum-of-factors n x)
  (cond ((= x n) 0)
        ((= 0 (remainder n x)) (+ x (sum-of-factors n (+ 1 x))))
        (else (+ 0 (sum-of-factors n (+ 1 x))))))

(define (next-perf n)
  (if (= n (sum-of-factors n 1)) n 
  (next-perf (+ 1 n))))

(next-perf 29) ; = 496, correct!

