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

; Explain the effect of interchanging the order in which the base cases in the cc procedure are checked

; ANSWER: When (or (< amount 0) (= kinds-of-coins 0)) is checked first, in a case where the amount is 0 AND the kinds of coins is zero, this
; wouldn't be counted as a 'way to make change', it would be added as a fail (0) even though it did make change (get to exactly 0).

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


; Give an algebraic formula relating the values of the parameters b, n, counter, and product of the expt and exp-iter procedures

; ANSWER: As n decreases by one, product is multiplied by b while b stays constant until n reaches 0.
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))




