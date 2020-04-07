#lang simply-scheme

; Write a procedure called product that returns the product of the values of a function at points over a given range. 
; Show how to define factorial in terms of product. 

(define (product term a next b)
    (if (= a b)
        1
        (* (term a) (product term (next a) next b))))


(define (identity x) x)
(define (dec x) (- x 1))

(define (factorial x)
    (product identity x dec 0))

(factorial 3)
(factorial 5)
(factorial 10)


; Show that sum and product (exercise 1.31) are both special cases of a still more general notion called accumulate that 
; combines a collection of terms, using some general accumulation function:
; (accumulate combiner null-value term a next b)
; Accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner procedure 
; (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a 
; null-value that specifies what base value to use when the terms run out. Write accumulate and show how sum and product can
;  both be defined as simple calls to accumulate.

(define (accumulate combiner null-value term a next b)
    (if (= a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumuFactor x y)
    (accumulate * 1 identity x dec y))

(accumuFactor 3 0)
(accumuFactor 5 0)
(accumuFactor 10 0)

; combine only those terms derived from values in the range that satisfy a specified condition. 
; The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional 
; predicate of one argument that specifies the filter.

; a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)

(define (square a) (* a a))
(define (inc a) (+ a 1))

(define (filtered-accumulate filter combiner null-value term a next b)
    (if (= a b)
    0
    (if (filter a)
    (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
    (combiner null-value (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (even-square-sum a b)
    (filtered-accumulate even? + 0 square a inc b))

(even-square-sum 1 5)


; Create a higher-order procedure called every that applies an arbitrary procedure, given as an argument, 
; to each word of an argument sentence. 

(define (every proc sen result)
    (if (empty? sen)
        result
        (every proc (butfirst sen) (sentence result (proc (first sen))))))


(every square '(1 2 3 4) '())

; EXTRA: Your task is to find a way to express the fact procedure in a Scheme without any way to define global names

(define (fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))

;;; ((lambda (n)
;;;     (let* ((a (lambda (x) (if (= x 0) 1 (* x (b (- n 1))))))
;;;             (b (lambda (y) (if (= y 0) 1 (* y (a (- n 1)))))))
;;;             (b n))))