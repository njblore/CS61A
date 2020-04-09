#lang simply-scheme

; 1. Make best-total
(define (best-total hand)

    (define (min-total hand)
        (cond ((empty? hand) 0)
          ((member? (butlast (first hand)) '(j q k)) (+ 10 (min-total (butfirst hand))))
          ((equal? 'a (butlast (first hand))) (+ 1 (min-total (butfirst hand))))
          (else (+ (butlast (first hand)) (min-total (butfirst hand))))))

	(define (ace-count hand)
		(cond ((empty? hand) 0)
			  ((equal? (butlast (first hand)) 'a) (+ 1 (ace-count (butfirst hand))))
			  (else (+ 0 (ace-count (butfirst hand))))))

	(define (get-values total count)
		(if (= 0 count) total
		(get-values (try-add-ten total) (- count 1))))
	
	(define (try-add-ten value)
		(if (> 22 (+ 10 value)) (+ 10 value)
		value))

	(get-values (min-total hand) (ace-count hand))
)

(best-total '(5f 2h ad))

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )