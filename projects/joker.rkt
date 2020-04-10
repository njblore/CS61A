#lang simply-scheme

(define (best-total hand)

    (define (min-total hand)
        (cond ((empty? hand) 0)
          ((member? (butlast (first hand)) '(J Q K)) (+ 10 (min-total (butfirst hand))))
          ((member? (butlast (first hand)) '(A Jo)) (+ 1 (min-total (butfirst hand))))
          (else (+ (butlast (first hand)) (min-total (butfirst hand))))))

    (define (rank-count hand rank)
        (cond ((empty? hand) 0)
              ((equal? (butlast (first hand)) rank) (+ 1 (rank-count (butfirst hand) rank)))
              (else (+ 0 (rank-count (butfirst hand) rank)))))

	(define (get-values total count add-fn)
		(if (= 0 count) total
		(get-values (add-fn total '2) (- count 1) add-fn)))
	
    (define (try-add-jokers value n)
        (cond ((< 21 (+ n value)) (+ (- n 1) value))
              ((= 10 n) (+ n value))
              (else (try-add-jokers value (+ 1 n)))))

	(define (try-add-aces value n)
		(if (> 22 (+ 10 value)) (+ 10 value)
		value))

	(get-values (get-values (min-total hand) (rank-count hand 'A) try-add-aces) (rank-count hand 'Jo) try-add-jokers)
)

(best-total '(Jok 7c ))

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
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C) 'Jok 'Jok) )

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


(define (play-n strategy n)
	(cond ((= n 0) 0)
	(else (+ (twenty-one strategy) (play-n strategy (- n 1))))))


(define (dealer-sensitive hand-so-far dealer-up-card)
	(or (and (member? (butlast dealer-up-card) '(A 7 8 9 10 Q K J)) (< (best-total hand-so-far) 17)) 
		(and (member? (butlast dealer-up-card) '(2 3 4 5 6)) (< (best-total hand-so-far) 12))))


(define (stop-at n)
	(lambda (x) (< (best-total x) n)))

(define (suit-strategy suit first-strategy second-strategy)
	(define (is-the-suit-in-hand hand)
		(cond ((empty? hand) #f)
				((equal? (last (first hand)) suit) #t)
				(else (is-the-suit-in-hand (butfirst hand)))))
	(lambda (x) (if (is-the-suit-in-hand x) (first-strategy x) (second-strategy x))))


(define (new-valentine hand)
	((suit-strategy 'h (stop-at 19) (stop-at 17)) hand))


(define (majority first-strategy second-strategy third-strategy)
	(lambda (x) (cond ((equal? (first-strategy x) (second-strategy x)) (first-strategy x)) 
					  ((equal? (first-strategy x) (third-strategy x)) (first-strategy x))
					  (else (second-strategy x)))))

(define (reckless strategy)
	(define (hit? hand) (strategy hand))
	(lambda (x) (or (hit? x) (hit? (butlast x)))))


