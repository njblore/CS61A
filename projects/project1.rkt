#lang simply-scheme

; 1. Make best-total
(define (best-total hand)
    (define (min-total hand)
        (cond ((empty? hand) 0)
          ((member? (butlast (first hand)) '(J Q K)) (+ 10 (min-total (butfirst hand))))
          ((equal? 'A (butlast (first hand))) (+ 1 (min-total (butfirst hand))))
          (else (+ (butlast (first hand)) (min-total (butfirst hand))))))
	(define (ace-count hand)
		(cond ((empty? hand) 0)
			  ((equal? (butlast (first hand)) 'A) (+ 1 (ace-count (butfirst hand))))
			  (else (+ 0 (ace-count (butfirst hand))))))
	(define (get-values total count)
		(if (= 0 count) total
		(get-values (try-add-ten total) (- count 1))))
	
	(define (try-add-ten value)
		(if (> 22 (+ 10 value)) (+ 10 value)
		value))
	(get-values (min-total hand) (ace-count hand))
)

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


; 2. Make stop-at-17
(define (stop-at-17 hand-so-far dealer-up-card)
	(< (best-total hand-so-far) 17))

; 3. Make play-n, plays n games with a given strategy and returns the number of games that the customer won 
; minus the number that s/he lost.

(define (play-n strategy n)
	(cond ((= n 0) 0)
	(else (+ (twenty-one strategy) (play-n strategy (- n 1))))))

; 4. Define a strategy named dealer-sensitive that takes a card if the dealer has an ace, 7, 8, 9, 10, 
; or picture card showing, AND the customer has less than 17. 
; OR the dealer has a 2, 3, 4, 5, or 6 showing, and the customer has less than 12.

(define (dealer-sensitive hand-so-far dealer-up-card)
	(or (and (member? (butlast dealer-up-card) '(A 7 8 9 10 Q K J)) (< (best-total hand-so-far) 17)) 
		(and (member? (butlast dealer-up-card) '(2 3 4 5 6)) (< (best-total hand-so-far) 12))))


; 5. Generalize part 2 above by defining a function stop-at. (stop-at n) should return a strategy that keeps hitting 
; until a hand’s total is n or more.

(define (stop-at n)
	(lambda (x) (< (best-total x) n)))

; 6. Write a valentine strategy that stops at 17 unless you have a heart in your hand, 
; in which case it stops at 19.

(define (valentine hand-so-far)
	(define (is-there-a-heart hand)
		(cond ((empty? hand) #f)
			  ((equal? (last (first hand)) 'h) #t)
			  (else (is-there-a-heart (butfirst hand)))))
	(if (is-there-a-heart hand-so-far) ((stop-at 19) hand-so-far)
	((stop-at 17) hand-so-far)))

; 7. Generalize part 6 above by defining a function suit-strategy that takes three argu-]ments: a suit (h,s,d,c), 
; a strategy to be used if your hand doesn’t include that suit, 
; and a strategy to be used if your hand does include that suit.

(define (suit-strategy suit first-strategy second-strategy)
	(define (is-the-suit-in-hand hand)
		(cond ((empty? hand) #f)
				((equal? (last (first hand)) suit) #t)
				(else (is-the-suit-in-hand (butfirst hand)))))
	(lambda (x) (if (is-the-suit-in-hand x) (first-strategy x) (second-strategy x))))

; Show how you could use this function and the stop-atfunction from part 5 to 
; redefine the valentine strategy of part 6.

(define (new-valentine hand)
	((suit-strategy 'h (stop-at 19) (stop-at 17)) hand))

; 8. Define a function majority that takes three strategies as arguments and produces a strategy as a result, 
; such that the result strategy always decides whether or not to “hit” by consulting the three argument strategies, 
; and going with the majority.

(define (majority first-strategy second-strategy third-strategy)
	(lambda (x) (cond ((equal? (first-strategy x) (second-strategy x)) (first-strategy x)) 
					  ((equal? (first-strategy x) (third-strategy x)) (first-strategy x))
					  (else (second-strategy x)))))

; 9. Write a procedure reckless that takes a strategy as its argument and returns another strategy. 
; This new strategy should take one more card than the original would

(define (reckless strategy)
	(define (hit? hand) (strategy hand))
	(lambda (x) (or (hit? x) (hit? (butlast x)))))

((reckless (stop-at 17))'(Kh 6d 4h))


