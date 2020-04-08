#lang simply-scheme 

; Write a procedure squares that takes a sentence of numbers as its argument and returns a sentence 
; of the squares of the numbers:> (squares ’(2 3 4 5))(4 9 16 25)

(define (squares numlist result) 
    (if (empty? numlist) 
    result 
    (squares (butfirst numlist) (sentence (* (first numlist) (first numlist)) result))))

(squares '(2 3 4 5) '())

; Write a procedure switch that takes a sentence as its argument and returns a sentence in which every 
; instance of the words I or me is replaced by you, while every instance of you is replaced by me except at the 
; beginning of the sentence, where it’s replaced by I.(Don’t worry about capitalization of letters.) 
; Example:> (switch ’(You told me that I should wake you up))(i told you that you should wake me up)

(define (switch sen result) 
    (if (equal? (count sen) 1)
        (if (member? (first sen) '(i me)) 
        (sentence 'you result ) 
        (if (equal? (first sen) 'you)  
        (sentence 'I result) 
        (sentence (first sen) result))) 
        (if (member? (last sen) '(i me)) 
        (switch (butlast sen) (sentence 'you result)) 
        (if (equal? (last sen) 'you) 
        (switch (butlast sen) (sentence 'me result)) 
        (switch (butlast sen) (sentence (last sen) result)))) 
    ))

(switch '(you told me that i should wake you up) '())

; Write a predicat eordered? that takes a sentence of numbers as its argument and returns a true value if the numbers 
; are in ascending order, or a false value otherwise.

(define (ordered? numlist result)
    (if (empty? numlist)
    'true
    (if (not (> (first numlist) result))
    'false
    (ordered? (butfirst numlist) (first numlist))
    ))
)

(ordered? '(1 2 3) 0)
(ordered? '(3 2 6) 0)

; Write a procedure ends-e that takes a sentence as its argument and returns a sentence containing only those 
; words of the argument whose last letter is 
; E:> (ends-e ’(please put the salami above the blue elephant))(please the above the blue)

(define (ends-e sen result)
    (if (equal? (count sen) 1)
        (if (equal? (last (last sen)) 'e)
            (sentence (last sen) result)
            result
        )
        (if (equal? (last (last sen)) 'e)
            (ends-e (butlast sen) (sentence (last sen) result))
            (ends-e (butlast sen) result)
        )
    )
)

(ends-e '(please put the salami above the blue elephant) '())


; Your mission is to devise a test that will tell you whether Scheme’s
; and and or are special forms or ordinary functions.

