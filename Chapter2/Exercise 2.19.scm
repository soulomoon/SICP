; Exercise 2.19: Consider the change_counting program of 1.2.2. It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure first_denomination and partly into the procedure count_change (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.
; We want to rewrite the procedure cc so that its second argument is a list of the values of the coins to use rather than an integer specifying which coins to use. We could then have lists that defined each kind of currency:
#lang planet neil/sicp

(define us_coins 
  (list 50 25 10 5 1))

(define uk_coins 
  (list 100 50 20 10 5 2 1 0.5))
; We could then call cc as follows:

; (cc 100 us_coins)
; 292
; To do this will require changing the program cc somewhat. It will still have the same form, but it will access its second argument differently, as follows:

(define (cc amount coin_values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no_more? coin_values)) 
         0)
        (else
         (+ (cc 
             amount
             (except_first_denomination 
              coin_values))
            (cc 
             (- amount
                (first_denomination 
                 coin_values))
             coin_values)))))
; Define the procedures first_denomination, except_first_denomination and no_more? in terms of primitive operations on list structures. Does the order of the list coin_values affect the answer produced by cc? Why or why not?

(define (except_first_denomination coin_values)
    (cdr coin_values)
)

(define (first_denomination coin_values)
    (car coin_values)
)

(define (no_more? coin_values)
    (null? coin_values)
)

; won't, all ways have to pass, no one is left
(cc 100 us_coins)

``````````````````````````````


Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
292
> 