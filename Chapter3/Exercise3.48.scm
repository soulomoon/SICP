; Exercise 3.48: Explain in detail why the deadlock-avoidance method described above, (i.e., the accounts are numbered, and each process attempts to acquire the smaller-numbered account first) avoids deadlock in the exchange problem. Rewrite serialized-exchange to incorporate this idea. (You will also need to modify make-account so that each account is created with a number, which can be accessed by sending an appropriate message.)


;because if there are process trying to acesss serialized-exchange, they always try to enter the same serialized procedure on the account with smallest number

#lang racket
(require sicp)
(require "git/SICP/Chapter3/parallel-execute.scm")

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance number)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) 
             (balance-serializer withdraw))
            ((eq? m 'deposit) 
             (balance-serializer deposit))
            ((eq? m 'balance) 
             balance)
            ((eq? m 'serializer) 
             balance-serializer)
            ((eq? m 'number)
             number)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (let ((serialized_exchange (if (< (account1 'number) (account2 'number)) 
                                   (serializer1 (serializer2 exchange)
                                   (serializer2 (serializer1 exchange))))))
    (serialized_exchange account1 account2))))