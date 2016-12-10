; Exercise 3.4: Modify the make-account procedure of Exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure call-the-cops.

(define (make-account balance password_server times_to_call_cops)
  (define (call-the-cops amount)
    "calling the cops"
  )
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
    
  (define (dispatch password m)
    (if (eq? password password_server)
        (begin (set! times_to_call_cops 0)
              (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    (else (error "Unknown request: 
                          MAKE-ACCOUNT" m)))

        )
        (begin (set! times_to_call_cops (+ 1 times_to_call_cops))
               (if (< 7 times_to_call_cops)
                  call-the-cops
                  (lambda (x) "Incorrect password"))))
               )
  dispatch)

(define acc 
  (make-account 100 'secret-password 0))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40)
; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 60
; "Incorrect password"
; "Incorrect password"
; "Incorrect password"
; "Incorrect password"
; "Incorrect password"
; "Incorrect password"
; "Incorrect password"
; "calling the cops"
; "calling the cops"
; "calling the cops"
; "calling the cops"
; 20
; > 