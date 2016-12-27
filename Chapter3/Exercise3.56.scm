; Exercise 3.56: A famous problem, first raised by R. Hamming, is to enumerate, in ascending order with no repetitions, all positive integers with no prime factors other than 2, 3, or 5. One obvious way to do this is to simply test each integer in turn to see whether it has any factors other than 2, 3, and 5. But this is very inefficient, since, as the integers get larger, fewer and fewer of them fit the requirement. As an alternative, let us call the required stream of numbers S and notice the following facts about it.

; S begins with 1.
; The elements of (scale-stream S 2) are also elements of S.
; The same is true for (scale-stream S 3) and (scale-stream S 5).
; These are all the elements of S.
; Now all we have to do is combine elements from these sources. For this we define a procedure merge that combines two ordered streams into one ordered result stream, eliminating repetitions:

; (define (merge s1 s2)
;   (cond ((stream-null? s1) s2)
;         ((stream-null? s2) s1)
;         (else
;          (let ((s1car (stream-car s1))
;                (s2car (stream-car s2)))
;            (cond ((< s1car s2car)
;                   (cons-stream 
;                    s1car 
;                    (merge (stream-cdr s1) 
;                           s2)))
;                  ((> s1car s2car)
;                   (cons-stream 
;                    s2car 
;                    (merge s1 
;                           (stream-cdr s2))))
;                  (else
;                   (cons-stream 
;                    s1car
;                    (merge 
;                     (stream-cdr s1)
;                     (stream-cdr s2)))))))))
; Then the required stream may be constructed with merge, as follows:

; (define S (cons-stream 1 (merge ⟨??⟩ ⟨??⟩)))
; Fill in the missing expressions in the places marked ⟨??⟩ above.
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

(define integers 
  (cons-stream 1 (add-streams ones integers)))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define S (cons-stream 1 
            (merge (scale-stream integers 2) 
              (merge (scale-stream integers 3) (scale-stream integers 5)))))

(define a 
  (stream-map (lambda (n) 
                (stream-ref S n)) (stream-enumerate-interval 0 20)))

(display-stream a)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 1
; 2
; 3
; 4
; 5
; 6
; 8
; 9
; 10
; 12
; 14
; 15
; 16
; 18
; 20
; 21
; 22
; 24
; 25
; 26
; 27'done
; > 