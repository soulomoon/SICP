; Exercise 3.67: Modify the pairs procedure so that (pairs integers integers) will produce the stream of all pairs of integers (i,j)(i,j) (without the condition i≤ji≤j). Hint: You will need to mix in an additional stream.
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

; (define (pairs s t)
;   (cons-stream
;    (list (stream-car s) (stream-car t))
;    (interleave
;     (stream-map (lambda (x) 
;                   (list (stream-car s) x))
;                 (stream-cdr t))
;     (pairs (stream-cdr s) (stream-cdr t)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
    (interleave
      (interleave 
        (stream-map (lambda (x) 
                      (list (stream-car s) x))
                    (stream-cdr t))
        (stream-map (lambda (x) 
                      (list x (stream-car s)))
                    (stream-cdr t)))
        (pairs (stream-cdr s) (stream-cdr t)))))

(display-10 (pairs integers integers))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; (1 1)
; (1 2)
; (2 2)
; (2 1)
; (2 3)
; (1 3)
; (3 3)
; (3 1)
; (3 2)
; (1 4)'done
; > 