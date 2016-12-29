; Exercise 3.69: Write a procedure triples that takes three infinite streams, SS, TT, and UU, and produces the stream of triples (Si,Tj,Uk)(Si,Tj,Uk) such that i≤j≤ki≤j≤k. Use triples to generate the stream of all Pythagorean triples of positive integers, i.e., the triples (i,j,k)(i,j,k) such that i≤ji≤j and i2+j2=k2i2+j2=k2.
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples S T U)
  (cons-stream
    (map stream-car (list S T U))
    (interleave
      (stream-map (lambda (x)
                    (cons (stream-car S) x))
                    (pairs T (stream-cdr U)))
      (apply triples (map stream-cdr (list S T U))))))

(define (pythagorean_pre s)
  (let ((i (car s))
        (j (cadr s))
        (k (caddr s)))
       (if (= (+ (square i) (square j)) (square k))
        true
        false)))
(define Pythagorean
  (stream-filter pythagorean_pre (triples integers integers integers)))


(display-10 Pythagorean)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; (3 4 5)
; (6 8 10)
; (5 12 13)
; (9 12 15)
; (8 15 17)
; (12 16 20)
; (15 20 25)