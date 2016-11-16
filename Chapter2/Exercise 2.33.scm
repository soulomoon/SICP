; Exercise 2.33: Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:
(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y)
                (cons
                    (p x)
                    y
                )
              ) 
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (+
                    1
                    y
                )
              ) 
              0 sequence))


(define a (list 1 2 3 4))
(define b (list 5 6 (list 7 8)))
(length a)
(length b)
(display (append a b))
(newline)
(display (map square a))

Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
4
3
(1 2 3 4 5 6 (7 8))
(1 4 9 16)
> 