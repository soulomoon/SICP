; Exercise 2.40: Define a procedure unique-pairs that, given an integer nn, generates the sequence of pairs (i,j)(i,j) with 1≤j<i≤n1≤j<i≤n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))
             
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (nique_pairs n)
    (flatmap 
        (lambda (x)
            (map 
                (lambda (y) (list x y))
                (enumerate-interval 1 (- x 1))
            )
        )
        (enumerate-interval 1 n)
    )
)

(display (nique_pairs 3))(newline)
(display (nique_pairs 5))(newline)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; ((2 1) (3 1) (3 2))
; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))
; > 