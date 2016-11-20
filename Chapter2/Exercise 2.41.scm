; Exercise 2.41: Write a procedure to find all ordered triples of distinct positive integers ii, jj, and kk less than or equal to a given integer nn that sum to a given integer ss.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
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

(define (unique_pairs n)
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

(define (unique_triples n)
    (flatmap
        (lambda (x) (map (lambda (y) (cons x y)) (unique_pairs (- x 1))))
        (enumerate-interval 1 n)
    )
)



(define (make_triples_sum triple)
    (list 
        (car triple)
        (cadr triple)
        (caddr triple)
        (+ (car triple) (cadr triple) (caddr triple))
    )
)

(define (dispack s n)
    (define (equal_s seq)
        (= (cadddr seq) s)
    )
    (filter equal_s
            (map make_triples_sum (unique_triples n))))

(display (dispack 50 30))
(newline)
(display (dispack 10 5))

Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
((18 17 15 50) (19 16 15 50) (19 17 14 50) (19 18 13 50) (20 16 14 50) (20 17 13 50) (20 18 12 50) (20 19 11 50) (21 15 14 50) (21 16 13 50) (21 17 12 50) (21 18 11 50) (21 19 10 50) (21 20 9 50) (22 15 13 50) (22 16 12 50) (22 17 11 50) (22 18 10 50) (22 19 9 50) (22 20 8 50) (22 21 7 50) (23 14 13 50) (23 15 12 50) (23 16 11 50) (23 17 10 50) (23 18 9 50) (23 19 8 50) (23 20 7 50) (23 21 6 50) (23 22 5 50) (24 14 12 50) (24 15 11 50) (24 16 10 50) (24 17 9 50) (24 18 8 50) (24 19 7 50) (24 20 6 50) (24 21 5 50) (24 22 4 50) (24 23 3 50) (25 13 12 50) (25 14 11 50) (25 15 10 50) (25 16 9 50) (25 17 8 50) (25 18 7 50) (25 19 6 50) (25 20 5 50) (25 21 4 50) (25 22 3 50) (25 23 2 50) (25 24 1 50) (26 13 11 50) (26 14 10 50) (26 15 9 50) (26 16 8 50) (26 17 7 50) (26 18 6 50) (26 19 5 50) (26 20 4 50) (26 21 3 50) (26 22 2 50) (26 23 1 50) (27 12 11 50) (27 13 10 50) (27 14 9 50) (27 15 8 50) (27 16 7 50) (27 17 6 50) (27 18 5 50) (27 19 4 50) (27 20 3 50) (27 21 2 50) (27 22 1 50) (28 12 10 50) (28 13 9 50) (28 14 8 50) (28 15 7 50) (28 16 6 50) (28 17 5 50) (28 18 4 50) (28 19 3 50) (28 20 2 50) (28 21 1 50) (29 11 10 50) (29 12 9 50) (29 13 8 50) (29 14 7 50) (29 15 6 50) (29 16 5 50) (29 17 4 50) (29 18 3 50) (29 19 2 50) (29 20 1 50) (30 11 9 50) (30 12 8 50) (30 13 7 50) (30 14 6 50) (30 15 5 50) (30 16 4 50) (30 17 3 50) (30 18 2 50) (30 19 1 50))
((5 3 2 10) (5 4 1 10))
> 