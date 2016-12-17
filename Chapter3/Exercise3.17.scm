; Exercise 3.17: Devise a correct version of the count-pairs procedure of Exercise 3.16 that returns the number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.)

(define (count_pairs x)
  (define counted_list '())
  (define (count_pairs_inner x)
    (cond
      ((not (pair? x)) 0)
      ((member x counted_list) 0)
      (else (begin (set! counted_list (cons x counted_list)) 
            (+ (count_pairs_inner (car x))
               (count_pairs_inner (cdr x))
               1)))))
  (count_pairs_inner x)
  )

(define (count_pairs_plain x)
  (if (not (pair? x))
      0
      (+ (count_pairs_plain (car x))
         (count_pairs_plain (cdr x))
         1)))

(define a (cons 'a 'b))
(define b (cons 'c 'd))
(define c (cons a b))
(define d (cons a c))
(count_pairs d)
(count_pairs_plain d)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 4
; 5
; > 