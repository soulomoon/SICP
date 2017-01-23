; Exercise 4.41: Write an ordinary Scheme program to solve the multiple dwelling puzzle.
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
(define (reduce f argslist)
  (if (null? argslist)
      '()
      (f
        (car argslist)
        (reduce f (cdr argslist)))))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))
(define (combiner A L)
  (define r (list))
  (if (null? L)
      (map list A)
      (begin 
        (for-each 
          (lambda (a) 
            (for-each 
              (lambda (l) (set! r (cons (cons a l) r)))
              L))
          A)
        r)))
(define predict-list '())
(define (multiple-dwelling)
  (let ((baker (list 1 2 3 4 5))
        (cooper (list 1 2 3 4 5))
        (fletcher (list 1 2 3 4 5))
        (miller (list 1 2 3 4 5))
        (smith (list 1 2 3 4 5)))
       (define (get-baker possible)
        (car possible))
       (define (get-cooper possible)
        (cadr possible))
       (define (get-fletcher possible)
        (caddr possible))
       (define (get-miller possible)
        (cadddr possible))
       (define (get-smith possible)
        (car (cddddr possible)))
       
       (define all-possible
        (reduce combiner (list baker cooper fletcher miller smith)))

       (define muterable-pair (list predict-list))
       (define (set-append predicate)
        (set-car! muterable-pair (cons predicate (car muterable-pair))))
       
       (define (filter-manager predict-list init)
         (if (null? predict-list)
             init
             (filter-manager 
               (cdr predict-list)
               (filter (car predict-list) init))))       
       
       
       (set-append
        (lambda (possible)
          (not (= (get-baker possible) 5))))
       (set-append 
        (lambda (possible)
          (not (= (get-cooper possible) 1))))
       (set-append 
        (lambda (possible)
          (not (or (= (get-fletcher possible) 1) (= (get-fletcher possible) 5)))))
       (set-append 
        (lambda (possible)
          (> (get-miller possible) (get-cooper possible))))
       (set-append 
        (lambda (possible)
          (not (= (abs (- (get-smith possible) (get-fletcher possible))) 1))))
       (set-append 
        (lambda (possible)
          (not (= (abs (- (get-cooper possible) (get-fletcher possible))) 1))))
       (set-append 
        (lambda (possible)
          (distinct? possible)))
       (filter-manager (car muterable-pair) all-possible)
))
(display (multiple-dwelling))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 512 MB.
; ((3 2 4 5 1))
; > 