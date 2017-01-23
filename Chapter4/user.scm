; Exercise 4.41: Write an ordinary Scheme program to solve the multiple dwelling puzzle.

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (multiple-dwelling)
  (let ((baker (list 1 2 3 4 5))
        (cooper (list 1 2 3 4 5))
        (fletcher (list 1 2 3 4 5))
        (miller (list 1 2 3 4 5))
        (smith (list 1 2 3 4 5)))
       (set! 
        baker 
        (filter 
          (lambda (baker)
            (not (= baker 5))) 
          baker))
       (set! 
        cooper 
        (filter 
          (lambda (cooper)
            (not (= cooper 1)))
          cooper))
       (set! 
        fletcher 
        (filter 
          (lambda (fletcher)
            (not (or (= fletcher 1) (= fletcher 5))))
          fletcher))
       (define m&c (list)) 
        
        (for-each 
          (lambda (c)
            (for-each 
              (lambda (m) 
                (if (> m c) 
                    (set! m&c (cons (list m c) m&c))
                ))
            miller))
          cooper)

       m&c
))
(display (multiple-dwelling))