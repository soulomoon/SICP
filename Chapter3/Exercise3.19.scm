; Exercise 3.19: Redo Exercise 3.18 using an algorithm that takes only a constant amount of space. (This requires a very clever idea.)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (cycle? x) 
   (define (safe-cdr l) 
     (if (pair? l) 
         (cdr l) 
         '())) 
   (define (iter slow fast) 
     (cond ((not (pair? slow)) #f) 
           ((not (pair? fast)) #f) 
           ((eq? slow fast) #t)
           (else (iter (safe-cdr slow) (safe-cdr (safe-cdr fast)))))) 
   (iter (safe-cdr x) (safe-cdr (safe-cdr x))))
(cycle? (list 'a 'b 'c 'd))
(cycle? z)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; #f
; #t
; > 