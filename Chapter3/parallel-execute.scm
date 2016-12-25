#lang racket
(require sicp)
(provide make-mutex)
(provide test-and-set!)
(provide make-serializer)
(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))


(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))


; (define s (make-serializer))
; (define (test x) (begin (parallel-execute (lambda () (set! x (+ x 1)))
;                                           (lambda () (set! x (* x x))))
; (display x)(newline)))

; (define (loop f n)
;   (let ((x 10)) 
;     (if (= n 0)
;         false
;         (begin (set! n (- n 1)) (f x) (loop f n))
;     )
;   )
; )

; (loop test 100)