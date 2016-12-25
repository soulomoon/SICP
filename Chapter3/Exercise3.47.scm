; Exercise 3.47: A semaphore (of size nn) is a generalization of a mutex. Like a mutex, a semaphore supports acquire and release operations, but it is more general in that up to nn processes can acquire it concurrently. Additional processes that attempt to acquire the semaphore must wait for release operations. Give implementations of semaphores

; in terms of mutexes
; in terms of atomic test-and-set! operations.
#lang racket
(require sicp)
(require "git/SICP/Chapter3/parallel-execute.scm")
(define (make-mutex-semaphore n)
  (let ((lock-numbers n)
        (the-mutex (make-mutex)))
        (define (the-semaphore m)
            (cond 
              ((eq? m 'acquire) 
                (begin (the-mutex 'acquire) 
                  (if (> lock-numbers 0)
                      (begin (set! lock-numbers (- lock-numbers 1))
                              (the-mutex 'release))
                      (the-semaphore 'acquire))))
              ((eq? m 'release) 
                (begin (the-mutex 'acquire)
                  (if (>= lock-numbers n)
                      (error "no semaphore to be release")
                      (set! lock-numbers (+ lock-numbers 1)))
                  (the-mutex 'release)))))
          the-semaphore))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))


(define (make-test-semaphore n)
  (let ((lock-numbers n)
        (cell (list false)))
        (define (the-semaphore m)
            (cond 
              ((eq? m 'acquire) 
                  (if (and (> lock-numbers 0) (test-and-set! cell))
                      (begin (set! lock-numbers (- lock-numbers 1)) 
                              (set-car! cell false))
                      (the-semaphore 'acquire)))
              ((eq? m 'release)
                  (if (and (>= lock-numbers n) (test-and-set! cell))
                      (error "no semaphore to be release")
                      (set! lock-numbers (+ lock-numbers 1)))
                  (set-car! cell false))))
          the-semaphore))


(define a (make-mutex-semaphore 2))
(a 'acquire)
(a 'acquire)
(define b (make-test-semaphore 2))
(b 'acquire)
(b 'acquire)