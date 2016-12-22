; Exercise 3.27: Memoization (also called tabulation) is a technique that enables a procedure to record, in a local table, values that have previously been computed. This technique can make a vast difference in the performance of a program. A memoized procedure maintains a table in which values of previous calls are stored using as keys the arguments that produced the values. When the memoized procedure is asked to compute a value, it first checks the table to see if the value is already there and, if so, just returns that value. Otherwise, it computes the new value in the ordinary way and stores this in the table. As an example of memoization, recall from 1.2.2 the exponential process for computing Fibonacci numbers:
(define (make-table)
  (list '*table*))
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) 
                        (cdr table)))))
  'ok)


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
; The memoized version of the same procedure is

; (define (memoize f)
;   (let ((table (make-table)))
;     (lambda (x)
;       (let ((previously-computed-result 
;              (lookup x table)))
;         (or previously-computed-result
;             (let ((result (f x)))
;               (insert! x result table)
;               result))))))

; (define memo-fib
;   (memoize 
;    (lambda (n)
;      (cond ((= n 0) 0)
;            ((= n 1) 1)
;            (else 
;             (+ (memo-fib (- n 1))
;                (memo-fib (- n 2))))))))
; where the memoizer is defined as

; Draw an environment diagram to analyze the computation of (memo-fib 3). Explain why memo-fib computes the nthnth Fibonacci number in a number of steps proportional to nn. Would the scheme still work if we had simply defined memo-fib to be (memoize fib)?

;unfold
(define memo-fib
  ((lambda (f)
    ((lambda (table)
      (lambda (x)
	((lambda (previously-computed-result)
	  (or previously-computed-result
	      ((lambda (result)
   		(insert! x result table)
		result) 
	       (f x))))
	 (lookup x table))))
     (make-table)))
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else 
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2)))))))) 
;if being define as (memoize fib),firt fib would have the memoize process, and the recursive fib would be just fib.


(define (f x) (+ x 1))
(define (g x) (f x))
(g 1)
(set! f (lambda (x) (+ x 10)))
(g 1)

(memo-fib 99)








                +-------------+  
env-------------|  memo-fib   |        
                +-------------+  
                           | 
                        +--------+
                        | n:3    |
                        |        |
                        +--------+
                      




