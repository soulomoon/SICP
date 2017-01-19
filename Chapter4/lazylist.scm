(load "/Users/soulomoon/git/SICP/Chapter4/lazyeval.scm")



(interpret
'(begin
(define (cons x y) (list 'lazy (lambda (m) (m x y))))
(define (car z) ((cadr z) (lambda (p q) p)))
(define (cdr z) ((cadr z) (lambda (p q) q)))
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) 
                       (car list2))
                    (add-lists
                     (cdr list1) 
                     (cdr list2))))))

; (define ones (cons 1 ones))
; (define integers 
;   (cons 1 (add-lists ones integers)))
; (define a 1)
))

(define (eval-quoted exp env)
  (text-of-quotation exp env))

(define (text-of-quotation exp env)
  (let ((result (cadr exp)))
    (if (pair? result)
        (eval# (make-list result) env)
        result)))
 
(define (make-list result)
  (if (null? result)
      '()
      (list 'cons 
            (list 'quote (car result)) 
            (make-list (cdr result)))))
(put-syntax! 'quote eval-quoted) 

; (driver-loop)
; (interpret 
; ''(1 2 3)
; )