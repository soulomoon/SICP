(load "/home/soulomoon/Documents/git/SICP/Chapter2/source.scm")
; Exercise 2.82: Show how to generalize apply-generic to handle coercion in the general case of multiple arguments. One strategy is to attempt to coerce all the arguments to the type of the first argument, then to the type of the second argument, and so on. Give an example of a situation where this strategy (and likewise the two-argument version given above) is not sufficiently general. (Hint: Consider the case where there are some suitable mixed-type operations present in the table that will not be tried.)


(define (apply-generic op . args)
  (define (try_get_coercion t1 t2)
    (if (eq? t1 t2)
        (lambda (t) t)
        (get-coercion tag-target tag-source)
    )
  )
  (define (get_coercions type-tags)
    (map 
      (lambda (target_tag) 
              (map (lambda (source_tag)
                           (try_get_coercion source_tag target_tag)
                           type-tags
                   )
              )
      ) 
      type-tags
    )
  )
  (define (test_coercion procs)
    (cond 
        ((null? procs) true)
        ((null? (car procs)) false)
        (else (test_coercion (cdr procs)))
    )
  )

  (define (find_coercion procs_list)
    (if (null? procs_list)
        false
        (if (test_coercion (car procs_list)) 
            (car procs_list)
            (find_coercion (cdr procs_list))
        )
    )
  )

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 2)
              (let ((procs_list (get_coercions type-tags)))
                  (let ((A->B (find_coercion procs_list)))
                       (if A->B
                           (apply apply-generic
                              (cons op (map contents (A->B args)))
                           )
                           (error "No method for these types" (list op type-tags))
                       )
                  )
              )
              (error 
                "No method for these types"
                (list op type-tags)
              )
          )
      )
    )
  )
)

(define scheme-number->rational 
  (lambda (x) (make-rational x 1))
)
(put-coercion 'scheme-number 'rational
              scheme-number->rational)
(get-coercion 'scheme-number 'rational)


(define a (make-complex-from-real-imag 0 0))
(define b (make-complex-from-real-imag 1 0))
(define c (make-rational 0 3))
(define d (make-rational 1 3))
(display (=zero? a))(newline)
(display (=zero? b))(newline)
(display (=zero? 0))(newline)
(display (=zero? 1))(newline)
(display (=zero? c))(newline)
(display (=zero? d))(newline)