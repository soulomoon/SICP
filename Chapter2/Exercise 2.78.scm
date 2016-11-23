; Exercise 2.78: The internal procedures in the scheme-number package are essentially nothing more than calls to the primitive procedures +, -, etc. It was not possible to use the primitives of the language directly because our type-tag system requires that each data object have a type attached to it. In fact, however, all Lisp implementations do have a type system, which they use internally. Primitive predicates such as symbol? and number? determine whether data objects have particular types. Modify the definitions of type-tag, contents, and attach-tag from 2.4.2 so that our generic system takes advantage of Scheme’s internal type system. That is to say, the system should work as before except that ordinary numbers should be represented simply as Scheme numbers rather than as pairs whose car is the symbol scheme-number.

;;; Code for creating the table, you don't need to worry about this.

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(define (attach-tag type-tag contents)
    (cond 
        ((or (symbol? contents) (numbers? contents)) contents) 
        (else (cons type-tag contents)) 
    )
)

(define (type-tag datum)
    (cond
        ((symbol? datum) 'symbol)
        ((number? datum) 'number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: 
                  TYPE-TAG" datum)))
)

(define (contents datum)
    (cond 
        ((or (symbol? contents) (number? contents)) contents) 
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: 
                CONTENTS" datum)))
)

(type-tag 1)
(type-tag 'a)
(type-tag '(type? a))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'number
; 'symbol
; 'type?
; > 