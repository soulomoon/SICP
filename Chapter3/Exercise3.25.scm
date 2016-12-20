;Exercise 3.25: Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. The lookup and insert! procedures should take as input a list of keys used to access the table.
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (iter key-list table)
        (let ((subtable
                (assoc (car key-list) (cdr table)))
              (remain-key-list (cdr key-list)))
              
              (if subtable  
                (if (null? remain-key-list)
                  (cdr subtable)
                  (iter remain-key-list subtable))
              
              false)))
      (iter key-list local-table))
    
    (define (insert! key-list value)
      (define (iter key-list table)
        (if (pair? (cdr table))
          (let ((subtable 
                  (assoc (car key-list) (cdr table)))
                (remain-key-list (cdr key-list)))
                (if subtable
                  (if (null? remain-key-list)
                    (set-cdr! subtable value)
                    (iter remain-key-list subtable))

                  (let ((newtable (list (car key-list))))
                    (begin
                      (set-cdr! table
                        (cons newtable
                              (cdr table)))
                    (iter key-list table)))))
          
          (let ((newtable (list (car key-list))))
            (begin
              (set-cdr! table
                (list newtable))
            (iter key-list table))))
            'ok)

      (iter key-list local-table)
      'ok local-table)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(display (put (list 'b) 0))(newline )
(display (put (list 'a) 0))(newline )
(display (put (list 'a 'b) 1))(newline )
(display (put (list 'a 'b 'c) 2))(newline )
(display (get (list 'a 'b)))(newline )
(display (get (list 'a 'b 'c)))(newline )

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (*table* (b . 0))
; (*table* (a . 0) (b . 0))
; (*table* (a (b . 1)) (b . 0))
; (*table* (a (b (c . 2))) (b . 0))
; ((c . 2))
; 2
; > 