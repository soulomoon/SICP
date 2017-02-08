; Exercise 4.75: Implement for the query language a new special form called unique. Unique should succeed if there is precisely one item in the data base satisfying a specified query. For example,

; (unique (job ?x (computer wizard)))
; should print the one-item stream

; (unique (job (Bitdiddle Ben)
;              (computer wizard)))
; since Ben is the only computer wizard, and

; (unique (job ?x (computer programmer)))
; should print the empty stream, since there is more than one computer programmer. Moreover,

; (and (job ?x ?j) 
;      (unique (job ?anyone ?j)))
; should list all the jobs that are filled by only one person, and the people who fill them.

; There are two parts to implementing unique. The first is to write a procedure that handles this special form, and the second is to make qeval dispatch to that procedure. The second part is trivial, since qeval does its dispatching in a data-directed way. If your procedure is called uniquely-asserted, all you need to do is

; (put 'unique 'qeval uniquely-asserted)
; and qeval will dispatch to this procedure for every query whose type (car) is the symbol unique.

; The real problem is to write the procedure uniquely-asserted. This should take as input the contents (cdr) of the unique query, together with a stream of frames. For each frame in the stream, it should use qeval to find the stream of all extensions to the frame that satisfy the given query. Any stream that does not have exactly one item in it should be eliminated. The remaining streams should be passed back to be accumulated into one big stream that is the result of the unique query. This is similar to the implementation of the not special form.

; Test your implementation by forming a query that lists all people who supervise precisely one person.
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS (list->stream assertions))
           (set! THE-RULES (list->stream rules))
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (cdr r-and-a)
                              (cons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (cdr r-and-a)
                              rules
                              (cons s assertions))))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  (put 'unique 'qeval uniquely-asserted)
  (deal-out rules-and-assertions '() '()))
(define (unique-query exps) (car exps))

(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((processed-frame (qeval (unique-query operands)
                                   (singleton-stream frame))))
       (if (stream-null? (stream-cdr processed-frame))
           processed-frame
           the-empty-stream)))
   frame-stream))

(initialize-data-base microshaft-data-base)


(inqu
 '(
   (unique (job ?x (computer wizard)))
   (unique (job (Bitdiddle Ben)
                (computer wizard)))
   (unique (job ?x (computer programmer)))
   (and (job ?x ?j) 
        (unique (job ?anyone ?j)))
   (and (supervisor ?x ?j) 
        (unique (supervisor ?anyone ?j)))
   ))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'done


; (unique (job (Bitdiddle Ben) (computer wizard)))


; (unique (job (Bitdiddle Ben) (computer wizard)))




; (and (job (Aull DeWitt) (administration secretary)) (unique (job (Aull DeWitt) (administration secretary))))
; (and (job (Cratchet Robert) (accounting scrivener)) (unique (job (Cratchet Robert) (accounting scrivener))))
; (and (job (Scrooge Eben) (accounting chief accountant)) (unique (job (Scrooge Eben) (accounting chief accountant))))
; (and (job (Warbucks Oliver) (administration big wheel)) (unique (job (Warbucks Oliver) (administration big wheel))))
; (and (job (Reasoner Louis) (computer programmer trainee)) (unique (job (Reasoner Louis) (computer programmer trainee))))
; (and (job (Tweakit Lem E) (computer technician)) (unique (job (Tweakit Lem E) (computer technician))))
; (and (job (Bitdiddle Ben) (computer wizard)) (unique (job (Bitdiddle Ben) (computer wizard))))


; (and (supervisor (Cratchet Robert) (Scrooge Eben)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
; (and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
; > 