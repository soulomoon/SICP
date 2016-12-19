; Exercise 3.23: A deque (“double-ended queue”) is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, rear-delete-deque!. Show how to represent deques using pairs, and give implementations of the operations.151 All operations should be accomplished in Θ(1)Θ(1) steps.


; to enable for backward rear delete. we have two way pine

;    +------------+   +---------+
;    |            |   |         |
;    |           [*] [*]        |
;    |              ^           |    
;    v              |           v
; [*] [*]      [*] [*]       [*] [*]
;  |   |        | ^           |   |
;  a   v        b |           c   v
;   [/] [*]       |            [*] [/]
;        |        |             | 
;        +--------+-------------+

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) 
  (set-car! queue item))
(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))

(define (empty-queue? queue) 
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))


(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (car (front-ptr queue))))

(define (rear-insert-queue! queue item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-car! (cdr new-pair)
                          (rear-ptr queue))
                (set-cdr! (cdr (rear-ptr queue)) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (front-insert-queue! queue item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (cdr new-pair)
                          (front-ptr queue))
                (set-car! (cdr (front-ptr queue)) 
                          new-pair)
                (set-front-ptr! queue new-pair)
                queue))))


(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with 
                 an empty queue" queue))
        (else (set-front-ptr! 
               queue 
               (cddr (front-ptr queue)))
              queue)))

(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with 
                 an empty queue" queue))
        (else (set-rear-ptr!
               queue 
               (cadr (rear-ptr queue)))
              (set-cdr! (cdr (rear-ptr queue)) nil)
              queue)))




(define (print_queue q)
  (define (iter q)
    (if (null? q)
        nil
        (cons (car q) (iter (cddr q)))))
  (display (iter (front-ptr q)))(newline))

(define q1 (make-queue))

(rear-insert-queue! q1 'a)
(print_queue q1)

(rear-insert-queue! q1 'b)
(print_queue q1)

(rear-insert-queue! q1 'c)
(print_queue q1)

(front-insert-queue! q1 'd)
(print_queue q1)

(rear-delete-queue! q1)
(print_queue q1)

(front-delete-queue! q1)
(print_queue q1)

(front-delete-queue! q1)
(print_queue q1)

(front-delete-queue! q1)
(print_queue q1)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; {mcons {mcons 'a {mcons '() '()}} {mcons 'a {mcons '() '()}}}
; (a)
; {mcons #0={mcons 'a {mcons '() #1={mcons 'b {mcons #0# '()}}}} #1#}
; (a b)
; {mcons #0={mcons 'a {mcons '() #1={mcons 'b {mcons #0# #2={mcons 'c {mcons #1# '()}}}}}} #2#}
; (a b c)
; {mcons #0={mcons 'd {mcons '() #1={mcons 'a {mcons #0# #2={mcons 'b {mcons #1# #3={mcons 'c {mcons #2# '()}}}}}}}} #3#}
; (d a b c)
; {mcons #0={mcons 'd {mcons '() #1={mcons 'a {mcons #0# #2={mcons 'b {mcons #1# '()}}}}}} #2#}
; (d a b)
; {mcons #0={mcons 'a {mcons {mcons 'd {mcons '() #0#}} #1={mcons 'b {mcons #0# '()}}}} #1#}
; (a b)
; {mcons #0={mcons 'b {mcons #1={mcons 'a {mcons {mcons 'd {mcons '() #1#}} #0#}} '()}} #0#}
; (b)
; {mcons '() #0={mcons 'b {mcons #1={mcons 'a {mcons {mcons 'd {mcons '() #1#}} #0#}} '()}}}
; ()
; > 