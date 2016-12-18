; Exercise 3.22: Instead of representing a queue as a pair of pointers, we can build a queue as a procedure with local state. The local state will consist of pointers to the beginning and the end of an ordinary list. Thus, the make-queue procedure will have the form

; (define (make-queue)
;   (let ((front-ptr … )
;         (rear-ptr … ))
;     ⟨definitions of internal procedures⟩
;     (define (dispatch m) …)
;     dispatch))
; Complete the definition of make-queue and provide implementations of the queue operations using this representation.

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! i)
      (set! front-ptr i)
    )
    (define (set-rear-ptr! i)
      (set! rear-ptr i)
    )
    (define (dispatch m)
      (cond
        ((eq? m 'front-ptr) front-ptr)
        ((eq? m 'rear-ptr) rear-ptr)
        ((eq? m 'set-front-ptr!) set-front-ptr!)
        ((eq? m 'set-rear-ptr!) set-rear-ptr!)
        (else (error "unknow"))
      )    
    )
    dispatch))


(define (front-ptr q)
  (q 'front-ptr)
)
(define (rear-ptr q)
  (q 'rear-ptr)
)
(define (set-front-ptr! q i)
  ((q 'set-front-ptr!) i)
)
(define (set-rear-ptr! q i)
  ((q 'set-rear-ptr!) i)
)
(define (empty-queue? queue) 
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))


(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with 
                 an empty queue" queue))
        (else (set-front-ptr! 
               queue 
               (cdr (front-ptr queue)))
              queue)))


(define (print_queue q)
  (display (front-ptr q))(newline))

(define q1 (make-queue))

(insert-queue! q1 'a)
(print_queue q1)

(insert-queue! q1 'b)
(print_queue q1)

(insert-queue! q1 'c)
(print_queue q1)

(delete-queue! q1)
(print_queue q1)

(delete-queue! q1)
(print_queue q1)

(delete-queue! q1)
(print_queue q1)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; #<procedure>
; (a)
; #<procedure>
; (a b)
; #<procedure>
; (a b c)
; #<procedure>
; (b c)
; #<procedure>
; (c)
; #<procedure>
; ()
; > 