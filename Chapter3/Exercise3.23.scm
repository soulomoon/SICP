; Exercise 3.23: A deque (“double-ended queue”) is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, rear-delete-deque!. Show how to represent deques using pairs, and give implementations of the operations.151 All operations should be accomplished in Θ(1)Θ(1) steps.

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (set-front-ptr! i)
      (set! front-ptr i))
    (define (set-rear-ptr! i)
      (set! rear-ptr i))
    (define (empty-queue?) 
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an 
                  empty queue" front-ptr)
          (car front-ptr)))
    
    (define (rear-deque)
      (if (empty-queue?)
          (error "REAR called with an 
                  empty queue" front-ptr)
          (car rear-ptr)))


    (define (front-insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
              (set-front-ptr! new-pair)
              (set-rear-ptr! new-pair)
              dispatch)
              (else (set! front-ptr 
                          (set-cdr! new-pair
                                    front-ptr)
                    dispatch))))

    (define (rear-insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
              (set-front-ptr! new-pair)
              (set-rear-ptr! new-pair)
              dispatch)
              (else (set-cdr! rear-ptr 
                              new-pair)
                    (set-rear-ptr! new-pair)
                    dispatch))))


    (define (front-delete-queue!)
      (cond ((empty-queue?)
            (error "DELETE! called with 
                    an empty queue" front-ptr))
            (else (set-front-ptr!  
                    (cdr front-ptr))
                  dispatch)))
    (define (rear-delete-queue!)
      (cond ((empty-queue?)
            (error "DELETE! called with 
                    an empty queue" front-ptr))
            (else (set-front-ptr!  
                    (cdr front-ptr))
                  dispatch)))
    
    
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



(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) 
  (set-car! (cdr queue) item))
(define (set-rear-ptr! queue item) 
  (set-cdr! (cdr queue) item))

(define (empty-queue? queue) 
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))


(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item (cons '() '()))))
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
