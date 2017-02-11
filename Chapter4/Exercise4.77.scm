; Exercise 4.77: In 4.4.3 we saw that not and lisp-value can cause the query language to give “wrong” answers if these filtering operations are applied to frames in which variables are unbound. Devise a way to fix this shortcoming. One idea is to perform the filtering in a “delayed” manner by appending to the frame a “promise” to filter that is fulfilled only when enough variables have been bound to make the operation possible. We could wait to perform filtering until all other operations have been performed. However, for efficiency’s sake, we would like to perform filtering as soon as possible so as to cut down on the number of intermediate frames generated.
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

; I would be using lambda to handle delay.
; every not and lisp value would just return an lambda with a frame
; I would use the name "promise-frame".
; this promise is actually a lambda 
; that should be apply to a frame

; handle a new data type "promise-frame".
; (list 'promise-frame promise frame)
(define (promise-frame? promise-frame)
  (tagged-list? promise-frame 'promise-frame))
(define (get-frame promise-frame)
  (caddr promise-frame))
(define (get-promise promise-frame)
  (cadr promise-frame))

(define (combine-promises p1 p2)
  (delay (lambda (frame) (p2 (p1 frame)))))

; force out a promise means to apply lambda to the frame.
(define (force-promise-frame frame)
  (if (tagged-list? frame 'promise-frame)
      ((get-promise frame) (get-frame frame))
      frame))

(define (make-promise-frame promise frame)
  (if (promise-frame? frame)
      (list 
       'promise-frame 
       ; combination of the old promise and the new one.
       (combine-promises 
        (get-promise frame)
        promise)
       (get-frame frame))
      (list 'promise-frame promise frame)))
  

; handle a frame with promise with delay
; frame wouold not be force but to accumualate if new promise
(define (delay-qeval query promise-frame-stream)
  (define (qeval-inner query frame-stream)
    (let ((qproc (get (type query) 'qeval)))
      (if qproc
          (qproc (contents query) frame-stream)
          (simple-query query frame-stream))))
  (if (tagged-list? promise-frame-stream 'promise-frame)
      ; stash the promise
      (make-promise-frame
       (get-promise promise-frame-stream)
       (qeval-inner 
        query 
        (get-frame promise-frame-stream)))
      (qeval-inner query promise-frame-stream)))

; handle a frame with promise with force:
; frame would be force if it is delayed
(define (force-qeval query promise-frame-stream)
  (force-promise-frame (delay-qeval query promise-frame-stream)))

; set the default case of qeval to be forcing
(define qeval force-qeval)

; I can think of no other way to implement the force promise 
; process in other way than to put it in the last,
; so i would put it in the last....

; but it turns out I can not put is to the last.
; you have to force it before anything another operation except for "and"
; for (or (not a) (and a b)), 
; that you have to force the evaluation of before combination of or.
; but force (and (not a) b),
; here you have to delay "not".

; since the normal case is to force a delayed frame,
; you have to change the implementation of "and" to use delay-qeval

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (delay-qeval (first-conjunct conjuncts)
                            frame-stream))))

; and modify "not" to give a promised frame instead of qeval it.
(define (negate operands frame-stream)
  (define (negate-inner operands frame-stream)
    (stream-flatmap
     (lambda (frame)
       (if (stream-null? (qeval (negated-query operands)
                                (singleton-stream frame)))
           (singleton-stream frame)
           the-empty-stream))
     frame-stream))
  (make-promise-frame (lambda (frame) (negate-inner operands frame))frame-stream))

(define (lisp-value call frame-stream)
  (define (lisp-value-inner call f-stream)
    (stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate
            call
            frame
            (lambda (v f)
              (error "Unknown pat var -- LISP-VALUE" v))))
          (singleton-stream frame)
          the-empty-stream))
    f-stream))
  (make-promise-frame (lambda (frame) (lisp-value-inner call frame)) frame-stream))

; but the there are problems that if "or" inside "and" the not still not normal, because the not would be evluated inside "or"
(setup-data-base)
(inqu
 '(

   (and (supervisor ?x ?y)
        (not (job ?x (computer programmer))))

   (and (not (job ?x (computer programmer)))
        (supervisor ?x ?y))
   
   (and (salary ?x ?y)
        (lisp-value = ?y 60000))

   (and (not (not (lisp-value = ?y 60000)))
        (salary ?x ?y))

   (and (supervisor ?x ?y)
        (or (not (job ?x (computer programmer)))
            (not (job ?x (computer wizard)))))
   (and 
        (or (not (job ?x (computer programmer)))
            (not (job ?x (computer wizard))))
        (supervisor ?x ?y))
 ))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'done


; (and (supervisor (Aull DeWitt) (Warbucks Oliver)) (not (job (Aull DeWitt) (computer programmer))))
; (and (supervisor (Cratchet Robert) (Scrooge Eben)) (not (job (Cratchet Robert) (computer programmer))))
; (and (supervisor (Scrooge Eben) (Warbucks Oliver)) (not (job (Scrooge Eben) (computer programmer))))
; (and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (not (job (Bitdiddle Ben) (computer programmer))))
; (and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (not (job (Reasoner Louis) (computer programmer))))
; (and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (not (job (Tweakit Lem E) (computer programmer))))


; (and (not (job (Aull DeWitt) (computer programmer))) (supervisor (Aull DeWitt) (Warbucks Oliver)))
; (and (not (job (Cratchet Robert) (computer programmer))) (supervisor (Cratchet Robert) (Scrooge Eben)))
; (and (not (job (Scrooge Eben) (computer programmer))) (supervisor (Scrooge Eben) (Warbucks Oliver)))
; (and (not (job (Bitdiddle Ben) (computer programmer))) (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
; (and (not (job (Reasoner Louis) (computer programmer))) (supervisor (Reasoner Louis) (Hacker Alyssa P)))
; (and (not (job (Tweakit Lem E) (computer programmer))) (supervisor (Tweakit Lem E) (Bitdiddle Ben)))


; (and (salary (Bitdiddle Ben) 60000) (lisp-value = 60000 60000))


; (and (not (not (lisp-value = 60000 60000))) (salary (Bitdiddle Ben) 60000))


; (and (supervisor (Aull DeWitt) (Warbucks Oliver)) (or (not (job (Aull DeWitt) (computer programmer))) (not (job (Aull DeWitt) (computer wizard)))))
; (and (supervisor (Aull DeWitt) (Warbucks Oliver)) (or (not (job (Aull DeWitt) (computer programmer))) (not (job (Aull DeWitt) (computer wizard)))))
; (and (supervisor (Cratchet Robert) (Scrooge Eben)) (or (not (job (Cratchet Robert) (computer programmer))) (not (job (Cratchet Robert) (computer wizard)))))
; (and (supervisor (Cratchet Robert) (Scrooge Eben)) (or (not (job (Cratchet Robert) (computer programmer))) (not (job (Cratchet Robert) (computer wizard)))))
; (and (supervisor (Scrooge Eben) (Warbucks Oliver)) (or (not (job (Scrooge Eben) (computer programmer))) (not (job (Scrooge Eben) (computer wizard)))))
; (and (supervisor (Scrooge Eben) (Warbucks Oliver)) (or (not (job (Scrooge Eben) (computer programmer))) (not (job (Scrooge Eben) (computer wizard)))))
; (and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (or (not (job (Bitdiddle Ben) (computer programmer))) (not (job (Bitdiddle Ben) (computer wizard)))))
; (and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (or (not (job (Reasoner Louis) (computer programmer))) (not (job (Reasoner Louis) (computer wizard)))))
; (and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (or (not (job (Reasoner Louis) (computer programmer))) (not (job (Reasoner Louis) (computer wizard)))))
; (and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (or (not (job (Tweakit Lem E) (computer programmer))) (not (job (Tweakit Lem E) (computer wizard)))))
; (and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (or (not (job (Tweakit Lem E) (computer programmer))) (not (job (Tweakit Lem E) (computer wizard)))))
; (and (supervisor (Fect Cy D) (Bitdiddle Ben)) (or (not (job (Fect Cy D) (computer programmer))) (not (job (Fect Cy D) (computer wizard)))))
; (and (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) (or (not (job (Hacker Alyssa P) (computer programmer))) (not (job (Hacker Alyssa P) (computer wizard)))))


; > 