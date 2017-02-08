; Exercise 4.76: Our implementation of and as a series combination of queries (Figure 4.5) is elegant, but it is inefficient because in processing the second query of the and we must scan the data base for each frame produced by the first query. If the data base has nn elements, and a typical query produces a number of output frames proportional to nn (say n/kn/k), then scanning the data base for each frame produced by the first query will require n2/kn2/k calls to the pattern matcher. Another approach would be to process the two clauses of the and separately, then look for all pairs of output frames that are compatible. If each query produces n/kn/k output frames, then this means that we must perform n2/k2n2/k2 compatibility checks—a factor of kk fewer than the number of matches required in our current method.

; Devise an implementation of and that uses this strategy. You must implement a procedure that takes two frames as inputs, checks whether the bindings in the frames are compatible, and, if so, produces a frame that merges the two sets of bindings. This operation is similar to unification.
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin-frame
        (eval (first-conjunct conjuncts) frame-stream)
        (conjoin (rest-conjuncts conjuncts) frame-stream))))

(define (conjoin-frame fs1 fs2)
  (stream-flatmap
    (lambda (f1) 
            (stream-map
              (lambda (f2)
                      (conjoin-match f1 f2))
              fs2))
    fs1))

(define (conjoin-match f1 f2)
  (define (match? var val)
    (let ((binding (binding-in-frame var f2)))
      (if binding
          (eq? val (binding-value binding)))
          true))
  (define (all-match? frame)
    (let ((binding (car frame))
          (rest-bindings (cdr frame))
          (pred (match? 
                  (binding-variable binding) 
                  (binding-val binding))))
          (if (and (null? rest-bindings) pred)
              true
              (and
                pred
                (all-match? rest-bindings)))))
  (define (merge frame1 frame2)
    (if (null? frame1) 
        frame2
        (let (var (binding-variable (car frame1)))
          (if (binding-value (car frame1)) 
              (merge frame2 (cdr frame1))
              (cons (car )))
        )
  ))
  (cond 
        ((null? f1) f2)
        ((null? f2) f1)
        ((all-match? f1) (merge-frame f1 f2))
        (else the-empty-stream)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1)
         (extend-if-possible p1 p2 frame))
        ((var? p2)
         (extend-if-possible 
          p2 
          p1 
          frame))        ; ***
        ((and (pair? p1) 
              (pair? p2))
         (unify-match 
          (cdr p1) 
          (cdr p2)
          (unify-match 
           (car p1)
           (car p2)
           frame)))
        (else 'failed)))

(define (stream-accumulate op initial stream)
  (if (stream-null? stream)
      initial
      (op (stream-car stream)
          (accumulate op 
                      initial 
                      (stream-cdr stream)))))


(define (stream-flatmap proc stream)
  (stream-accumulate stream-append the-empty-stream (stream-map proc stream)))

(initialize-data-base microshaft-data-base)

(inqu
'(

)
)