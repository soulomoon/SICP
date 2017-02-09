; Exercise 4.76: Our implementation of and as a series combination of queries (Figure 4.5) is elegant, but it is inefficient because in processing the second query of the and we must scan the data base for each frame produced by the first query. If the data base has nn elements, and a typical query produces a number of output frames proportional to nn (say n/kn/k), then scanning the data base for each frame produced by the first query will require n2/kn2/k calls to the pattern matcher. Another approach would be to process the two clauses of the and separately, then look for all pairs of output frames that are compatible. If each query produces n/kn/k output frames, then this means that we must perform n2/k2n2/k2 compatibility checks—a factor of kk fewer than the number of matches required in our current method.

; Devise an implementation of and that uses this strategy. You must implement a procedure that takes two frames as inputs, checks whether the bindings in the frames are compatible, and, if so, produces a frame that merges the two sets of bindings. This operation is similar to unification.
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")
(define (iq query)
  (let ((q (query-syntax-process query)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base."))
          (else
           ;  (newline)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()))))))))
(setup-data-base)
(define (display-stream s)
  (= 1 1))

(define (try-wraper f n)
  (lambda vars 
    (define (loop n)
      (if (= 0 n)
          'done
          (begin
            (apply f vars)
            (loop (- n 1))
            )))
    (loop n)))
(define inqut
  (lambda vars (time (apply (try-wraper inqu 10000) vars))))



(collect-garbage)
(inqut
 '(
   (and (job ?x ?y)
        (address ?x ?ad)
        )))



(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))



(define (conjoin conjuncts frame-stream)
  (define (innner conjuncts frame-stream)
    (if (empty-conjunction? conjuncts)
        frame-stream
        (conjoin-frame
         (qeval (first-conjunct conjuncts) frame-stream)
         (delay (innner (rest-conjuncts conjuncts) frame-stream)))))
  ; (display "nie")
  (stream-filter (lambda (s) (not (null? s))) 
                 (innner conjuncts frame-stream)))

(define (conjoin-frame fs1 fs2)
  (stream-flatmap
   (lambda (f1) 
     (stream-map
      (lambda (f2)
        (conjoin-match f1 f2))
      (force fs2)))
   fs1))

(define (conjoin-match f1 f2)
  (cond ((null? f1) f2)
        ((null? f2) f1)
        ((conjoinable? f1 f2) (merge-frame f1 f2))
        (else the-empty-stream)))

(define (merge-frame f1 f2)
  (let ((first (car f1))
        (rest (cdr f1)))
    (let ((var1 (binding-variable first))
          (val1 (binding-value first)))
      (let ((b2 (binding-in-frame var1 f2)))
        (if (null? rest)
            (if b2
                (merge-frame rest f2)
                (merge-frame rest (cons first f2)))
            (cons first f2))))))

(define (conjoinable? f1 f2)
  (accumulate 
   (lambda (x y) (and x y))
   nil
   (conjoinable-helper f1 f2)))

(define (conjoinable-helper f1 f2)
  (flatmap
   (lambda (b1) 
     (map
      (lambda (b2)
        (binding-match? b1 b2))
      f2))
   f1))

(define (binding-match? b1 b2)
  (let ((var1 (binding-variable b1))
        (var2 (binding-variable b2))
        (val1 (binding-value b1))
        (val2 (binding-value b2)))
    (if (same-var? var1 var2)
        (same-var? val1 val2)
        true)))
            
(define (same-var? var1 var2)
  (cond ((and (null? var1) (null? var2)) true)
        ((and (pair? var1) (pair? var2)) 
         (and (same-var? (car var1) (car var2))
              (same-var? (cdr var1) (cdr var2))))
        ((and (symbol? var1) (symbol? var2))
         (eq? var1 var2))
        (else false)))

(collect-garbage)
(inqut
 '(
   (and (job ?x ?y)
        (address ?x ?ad)
        )))

; but there is problem working with not or lisp-value
(collect-garbage)
(inqut
 '(
   (and (job ?someone ?y)
        (not (supervisor ?someone ?ad))
        )))
; it does not print out and value. because it just merge a empty-stream to the first stream, 
; the result is then an empty-stream


; we could see that the second one is running faster

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; 'done
; cpu time: 674 real time: 693 gc time: 443
; 'done
; cpu time: 279 real time: 280 gc time: 5
; 'done
; > 