; Exercise 4.71: Louis Reasoner wonders why the simple-query and disjoin procedures (4.4.4.2) are implemented using explicit delay operations, rather than being defined as follows:

```scheme
(define (simple-query 
         query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append
      (find-assertions query-pattern frame)
      (apply-rules query-pattern frame)))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts)
              frame-stream)
       (disjoin (rest-disjuncts disjuncts)
                frame-stream))))
```

; Can you give examples of queries where these simpler definitions would lead to undesirable behavior?


in the case where a rule may lead to infinit loop, and there is also a assertions,
that you can at least display the assertions that match, and then went crazy while the first rule went crazy.

```scheme
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1)
                              delayed-s2))))
```
which you can see here, cons-stream does not force the
```scheme
       (stream-append-delayed (stream-cdr s1)
                              delayed-s2))))
```
until it is force by the the program,
going through simple-query, qeval, instantiate does not force it.
only going to the `display-stream` it was forced and display.

the same goes for `interleave-delayed`

