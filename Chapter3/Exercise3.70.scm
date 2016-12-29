; Exercise 3.70: It would be nice to be able to generate streams in which the pairs appear in some useful order, rather than in the order that results from an ad hoc interleaving process. We can use a technique similar to the merge procedure of Exercise 3.56, if we define a way to say that one pair of integers is “less than” another. One way to do this is to define a “weighting function” W(i,j)W(i,j) and stipulate that (i1,j1)(i1,j1) is less than (i2,j2)(i2,j2) if W(i1,j1)<W(i2,j2)W(i1,j1)<W(i2,j2). Write a procedure merge-weighted that is like merge, except that merge-weighted takes an additional argument weight, which is a procedure that computes the weight of a pair, and is used to determine the order in which elements should appear in the resulting merged stream.197 Using this, generalize pairs to a procedure weighted-pairs that takes two streams, together with a procedure that computes a weighting function, and generates the stream of pairs, ordered according to weight. Use your procedure to generate

; the stream of all pairs of positive integers (i,j)(i,j) with i≤ji≤j ordered according to the sum i+ji+j,
; the stream of all pairs of positive integers (i,j)(i,j) with i≤ji≤j, where neither ii nor jj is divisible by 2, 3, or 5, and the pairs are ordered according to the sum 2i+3j+5ij2i+3j+5ij.

(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream 
                   s1car 
                   (merge-weighted (stream-cdr s1) 
                          s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream 
                   s2car 
                   (merge-weighted s1
                          (stream-cdr s2) weight)))
                 (else
                  (cons-stream 
                   s1car
                   (merge-weighted 
                    (stream-cdr s1)
                    s2
                    weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (sum_weight pair)
  (+ (car pair) (cadr pair)))

(define (235_weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
        (+ 
          (* 2 i) 
          (* 3 j) 
          (* 5 i j))))

(define ijintegers (weighted-pairs integers integers sum_weight))

(define (235_divided? n)
  (or 
    (= (remainder n 2) 0)
    (= (remainder n 3) 0)
    (= (remainder n 5) 0)))

(define 235integers 
  (stream-filter 
    (lambda (pair) 
              (or (235_divided? (car pair))
                  (235_divided? (cadr pair)))) 
                (weighted-pairs integers integers 235_weight)))

; (display-10 ijintegers)
; (1 1)
; (1 2)
; (1 3)
; (2 2)
; (1 4)
; (2 3)
; (1 5)
; (2 4)
; (3 3)
; (1 6)
; (2 5)'done
; > 

; (display-10 235integers)
; (1 2)
; (1 3)
; (2 2)
; (1 4)
; (1 5)
; (2 3)
; (1 6)
; (2 4)
; (3 3)
; (1 8)
; (2 5)'done
; > 