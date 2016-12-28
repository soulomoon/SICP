; Exercise 3.69: Write a procedure triples that takes three infinite streams, SS, TT, and UU, and produces the stream of triples (Si,Tj,Uk)(Si,Tj,Uk) such that i≤j≤ki≤j≤k. Use triples to generate the stream of all Pythagorean triples of positive integers, i.e., the triples (i,j,k)(i,j,k) such that i≤ji≤j and i2+j2=k2i2+j2=k2.
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples S T U)
  (cons-stream 
    (map stream-car (list S T U))
    (interleave
    (stream-map (lambda x)
    
    ) 
      (stream-map (lambda (x) 
                    (list (stream-car S T) x))
                  (stream-cdr T))
      (apply triples (map stream-cdr (list S T U)))
    )