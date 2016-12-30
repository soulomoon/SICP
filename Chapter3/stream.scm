; (define (delay x) 
;   (lambda () x))

; (define (delay x) (lambda () x))

; (define (force delayed-object)
;   (delayed-object))

; (define (cons-stream a b)
;   (cons a (delay b)))
(define (square x) (* x x))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; (define (stream-map proc s)
;   (if (stream-null? s)
;       the-empty-stream
;       (cons-stream 
;        (proc (stream-car s))
;        (stream-map proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))


(define (add-streams s1 s2) 
  (stream-map + s1 s2))

  
(define (mul-streams s1 s2) 
  (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

  
(define ones (cons-stream 1 ones))

(define integers 
  (cons-stream 1 (add-streams ones integers)))



; (display-stream (stream-enumerate-interval 10 12))
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

(define (display-10 s)
  (display-stream 
    (stream-map (lambda (n) 
                  (stream-ref s n)) 
                    (stream-enumerate-interval 0 10))))



(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

  (define (list_to_stream l)
  (if (null? l)
    nil
    (cons-stream (car l)
      (list_to_stream (cdr l)))))