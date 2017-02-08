; Exercise 4.73: Why does flatten-stream use delay explicitly? What would be wrong with defining it as follows:

; (define (flatten-stream stream)
;   (if (stream-null? stream)
;       the-empty-stream
;       (interleave (stream-car stream)
;                   (flatten-stream 
;                    (stream-cdr stream)))))

the same reason for 4.71