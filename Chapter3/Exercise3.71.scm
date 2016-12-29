; Exercise 3.71: Numbers that can be expressed as the sum of two cubes in more than one way are sometimes called Ramanujan numbers, in honor of the mathematician Srinivasa Ramanujan.198 Ordered streams of pairs provide an elegant solution to the problem of computing these numbers. To find a number that can be written as the sum of two cubes in two different ways, we need only generate the stream of pairs of integers (i,j)(i,j) weighted according to the sum i3+j3i3+j3 (see Exercise 3.70), then search the stream for two consecutive pairs with the same weight. Write a procedure to generate the Ramanujan numbers. The first such number is 1,729. What are the next five?

(load "/home/soulomoon/git/SICP/Chapter3/Exercise3.70.scm")

(define (Ramanujan_weight pair)
  (+ (expt (car pair) 3) (expt (cadr pair) 3)))

(define Ramanujan_weighted_stream (weighted-pairs integers integers Ramanujan_weight))

(define (Ramanujan_iter S)
  (let ((first (Ramanujan_weight (stream-car S)))
        (rest (stream-cdr S)))
        (let ((second (Ramanujan_weight (stream-car rest))))
          (if (= first second)
              (cons-stream
                first
                (Ramanujan_iter rest))
              (Ramanujan_iter rest)))))

(define Ramanujan_numbers (Ramanujan_iter Ramanujan_weighted_stream))

; (display-10 Ramanujan_numbers)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 1729
; 4104
; 13832
; 20683
; 32832
; 39312
; 40033
; 46683
; 64232
; 65728
; 110656'done
; > 