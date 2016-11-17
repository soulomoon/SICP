; Exercise 2.37: Suppose we represent vectors v = (vi)(vi) as sequences of numbers, and matrices m = (mij)(mij) as sequences of vectors (the rows of the matrix). For example, the matrix
; 146257368469
; (123445666789)
; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations to concisely express the basic matrix and vector operations. These operations (which are described in any book on matrix algebra) are the following:
; (dot_product v w)(matrix_*_vector m v)(matrix_*_matrix m n)(transpose m)returns the sumΣiviwi;returns the vectort,whereti=Σjmijvj;returns the matrixp,wherepij=Σkmiknkj;returns the matrixn,wherenij=mji.
; (dot_product v w)returns the sumΣiviwi;(matrix_*_vector m v)returns the vectort,whereti=Σjmijvj;(matrix_*_matrix m n)returns the matrixp,wherepij=Σkmiknkj;(transpose m)returns the matrixn,wherenij=mji.
; We can define the dot product as83

; (define (dot_product v w)
;   (accumulate + 0 (map * v w)))
; Fill in the missing expressions in the following procedures for computing the other matrix operations. (The procedure accumulate_n is defined in Exercise 2.36.)

; (define (matrix_*_vector m v)
;   (map ⟨??⟩ m))

; (define (transpose mat)
;   (accumulate_n ⟨??⟩ ⟨??⟩ mat))

; (define (matrix_*_matrix m n)
;   (let ((cols (transpose n)))
;     (map ⟨??⟩ m)))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate_n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate_n op init (map (lambda (seq) (cdr seq)) seqs)))))

(define (dot_product v w)
  (accumulate + 0 (map * v w)))

  (define (matrix_*_vector m v)
  (map (lambda (w) (dot_product v w)) m))

(define (transpose mat)
  (accumulate_n cons nil mat))

(define (matrix_*_matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (w) (matrix_*_vector cols w)) m)))

(define a (list 1 2 3))
(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(dot_product a a)
(display (matrix_*_vector x a))
(newline)
(display (transpose x))
(newline)
(display (matrix_*_matrix x x))

