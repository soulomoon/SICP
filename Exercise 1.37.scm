; Exercise 1.37:

; An infinite continued fraction is an expression of the form
; f=N1D1+N2D2+N3D3+….
; f=N1D1+N2D2+N3D3+….
; As an example, one can show that the infinite continued fraction expansion with the NiNi and the DiDi all equal to 1 produces 1/φ1/φ, where φφ is the golden ratio (described in 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation—a so-called finite continued fraction k-term finite continued fraction—has the form
; N1D1+N2⋱+NkDk.
; N1D1+N2⋱+NkDk.
; Suppose that n and d are procedures of one argument (the term index ii) that return the NiNi and DiDi of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the kk-term finite continued fraction. Check your procedure by approximating 1/φ1/φ using
; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)
; for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?

; If your cont-frac procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
#lang planet neil/sicp
(define (cont_frac n d k)
    (define (search_frac g)
        (cond
            ((= g k) (/ (n g) (d g)))
            (else (/ (n g) (+ (d g) (search_frac (+ 1 g)))))
        )
    )
    (search_frac 0)
)

(define k 10)
(cont_frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)

0.6180555555555556

(define (cont_frac_iter n d k)
    (define (search_frac g result)
        (let ((new_result (/ (n g) (+ (d g) result))))
            (cond
                ((= g 0) result)
                (else (search_frac (- g 1) new_result))
            )
        )
    )
    (search_frac k 0)
)

(define k 10)
(cont_frac_iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)
0.6179775280898876