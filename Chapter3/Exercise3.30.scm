; Exercise 3.30: Figure 3.27 shows a ripple-carry adder formed by stringing together nn full-adders. This is the simplest form of parallel adder for adding two nn-bit binary numbers. The inputs A1A1, A2A2, A3A3, …, AnAn and B1B1, B2B2, B3B3, …, BnBn are the two binary numbers to be added (each AkAk and BkBk is a 0 or a 1). The circuit generates S1S1, S2S2, S3S3, …, SnSn, the nn bits of the sum, and CC, the carry from the addition. Write a procedure ripple-carry-adder that generates this circuit. The procedure should take as arguments three lists of nn wires each—the AkAk, the BkBk, and the SkSk—and also another wire CC. The major drawback of the ripple-carry adder is the need to wait for the carry signals to propagate. What is the delay needed to obtain the complete output from an nn-bit ripple-carry adder, expressed in terms of the delays for and-gates, or-gates, and inverters?

(load "/home/soulomoon/git/SICP/Chapter3/Exercise3.29.scm")
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder alist blist)
  (define (iter alist blist c-in rlist)
    (if (null? alist)
      rlist
      (let ((a (car alist))
            (b (car blist))
            (rest_alist (cdr alist))
            (rest_blist (cdr blist))
            (c-out (make-wire))
            (sum (make-wire)))
          (full-adder a b c-in sum c-out)
          (iter rest_alist 
                rest_blist 
                c-out
                (cons sum rlist)))))
  (iter alist blist (make-wire) '()))

(define (result) (map get-signal rlist))
(define (input signals numbers) 
  (let ((rnumbers (reverse numbers)))
    (for-each (lambda (s n) (set-signal! s n)) signals rnumbers)))

(define a1 (make-wire))
(define a2 (make-wire))
(define a3 (make-wire))
(define a4 (make-wire))

(define b1 (make-wire))
(define b2 (make-wire))
(define b3 (make-wire))
(define b4 (make-wire))

(define alist (list a1 a2 a3 a4))
(define blist (list b1 b2 b3 b4))

(define rlist (ripple-carry-adder alist blist))

(define avalue '(0 1 0 1))
(define bvalue '(0 0 1 1))

(input alist '(0 0 0 1))
(input blist '(0 0 0 1))
(display (result))(newline )

(input alist avalue)
(input blist bvalue)
(display (result))(newline )


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 3.29
; (0 0 1 0)
; (1 0 0 0)
; > 