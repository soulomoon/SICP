; Exercise 3.37: The celsius-fahrenheit-converter procedure is cumbersome when compared with a more expression-oriented style of definition, such as

; (define (celsius-fahrenheit-converter x)
;   (c+ (c* (c/ (cv 9) (cv 5))
;           x)
;       (cv 32)))

; (define C (make-connector))
; (define F (celsius-fahrenheit-converter C))
; Here c+, c*, etc. are the “constraint” versions of the arithmetic operations. For example, c+ takes two connectors as arguments and returns a connector that is related to these by an adder constraint:

; (define (c+ x y)
;   (let ((z (make-connector)))
;     (adder x y z)
;     z))
; Define analogous procedures c-, c*, c/, and cv (constant value) that enable us to define compound constraints as in the converter example above.161
(load "/home/soulomoon/git/SICP/Chapter3/constraint.scm")

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv x)
  (let ((a (make-connector)))
    (constant x a)
  a))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe 'C C)
(probe 'F F)

(set-value! C 17.0 'user)
(forget-value! C 'user)
(set-value! F 17.0 'user)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; #<procedure:...3/constraint.scm:102:2>
; #<procedure:...3/constraint.scm:102:2>

; Probe: F = 62.6
; Probe: C = 17.0'done

; Probe: F = ?
; Probe: C = ?'done

; Probe: C = -8.333333333333334
; Probe: F = 17.0'done
; > 