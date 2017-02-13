; Exercise 4.79: When we implemented the Lisp evaluator in 4.1, we saw how to use local environments to avoid name conflicts between the parameters of procedures. For example, in evaluating

; (define (square x) 
;   (* x x))

; (define (sum-of-squares x y)
;   (+ (square x) (square y)))

; (sum-of-squares 3 4)
; there is no confusion between the x in square and the x in sum-of-squares, because we evaluate the body of each procedure in an environment that is specially constructed to contain bindings for the local variables. In the query system, we used a different strategy to avoid name conflicts in applying rules. Each time we apply a rule we rename the variables with new names that are guaranteed to be unique. The analogous strategy for the Lisp evaluator would be to do away with local environments and simply rename the variables in the body of a procedure each time we apply the procedure.

; Implement for the query language a rule-application method that uses environments rather than renaming. See if you can build on your environment structure to create constructs in the query language for dealing with large systems, such as the rule analog of block-structured procedures. Can you relate any of this to the problem of making deductions in a context (e.g., “If I supposed that PP were true, then I would be able to deduce AA and BB.”) as a method of problem solving? (This problem is open-ended. A good answer is probably worth a Ph.D.)
(load "/Users/soulomoon/git/SICP/Chapter4/ch4-query.rkt")
;;;SECTION 4.4.4.8
;;;Frames and bindings
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))


; unification work both ways, assign val in rule to var frame, assign val in frame to var in rule,
; and assign var in frame to var in rule

; here I tempt it with an extra layer in between two the origin frame and the frame in rule
; since the extra frame have to be contain somewhere and I don't have want to change the to let a definition to accept two frame or more , so I define a outer frame to be the parent frame

(define (outer frame)
  (caar frame))

(define (layer frame)
  (car frame))

(define (frame-body frame)
  (cadr frame))

; layer is very similar to frame.
; there is something special about layer, because it is where both frame is sharing variables
; when reducing frame, the vale in rule frame pass back to its parent through this layer.
; add binding to layer means combine one from parent fram and one from rule frame.
(define (extend-layer p1 p2 frame)
  (let ((layer-frame (layer frame)))
    (cons
      (extend p1 p2 layer-frame)
      (list (frame-body frame)))))
; have to define something like assoc except the key is cdr

(define (cdr-assoc var S)
  (if (null? S)
      false
      (if (equal? (binding-value (car S)) var)
          (car S)
          (cdr-assoc (cdr S)))))

(define (binding-out-layer variable frame)
  (cdr-assoc variable (frame-body (layer frame))))

(define (binding-in-layer variable frame)
  (assoc variable (frame-body (layer frame))))

; to search in the current frame
(define (binding-in-frame variable frame)
  (assoc variable (frame-body frame)))

; recursive tracing to var, bug if looping
(define (recursive-trace variable frame)
  (let ((binding (binding-in-frame variable frame)))
       (if binding
           (recursive-trace (binding-value binding) frame)
           variable)))

(define (binding-in-parent-frame variable frame)
  (binding-in-frame variable (outer frame)))

(define (top-frame? frame)
        (eq? (layer frame) 'global))

(define (extend variable value frame)
  (cons (layer frame)
        (list (cons (make-binding variable value) (frame-body frame)))))

; method to init frame and adding layer
(define (init-frame)
  (list 'global '()))

(define (add-layer frame)
  (list frame '()))

(define (add-frame frame)
  (list (add-layer frame) '()))

(define frame (init-frame))
; frame

(set! frame (extend 'variable 'value frame))
; (binding-in-frame 'variable frame)
(display frame)
(newline )

(set! frame (extend 'variable1 'value1 (add-frame frame)))
(set! frame (extend 'variable2 'value2 frame))

(set! frame (extend-layer 'layer1 'lval frame))
(display frame)
(newline )
(display (binding-out-layer 'lval frame))
(newline )
(display (binding-in-parent-frame 'variable frame))

; if impelementing a rule-application method , means that it is one way binding, that build out another layer of the frame, and apply the vars to rule, the (conclusion clean-rule) part have to be vars, the query part is value
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    ; (newline)
    ; (display "query-content:")
    ; (display (contents query))
    ; (newline)
    ; (display "query-frame-stream::")
    ; (display frame-stream)
    ; (newline)
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    ; (newline )
    ; (display "assertion:")
    ; (display query-pat)
    ; (newline )    
    ; (display "assertion-flame:")
    ; (display match-result)(newline )
  
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (conjoin conjuncts frame-stream)
  ; (newline )
  ; (display "conjoin:")
  ; (display conjuncts)
  ; (newline )
  ; (display "frame-stream:")
  ; (display frame-stream)
  ; (newline )
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
    ;  (newline )
    ;  (display "negate-frame-pre:")
    ;  (display frame)
    ;  (newline )
    ;  (display "negate-frame:")
    ;  (display (qeval (negated-query operands)
    ;                           (singleton-stream frame)))
    ;  (display "--negaite end" )
    ;  (newline )
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
; helper function to extend the frame with layer with rule
(define (extend-rule-frame p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ; count down to lowest level to avoid confict of ?x ?x
        ((eq? p1 p2) frame)
        ; if either is var, trace and extend to layer.
        ((or (var? p1) (var? p1)) 
          (extend-if-possible (recursive-trace p1 (outer frame)) (recursive-trace p2 frame) frame))
        ((and (pair? p1) (pair? p2))
         (extend-rule-frame (cdr p1)
                            (cdr p2)
                            (extend-rule-frame (car p1)
                                               (car p2)
                                               frame)))
        (else 'failed)))

(define (extend-layer-if-possible var val frame)
  (let ((binding (binding-in-parent-frame var frame)))
    (cond (binding
           (extend-rule-frame
            (binding-value binding) val frame))
          ((var? val)                     ; {\em ; ***}
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (extend-rule-frame
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; {\em ; ***}
           'failed)
          (else (extend var val frame)))))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((extend-frame (extend-rule-frame query-pattern (conclusion rule) (add-frame query-frame))))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion rule)
                        extend-frame)))
      ; (newline )
      ; (display "rule-flame-pre:")
      ; (display query-frame)
      ; (newline )
      ; (display "rule:")
      ; (display rule)
      ; (newline )
      ; (display "rule-pattern:")
      ; (display query-pattern)
      ; (newline )
      ; (display "rule-flame:")
      ; (display unify-result)
      ; (newline )
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body rule)
                 (singleton-stream unify-result))))))

; cut the binding val as var, because it have to be value
(define (unify-match val var frame)
  ; (newline )
  ; (display "val:")
  ; (display val)
  ; (display " var:")
  ; (display var)
  ; (newline )
  (cond ((eq? frame 'failed) 'failed)
        ((equal? val var) frame)
        ; ((var? val) (extend-if-possible val var frame))
        ((var? var) (extend-if-possible var val frame)) ; {\em ; ***}
        ((and (pair? val) (pair? var))
         (unify-match (cdr val)
                      (cdr var)
                      (unify-match (car val)
                                   (car var)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  ; (newline )
  ; (display "extend-if-possible")
  ; (newline )
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                   ; ***
           (let ((binding 
                  (binding-in-frame 
                   val
                   frame)))
             (if binding
                 (unify-match
                  var 
                  (binding-value binding) 
                  frame)
                 (extend var val frame))))
          ((depends-on? val var frame)  ; ***
            ; (newline )
            ; (display "depend")
            ; (newline )
           'failed)
          (else (extend var val frame)))))
          
; (init-frame) as global
(define (iq query)
  (let ((q (query-syntax-process query)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base."))
          (else
           (newline)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream (init-frame)))))))))

(setup-data-base)

(inqu
 '(
      (same (Reasoner Louis) ?someone)
  ;  (and
      (lives-near ?someone ?ad)
      ; (job ?someone ?y)
    ))