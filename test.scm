(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible-frame p1 p2 frame))
        ((var? p2) (extend-if-possible-layer p1 p2 frame)) ; {\em ; ***}
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible-frame var val frame)
  (let ((binding (binding-in-frame var frame))
        (bindlayer (binding-up-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          (bindlayer
            )
          ((var? val)                     ; {\em ; ***}
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; {\em ; ***}
           'failed)
          (else (extend var val frame)))))

(define (extend-if-possible-layer var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                     ; {\em ; ***}
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; {\em ; ***}
           'failed)
          (else (extend var val frame)))))

(define (depends-on-frame? exp var frame)
  (define (tree-walk e n)
      (cond ((and (var? e) (= n 0))
              (if (equal? var e)
                  true
                  (let ((b (binding-in-frame e frame))
                        (by (binding-up-frame e frame)))
                        (cond (and b by)
                              (or (tree-walk (binding-value b) 1)
                                  (tree-walk (car by) 0)))
                              (b
                                (tree-walk (binding-value b) 1))
                              (by
                                (tree-walk (car by) 0))
                              (else false))))
            ((var? e)
              (let ((binding-layer (binding-down-layer e frame)))
                      (cond ((binding-layer) 
                              (tree-walk (car binding-layer) 1))
                            (else false))))
            ((pair? e)
              (or (tree-walk (car e) n)
                  (tree-walk (cdr e) n)))
            (else false)))
  (tree-walk exp 0))

(define (depends-on-layer? exp var frame)
  (define (tree-walk e n)
      (cond 
            ((and (var? e) (= n 1))
              (if (equal? var e)
                  true
                  (let ((binding-layer (binding-down-layer e frame)))
                          (cond ((binding-layer) 
                                  (tree-walk (car binding-layer) 1))
                                (else false))))
                                  ))
            ((var? e)
                  (let ((b (binding-in-frame e frame))
                            (by (binding-up-frame e frame)))
                            (cond (and b by)
                                  (or (tree-walk (binding-value b) 1)
                                      (tree-walk (car by) 0)))
                                  (b
                                    (tree-walk (binding-value b) 1))
                                  (by
                                    (tree-walk (car by) 0))
                                  (else false))
            ((pair? e)
              (or (tree-walk (car e) n)
                  (tree-walk (cdr e) n)))
            (else false)))
  (tree-walk exp 1))