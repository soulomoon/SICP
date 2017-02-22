;Exercise 5.8: The following register-machine code is ambiguous, because the label here is defined more than once:
;
;start
;  (goto (label here))
;here
;  (assign a (const 3))
;  (goto (label there))
;here
;  (assign a (const 4))
;  (goto (label there))
;there
;With the simulator as written, what will the contents of register a be when control reaches there? Modify the extract-labels procedure so that the assembler will signal an error if the same label name is used to indicate two different locations.
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")
(define (print x)
  (newline )
  (display x)
  (newline ))

(define go-machine
(make-machine
  '(a)
  (list (list 'print print))
  '(
    start
      (goto (label here))
    here
      (assign a (const 3))
      (goto (label there))
    here
      (assign a (const 4))
      (goto (label there))
    there
    (perform (op print) (reg a))
  )
  )
)

(start go-machine)

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (begin
                  (if (assoc next-inst labels)
                      (error "repeated label: " "" next-inst "" )
                      (receive
                         insts
                         (cons
                          (make-label-entry
                           next-inst
                           insts)
                          labels))))
               (receive
                   (cons (make-instruction
                          next-inst)
                         insts)
                   labels)))))))

(define go-machine
(make-machine
 '(a)
 (list (list 'print print))
 '(
   start
     (goto (label here))
   here
     (assign a (const 3))
     (goto (label there))
   here
     (assign a (const 4))
     (goto (label there))
   there
   (perform (op print) (reg a))
 )
 )
)
(start go-machine)

;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;
;3
;'done
;. . repeated label:  "" here ""
;>
