;Exercise 5.21: Implement register machines for the following procedures. Assume that the list-structure memory operations are available as machine primitives.
;
;Recursive count-leaves:
;(define (count-leaves tree)
;  (cond ((null? tree) 0)
;        ((not (pair? tree)) 1)
;        (else
;         (+ (count-leaves (car tree))
;            (count-leaves (cdr tree))))))
;Recursive count-leaves with explicit counter:
;(define (count-leaves tree)
;  (define (count-iter tree n)
;    (cond ((null? tree) n)
;          ((not (pair? tree)) (+ n 1))
;          (else
;           (count-iter
;            (cdr tree)
;            (count-iter (car tree)
;                        n)))))
;  (count-iter tree 0))
(load "/Users/soulomoon/git/SICP/Chapter5/ch5-regsim.scm")

(define count-leaves-machine1
(make-machine
  '(val temp tree continue)
  (list
    (list 'cdr cdr)
    (list 'car car)
    (list 'pair? pair?)
    (list 'print print)
    (list '+ +)
    (list 'null? null?)
    (list 'not not))
  '(
    (assign continue (label count-done))
    (save continue)
  count-begin
    ;test wether it is a case of bases
    (test (op null?) (reg tree))
    (branch (label answer0))
    (assign temp (op pair?) (reg tree))
    (test (op not) (reg temp))
    (branch (label answer1))

    ;prepare for count-car
    (save tree)
    (assign tree (op car) (reg tree))
    (assign continue (label count-car))
    (save continue)
    (goto (label count-begin))

  count-car
    ;restore the car tree to get the its sub cdr tree
    (restore tree)
    ;save n for count-car
    (save val)
    ;prepare for count-cdr
    (assign tree (op cdr) (reg tree))
    (assign continue (label count-cdr))
    (save continue)
    (goto (label count-begin))

  count-cdr
    ;now n store the val of cdr
    (assign temp (reg val))

    ;get count-car value
    (restore val)

    ;adding car-val and cdr-val
    ;now val is the temp result
    (assign val (op +) (reg val) (reg temp))
    (restore continue)
    (goto (reg continue))


  answer0
    (restore continue)
    (assign val (const 0))
    (goto (reg continue))
  answer1
    (restore continue)
    (assign val (const 1))
    (goto (reg continue))
  count-done
    (perform (op print) (reg val))
)))

(define count-leaves-machine2
(make-machine
  '(temp val n tree continue)
  (list
    (list 'cdr cdr)
    (list 'car car)
    (list 'pair? pair?)
    (list 'print print)
    (list '+ +)
    (list 'null? null?)
    (list 'not not))
  '(
    (assign n (const 0))
    ;end-point with tree
    (save tree)
    (assign continue (label count-done))
    (save continue)
  count-iter
    (test (op null?) (reg tree))
    (branch (label answer0))
    (assign temp (op pair?) (reg tree))
    (test (op not) (reg temp))
    (branch (label answer1))
    ;split the trees, save the cdr-tree and set tree to car
    (assign temp (op car) (reg tree))
    (assign tree (op cdr) (reg tree))
    (save tree)
    (assign tree (reg temp))
    ;leave label count-iter
    (assign continue (label count-iter))
    (save continue)
    (goto (label count-iter))
  answer0
    (restore continue)
    (restore tree)
    (goto (reg continue))
  answer1
    (restore continue)
    (restore tree)
    (assign n (op +) (reg n) (const 1))
    (goto (reg continue))
  count-done
    (perform (op print) (reg n))
)))
(define tree '((1 2) (1 2 3) 4 5))
(set-register-contents! count-leaves-machine1 'tree tree)
(start count-leaves-machine1)
(set-register-contents! count-leaves-machine2 'tree tree)
(start count-leaves-machine2)
;Welcome to DrRacket, version 6.8 [3m].
;Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;(REGISTER SIMULATOR LOADED)
;'done
;
;7
;'done
;'done
;
;7
;'done
;>
