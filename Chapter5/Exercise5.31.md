;Exercise 5.31: In evaluating a procedure application, the explicit-control evaluator always saves and restores the env register around the evaluation of the operator, saves and restores env around the evaluation of each operand (except the final one), saves and restores argl around the evaluation of each operand, and saves and restores proc around the evaluation of the operand sequence. For each of the following combinations, say which of these save and restore operations are superfluous and thus could be eliminated by the compiler’s preserving mechanism:
;
;(f 'x 'y)
;((f) 'x 'y)
;(f (g 'x) y)
;(f (g 'x) 'y)

explicit-control interpretor way would be:
operator:
```Scheme
save and restore env

operator(not the last):
save and restore env
save and restore argl
save and restore proc

operator(not the last):
save and restore argl
save and restore proc


(f 'x 'y) need:
none

((f) 'x 'y) need:
operator:
save and restore env

(f (g 'x) 'y) need:
operator(not the last):
save and restore env
save and restore argl
save and restore proc

(f (g 'x) 'y):
operator(not the last):
save and restore env
save and restore argl
save and restore proc
```
