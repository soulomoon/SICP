; Exercise 4.70: What is the purpose of the let bindings in the procedures add-assertion! and add-rule!? What would be wrong with the following implementation of add-assertion!? Hint: Recall the definition of the infinite stream of ones in 3.5.2: (define ones (cons-stream 1 ones)).
```scheme
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion 
                     THE-ASSERTIONS))
  'ok)
```
for cons-stream, the later var is delayed

for ones definition, it would loopingly interpret ones, when cdring the stream list

same is true for under
```scheme
  (set! THE-ASSERTIONS
        (cons-stream assertion 
                     THE-ASSERTIONS))
```
but there is a defferent case when you stored it in a new var

```scheme
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion 
                       old-assertions))
    'ok))
```
    
the  `(let ((old-assertions THE-ASSERTIONS))` here 
let `old-assertions` take the value of `THE-ASSERTIONS`.
would strip out the outer layer which is a pointer

same here:
```scheme
(define a 1)
(define b a)
(set! a 2)
b
```
as you can see, b would change with a even if a new value has
been assign to a.