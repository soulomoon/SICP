; Exercise 2.24: Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree (as in Figure 2.6).
(list 1 (list 2 (list 3 4)))



Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
{mcons 1 {mcons {mcons 2 {mcons {mcons 3 {mcons 4 '()}} '()}} '()}}
> 