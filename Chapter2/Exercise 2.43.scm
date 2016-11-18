; Exercise 2.43: Louis Reasoner is having a terrible time doing Exercise 2.42. His queens procedure seems to work, but it runs extremely slowly. (Louis never does manage to wait long enough for it to solve even the 6×66×6 case.) When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order of the nested mappings in the flatmap, writing it as

; (flatmap
;  (lambda (new-row)
;    (map (lambda (rest-of-queens)
;           (adjoin-position 
;            new-row k rest-of-queens))
;         (queen-cols (- k 1))))
;  (enumerate-interval 1 board-size))
; Explain why this interchange makes the program run slowly. Estimate how long it will take Louis’s program to solve the eight-queens puzzle, assuming that the program in Exercise 2.42 solves the puzzle in time TT.

because you call each (queen-cols (- k 1)) board-size times,where is should be 1 time
thus, your bonus time would be board-size T time.