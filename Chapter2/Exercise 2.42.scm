; Exercise 2.42: The “eight_queens puzzle” asks how to place eight queens on a chessboard so that no queen is in check from any other (i.e., no two queens are in the same row, column, or diagonal). One possible solution is shown in Figure 2.8. One way to solve the puzzle is to work across the board, placing a queen in each column. Once we have placed k−1k−1 queens, we must place the kthkth queen in a position where it does not check any of the queens already on the board. We can formulate this approach recursively: Assume that we have already generated the sequence of all possible ways to place k−1k−1 queens in the first k−1k−1 columns of the board. For each of these ways, generate an extended set of positions by placing a queen in each row of the kthkth column. Now filter these, keeping only the positions for which the queen in the kthkth column is safe with respect to the other queens. This produces the sequence of all ways to place kk queens in the first kk columns. By continuing this process, we will produce not only one solution, but all solutions to the puzzle.

 
; Figure 2.8: A solution to the eight_queens puzzle.
; We implement this solution as a procedure queens, which returns a sequence of all solutions to the problem of placing nn queens on an n×nn×n chessboard. Queens has an internal procedure queen_cols that returns the sequence of all ways to place queens in the first kk columns of the board.

; (define (queens board_size)
;   (define (queen_cols k)
;     (if (= k 0)
;         (list empty_board)
;         (filter
;          (lambda (positions) 
;            (safe? k positions))
;          (flatmap
;           (lambda (rest_of_queens)
;             (map (lambda (new_row)
;                    (adjoin_position 
;                     new_row 
;                     k 
;                     rest_of_queens))
;                  (enumerate_interval 
;                   1 
;                   board_size)))
;           (queen_cols (- k 1))))))
;   (queen_cols board_size))
; In this procedure rest_of_queens is a way to place k−1k−1 queens in the first k−1k−1 columns, and new_row is a proposed row in which to place the queen for the kthkth column. Complete the program by implementing the representation for sets of board positions, including the procedure adjoin_position, which adjoins a new row_column position to a set of positions, and empty_board, which represents an empty set of positions. You must also write the procedure safe?, which determines for a set of positions, whether the queen in the kthkth column is safe with respect to the others. (Note that we need only check whether the new queen is safe—the other queens are already guaranteed safe with respect to each other.)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (test term x y)
    (term x y)
)
(define (enumerate_interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate_interval 
             (+ low 1) 
             high))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))                      

(define (queens board_size)
  (define (queen_cols k)
    (if (= k 0)
        (list empty_board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest_of_queens)
            (map (lambda (new_row)
                   (adjoin_position 
                    new_row 
                    k 
                    rest_of_queens))
                 (enumerate_interval 
                  1 
                  board_size)))
          (queen_cols (- k 1))))))
  (queen_cols board_size))
(define (adjoin_position new_row k rest_of_queens) 
    (cons (list new_row k) rest_of_queens)
)
(define empty_board (list nil))


(define (safe? k positions)
    (= 
        (accumulate 
            +
            0
            (map 
                (lambda (one_queen_row)
                    (if (null? one_queen_row)
                        0
                        (if 
                            (= (car one_queen_row) (caar positions))
                            1
                            0
                        )
                    )
                )
                (cdr positions)
            ) 
        ) 
        0
    )
)
(display (accumulate + 0 (map (lambda (x) (newline) (display x) 1) (queens 2))))
(display (accumulate + 0 (map (lambda (x) (newline) (display x) 1) (queens 3))))
(display (accumulate + 0 (map (lambda (x) (newline) (display x) 1) (queens 4))))
Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.

((2 2) (1 1) ())
((1 2) (2 1) ())2
((3 3) (2 2) (1 1) ())
((2 3) (3 2) (1 1) ())
((3 3) (1 2) (2 1) ())
((1 3) (3 2) (2 1) ())
((2 3) (1 2) (3 1) ())
((1 3) (2 2) (3 1) ())6
((4 4) (3 3) (2 2) (1 1) ())
((3 4) (4 3) (2 2) (1 1) ())
((4 4) (2 3) (3 2) (1 1) ())
((2 4) (4 3) (3 2) (1 1) ())
((3 4) (2 3) (4 2) (1 1) ())
((2 4) (3 3) (4 2) (1 1) ())
((4 4) (3 3) (1 2) (2 1) ())
((3 4) (4 3) (1 2) (2 1) ())
((4 4) (1 3) (3 2) (2 1) ())
((1 4) (4 3) (3 2) (2 1) ())
((3 4) (1 3) (4 2) (2 1) ())
((1 4) (3 3) (4 2) (2 1) ())
((4 4) (2 3) (1 2) (3 1) ())
((2 4) (4 3) (1 2) (3 1) ())
((4 4) (1 3) (2 2) (3 1) ())
((1 4) (4 3) (2 2) (3 1) ())
((2 4) (1 3) (4 2) (3 1) ())
((1 4) (2 3) (4 2) (3 1) ())
((3 4) (2 3) (1 2) (4 1) ())
((2 4) (3 3) (1 2) (4 1) ())
((3 4) (1 3) (2 2) (4 1) ())
((1 4) (3 3) (2 2) (4 1) ())
((2 4) (1 3) (3 2) (4 1) ())
((1 4) (2 3) (3 2) (4 1) ())24
> 