
; Exercise 3.20: Draw environment diagrams to illustrate the evaluation of the sequence of expressions

; (define x (cons 1 2))
; (define z (cons x x))

; (set-car! (cdr z) 17)

; (car x)
; 17
; using the procedural implementation of pairs given above. (Compare Exercise 3.11.)
              +---------------------------------------------------------------------+
              |                                                                     |
global env--->|cons                                        set-car!                 |
	      | |                     			      |                     |    
              +-|---------------------------------------------|---------------------+
                v    ^                                        v   ^
             [*] [*]-+                                     [*] [*]+
              v                                             v 
       parameters: x y                                    parameters: z new-value
       body:                                           body: ((z 'set-car!) new-value)
	  (define (set-x! v) (set! x v))                     z) 
	  (define (set-y! v) (set! y v))
	  (define (dispatch m)
	    (cond ((eq? m 'car) x)
		  ((eq? m 'cdr) y)
		  ((eq? m 'set-car!) set-x!)
		  ((eq? m 'set-cdr!) set-y!)
		  (else (error "Undefined 
			 operation: CONS" m))))
	  dispatch)                  
              +-----------------------------------------------------------+
              |                                                           |
global env--->| x                                        z                |
	            | |                                        |                | 
              +-|----------------------------------------|----------------+
                |      ^                                 |   
		|  +--------+                            |  +-------+
		|  | x:1    |                            |  | x---------------------------+
		|  | y:2    |                            |  | y--------------------------+|
		|e>|set-x!  |                            |  |set-x! |                    ||
		|  |set-y!  |<------------+              |  |set-y! |<------------+      ||
		+-->dispatch---------[*] [*]             +-->dispatch--------[*] [*]     ||
		   |  |  |  |         |                     |       |         |          ||
		   +--|--|--+         v                     +-------+         v          ||
                      |  |                                                  ....         ||
                      |  |        parameters:m                                           ||
                      |  +---------------------------------------------------------------+|
                      +-------------------------------------------------------------------+

 
               +----------------------------------------------------------+
              |                                                           |
global env--->|                                                           |
	      |                                                           | 
              +-----------------------------------------------------------+
         (set-car! (cdr z) 17) ^                      
                         +----------+<----------+-----------+
                         |z-------------------->| z:z       |
                         |new: 17   |  (cdr z)  |           |
                         |          |           +-----------+
                         |          |            (z 'car)
                         |          |
                         +----------+
                   




