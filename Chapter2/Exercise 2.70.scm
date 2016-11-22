; Exercise 2.70: The following eight-symbol alphabet with associated relative frequencies was designed to efficiently encode the lyrics of 1950s rock songs. (Note that the “symbols” of an “alphabet” need not be individual letters.)


; A    2    NA  16
; BOOM 1    SHA  3
; GET  2    YIP  9
; JOB  2    WAH  1
; Use generate-huffman-tree (Exercise 2.69) to generate a corresponding Huffman tree, and use encode (Exercise 2.68) to encode the following message:

; Get a job
; Sha na na na na na na na na

; Get a job
; Sha na na na na na na na na

; Wah yip yip yip yip 
; yip yip yip yip yip
; Sha boom
; How many bits are required for the encoding? What is the smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))
      

(define (make_leaf_set pairs)
    (map (lambda (pair) (make-leaf (car pair) (cadr pair))) pairs)
)

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (successive-merge leaves)
    (if (> (length leaves) 1)
        (let 
            (
                (new_node 
                    (make-code-tree (car leaves) (cadr leaves))
                ) 
                (rest_set (cddr leaves)) 
            )
            (successive-merge (adjoin-set new_node rest_set))
        )
        (car leaves)
    )
)

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode_symbol (car message) 
                      tree)
       (encode (cdr message) tree))))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (encode_symbol ele tree)
    (if (leaf? tree) 
        nil
        (let 
            (
                (left (left-branch tree))
                (right (right-branch tree))
            )
            (cond 
                ((element-of-set? ele (symbols left)) (cons 0 (encode_symbol ele left)))   
                ((element-of-set? ele (symbols right)) (cons 1 (encode_symbol ele right)))   
                (else (error "ele not found" ele))
            )
        )
    )
)

(define a
    (list 
        (list 'a 2)
        (list 'na 16)
        (list 'boom 1)
        (list 'Sha 3)
        (list 'Get 2)
        (list 'yip 9)
        (list 'job 2)
        (list 'Wah 1)
    )
)
; Huffman tree
(define ht (generate-huffman-tree a))
(display ht)(newline)
; encode

(define
    message
    '(Get a job
    Sha na na na na na na na na

    Get a job
    Sha na na na na na na na na

    Wah yip yip yip yip 
    yip yip yip yip yip
    Sha boom)
)
(define eh (encode message ht))
(display (length eh))(newline)
(display eh)(newline)
; 84 required
; for fiexed 108

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf Wah 1) (leaf boom 1) (Wah boom) 2) (a Wah boom) 4) ((leaf Sha 3) ((leaf job 2) (leaf Get 2) (job Get) 4) (Sha job Get) 7) (a Wah boom Sha job Get) 11) (yip a Wah boom Sha job Get) 20) (na yip a Wah boom Sha job Get) 36)
; 84
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
; > 