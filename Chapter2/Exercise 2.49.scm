; Exercise 2.49: Use segments_>painter to define the following primitive painters:

; The painter that draws the outline of the designated frame.
; The painter that draws an “X” by connecting opposite corners of the frame.
; The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
; The wave painter.

#lang racket/gui
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (make_vect x y)
    (list x y)
)

(define (xcor_vect vect) (car vect))
(define (ycor_vect vect) (cadr vect))
(define (add_vect vect1 vect2)
    (map + vect1 vect2)
)

(define (sub_vect vect1 vect2)
    (map - vect1 vect2)
)

(define (scale_vect n vect)
    (map (lambda (v) (* v n)) vect)
)

(define (frame_coord_map frame)
  (lambda (v)
    (add_vect
     (origin_frame frame)
     (add_vect 
      (scale_vect (xcor_vect v)
                  (edge1_frame frame))
      (scale_vect (ycor_vect v)
                  (edge2_frame frame))))))
(define (make_segment vector1 vector2)
    (list vector1 vector2)
)

(define (start_segment seg)
    (car seg)
)
(define (end_segment seg)
    (cadr seg)
)

(define (make_frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make_frame1 origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin_frame frame) (car frame))
(define (edge1_frame frame) (cadr frame))
(define (edge2_frame frame) (caddr frame))

(define (segments_>painter segment_list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start_coord_map ((frame_coord_map frame) (start_segment segment)))
             (end_coord_map ((frame_coord_map frame) (end_segment segment))))
       (line
        (make-posn (xcor_vect start_coord_map) (ycor_vect start_coord_map))
        (make-posn (xcor_vect end_coord_map) (ycor_vect end_coord_map)))))
     segment_list)))


(define unit_frame (make_frame (make_vect 0 500) (make_vect 500 0) (make_vect 0 -500)))

;1 outline
(define outline_drawer
    (let 
        (
            (a (make_vect 0 0))
            (b (make_vect 0 1))
            (c (make_vect 1 0))
            (d (make_vect 1 1))
            
        )
        (let
            (
                (
                    segment_list 
                    (list 
                        (make_segment a b)
                        (make_segment a c)
                        (make_segment b d)
                        (make_segment c d)
                    )
                )
            )
            (segments_>painter segment_list)
        )
    )
)
; 2x
(define X_drawer
    (let 
        (
            (a (make_vect 0 0))
            (b (make_vect 0 1))
            (c (make_vect 1 0))
            (d (make_vect 1 1))
            
        )
        (let
            (
                (
                    segment_list 
                    (list 
                        (make_segment a d)
                        (make_segment b c)
                    )
                )
            )
            (segments_>painter segment_list)
        )
    )
)
; 3 diamond
(define D_drawer
    (let 
        (
            (a (make_vect 0.5 0))
            (b (make_vect 0 0.5))
            (c (make_vect 0.5 1))
            (d (make_vect 1 0.5))
            
        )
        (let
            (
                (
                    segment_list 
                    (list 
                        (make_segment a b)
                        (make_segment a d)
                        (make_segment b c)
                        (make_segment c d)
                    )
                )
            )
            (segments_>painter segment_list)
        )
    )
)
; 4.wave(I dont want to draw - -//// onz)
(define D_drawer
    (let 
        (
            (a (make_vect 0.5 0))
            (b (make_vect 0 0.5))
            (c (make_vect 0.5 1))
            (d (make_vect 1 0.5))
            
        )
        (let
            (
                (
                    segment_list 
                    (list 
                        (make_segment a b)
                        (make_segment a d)
                        (make_segment b c)
                        (make_segment c d)
                    )
                )
            )
            (segments_>painter segment_list)
        )
    )
)

(D_drawer unit_frame)