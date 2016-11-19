; Exercise 2.50: Define the transformation flip-horiz, which flips painters horizontally, and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.

#lang racket/gui
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))
(define (drawline begine end)
    (line
        (make-posn (car begine) (cadr begine)) 
        (make-posn (car end) (cadr end)))
)

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

(define (segments_>painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (drawline
        ((frame_coord_map frame) 
         (start_segment segment))
        ((frame_coord_map frame) 
         (end_segment segment))))
     segment-list)))




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

; 3 triangle
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
                        (make_segment b c)
                        (make_segment a c)
                    )
                )
            )
            (segments_>painter segment_list)
        )
    )
)
(define wave
  (segments_>painter
   (list
    (make_segment (make_vect 0.5 0.4) ;;; leg triangle
                  (make_vect 0.6 0))
    (make_segment (make_vect 0.5 0.4)
                  (make_vect 0.4 0))
    (make_segment (make_vect 0.3 0)
                  (make_vect 0.35 0.4))
    (make_segment (make_vect 0.35 0.4)
                  (make_vect 0.3 0.7))
    (make_segment (make_vect 0.3 0.7)
                  (make_vect 0.2 0.6))
    (make_segment (make_vect 0.2 0.6)
                  (make_vect 0 0.8))
    (make_segment (make_vect 0 0.9)
                  (make_vect 0.2 0.7))
    (make_segment (make_vect 0.2 0.7)
                  (make_vect 0.3 0.75))
    (make_segment (make_vect 0.3 0.75)
                  (make_vect 0.4 0.75))
    (make_segment (make_vect 0.4 0.75)
                  (make_vect 0.35 0.9))
    (make_segment (make_vect 0.35 0.9)
                  (make_vect 0.4 1))
    (make_segment (make_vect 0.5 1)
                  (make_vect 0.55 0.9))
    (make_segment (make_vect 0.55 0.9)
                  (make_vect 0.5 0.75))
    (make_segment (make_vect 0.5 0.75)
                  (make_vect 0.6 0.75))
    (make_segment (make_vect 0.6 0.75)
                  (make_vect 1 0.45))
    (make_segment (make_vect 1 0.3)
                  (make_vect 0.6 0.5))
    (make_segment (make_vect 0.6 0.5)
                  (make_vect 0.7 0)))))

; 4.wave(I dont want to draw - -//// onz)
(define (transform_painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame_coord_map frame)))
      (let ((new_origin (m origin)))
        (painter (make_frame new_origin
                  (sub_vect (m corner1) 
                            new_origin)
                  (sub_vect (m corner2)
                            new_origin)))))))
(define (flip_vert painter)
  (transform_painter 
   painter
   (make_vect 0.0 1.0)   ; new origin
   (make_vect 1.0 1.0)   ; new end of edge1
   (make_vect 0.0 0.0))) ; new end of edge2

(define (flip_horiz painter)
  (transform_painter 
   painter
   (make_vect 1.0 0.0)   ; new origin
   (make_vect 0.0 0.0)   ; new end of edge1
   (make_vect 1.0 1.0))) ; new end of edge2

(define (rotate_180 painter)
  (transform_painter 
   painter
   (make_vect 1.0 1.0)   ; new origin
   (make_vect 0.0 1.0)   ; new end of edge1
   (make_vect 1.0 0.0))) ; new end of edge2
(define (rotate_270 painter)
  (transform_painter 
   painter
   (make_vect 1.0 0.0)   ; new origin
   (make_vect 1.0 1.0)   ; new end of edge1
   (make_vect 0.0 0.0))) ; new end of edge2
(define (nothing painter)
    painter
)
(define unit_frame1 (make_frame (make_vect 0 100) (make_vect 100 0) (make_vect 0 -100)))
(define unit_frame2 (make_frame (make_vect 0 200) (make_vect 100 0) (make_vect 0 -100)))
(define unit_frame3 (make_frame (make_vect 0 300) (make_vect 100 0) (make_vect 0 -100)))
(define unit_frame4 (make_frame (make_vect 0 400) (make_vect 100 0) (make_vect 0 -100)))
(outline_drawer unit_frame1)
((nothing wave) unit_frame1)
((flip_horiz wave) unit_frame2)
((rotate_180 wave) unit_frame3)
((rotate_270 wave) unit_frame4)