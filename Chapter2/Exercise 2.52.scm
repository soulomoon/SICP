; Exercise 2.52: Make changes to the square limit of wave shown in Figure 2.9 by working at each of the levels described above. In particular:

; Add some segments to the primitive wave painter of Exercise 2.49 (to add a smile, for example).
; Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).
; Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (For example, you might make the big Mr. Rogers look outward from each corner of the square.)

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
    (make_segment (make_vect 0.48 0.48)
                  (make_vect 0.52 0.52))
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

(define (beside painter1 painter2)
  (let ((split_point (make_vect 0.5 0.0)))
    (let ((paint_left  (transform_painter 
                        painter1
                        (make_vect 0.0 0.0)
                        split_point
                        (make_vect 0.0 1.0)))
          (paint_right (transform_painter
                        painter2
                        split_point
                        (make_vect 1.0 0.0)
                        (make_vect 0.5 1.0))))
      (lambda (frame)
        (paint_left frame)
        (paint_right frame)))))

(define (below painter1 painter2)
  (let ((split_point (make_vect 0.0 0.5)))
    (let ((paint_down  (transform_painter 
                        painter1
                        (make_vect 0.0 0.0)
                        (make_vect 1.0 0.0)
                        split_point))
          (paint_up (transform_painter
                        painter2
                        split_point
                        (make_vect 1.0 0.5)
                        (make_vect 0.0 1.0))))
      (lambda (frame)
        (paint_up frame)
        (paint_down frame)))))

(define (below2 painter1 painter2)
    (let (
            (p1 (rotate_270 painter1))
            (p2 (rotate_270 painter2))
         )
         (rotate_180 (rotate_270 (below p1 p2)))
    )
)

(define (up_split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up_split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right_split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right_split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))



(define unit_frame1 (make_frame (make_vect 0 100) (make_vect 100 0) (make_vect 0 -100)))
(define unit_frame2 (make_frame (make_vect 0 200) (make_vect 100 0) (make_vect 0 -100)))
(define unit_frame3 (make_frame (make_vect 0 300) (make_vect 100 0) (make_vect 0 -100)))
(define unit_frame4 (make_frame (make_vect 0 500) (make_vect 200 0) (make_vect 0 -200)))
(outline_drawer unit_frame1)
((nothing wave) unit_frame1)
((beside wave wave) unit_frame2)


; 1 knife cut added
; 2 
(define (corner_split painter n)
  (if (= n 0)
      painter
      (let ((up (up_split painter (- n 1)))
            (right (right_split painter 
                                (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner_split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))
((corner_split  wave 3) unit_frame3)

; 3
(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip_vert 
                         rotate_180
                         identity
                         flip_horiz)))
    (combine4 (corner_split painter n))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

((square-limit  wave 3) unit_frame4)