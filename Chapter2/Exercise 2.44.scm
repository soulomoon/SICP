#lang racket/gui
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (make_vect x y)
  (cons x y))

(define (xcor_vect v)
  (car v))

(define (ycor_vect v)
  (cdr v))

(define (add_vect v1 v2)
  (make_vect (+ (xcor_vect v1)
                (xcor_vect v2))
             (+ (ycor_vect v1)
                (ycor_vect v2))))

(define (sub_vect v1 v2)
  (make_vect (- (xcor_vect v1)
                (xcor_vect v2))
             (- (ycor_vect v1)
                (ycor_vect v2))))

(define (scale_vect s v)
  (make_vect (* s (xcor_vect v))
             (* s (ycor_vect v))))


(define (make_frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin_frame f)
  (car f))

(define (edge1_frame f)
  (cadr f))

(define (edge2_frame f)
  (caddr f))

(define (frame_coord_map frame)
  (lambda (v)
    (add_vect
     (origin_frame frame)
     (add_vect (scale_vect (xcor_vect v)
                           (edge1_frame frame))
               (scale_vect (ycor_vect v)
                           (edge2_frame frame))))))

(define (make_segment start end)
  (cons start end))

(define (start_segment segment)
  (car segment))

(define (end_segment segment)
  (cdr segment))

(define (segments->painter segment_list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start_coord_map ((frame_coord_map frame) (start_segment segment)))
             (end_coord_map ((frame_coord_map frame) (end_segment segment))))
       (line
        (make-posn (xcor_vect start_coord_map) (ycor_vect start_coord_map))
        (make-posn (xcor_vect end_coord_map) (ycor_vect end_coord_map)))))
     segment_list)))

(define (transform_painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame_coord_map frame)))
      (let ((new_origin (m origin)))
        (painter
         (make_frame new_origin
                     (sub_vect (m corner1) new_origin)
                     (sub_vect (m corner2) new_origin)))))))


(define (beside painter1 painter2)
  (let ((split_point (make_vect 0.5 0.0)))
    (let ((paint_left
           (transform_painter painter1
                              (make_vect 0.0 0.0)
                              split_point
                              (make_vect 0.0 1.0)))
          (paint_right
           (transform_painter painter2
                              split_point
                              (make_vect 1.0 0.0)
                              (make_vect 0.5 1.0))))
      (lambda (frame)
        (paint_left frame)
        (paint_right frame)))))

(define (flip-vert painter)
  (transform_painter painter
                     (make_vect 0.0 1.0)
                     (make_vect 1.0 1.0)
                     (make_vect 0.0 0.0)))

(define (flip_horiz painter)
  (transform_painter painter
                     (make_vect 1.0 0.0)
                     (make_vect 0.0 0.0)
                     (make_vect 1.0 1.0)))

(define outline_frame_painter
  (segments->painter
   (list
    (make_segment (make_vect 0 0)
                  (make_vect 0 1))
    (make_segment (make_vect 0 1)
                  (make_vect 1 1))
    (make_segment (make_vect 1 1)
                  (make_vect 1 0))
    (make_segment (make_vect 1 0)
                  (make_vect 0 0)))))

(define x_painter
  (segments->painter
   (list
    (make_segment (make_vect 0 0)
                  (make_vect 1 1))
    (make_segment (make_vect 0 1)
                  (make_vect 1 0)))))

(define diamond_painter
  (segments->painter
   (list
    (make_segment (make_vect 0 0.5)
                  (make_vect 0.5 0))
    (make_segment (make_vect 0.5 0)
                  (make_vect 1 0.5))
    (make_segment (make_vect 1 0.5)
                  (make_vect 0.5 1))
    (make_segment (make_vect 0.5 1)
                  (make_vect 0 0.5)))))

(define wave
  (segments->painter
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


(define (rotate-180 painter)
  (flip-vert painter))


(define (rotate-270 painter)
  (transform_painter painter
                     (make_vect 1.0 0.0)
                     (make_vect 1.0 1.0)
                     (make_vect 0.0 0.0)))

(define (below painter1 painter2)
  (let ((split_point (make_vect 0.0 0.5)))
    (let ((paint_bottom
           (transform_painter painter1
                              (make_vect 0.0 0.0)
                              (make_vect 1.0 0.0)
                              split_point))
          (paint_top
           (transform_painter painter2
                              split_point
                              (make_vect 1.0 0.5)
                              (make_vect 0.0 1.0))))
      (lambda (frame)
        (paint_bottom frame)
        (paint_top frame)))))

(define (right_split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right_split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up_split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up_split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner_split painter n)
  (if (= n 0)
      painter
      (let ((up (up_split painter (- n 1)))
            (right (right_split painter (- n 1))))
        (let ((top_left (beside up up))
              (bottom_right (below right right))
              (corner (corner_split painter (- n 1))))
          (beside (below painter top_left)
                  (below bottom_right corner))))))

(define (square_limit painter n)
  (let ((quarter (corner_split painter n)))
    (let ((half (beside (flip_horiz quarter) quarter)))
      (below (flip-vert half) half))))


(define (below_transformations painter1 painter2)
  (flip_horiz (flip-vert (rotate-270 (beside (rotate-270 painter1) (rotate-270 painter2))))))

(define unit_frame (make_frame (make_vect 0 500) (make_vect 500 0) (make_vect 0 -500)))

(define wave2 (beside wave (flip-vert wave)))

(define (flipped-pairs painter)
  (let ((painter2 
         (beside painter 
                 (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))
(wave4 unit_frame)

