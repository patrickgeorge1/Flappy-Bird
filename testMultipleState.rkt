#lang racket

(require 2htdp/image)
(require 2htdp/universe)

; ----- Universe -----
(define WIDTH 500)
(define HEIGHT 300)
(define OFFSET 10)





; struct  + initial state
(define-struct ball (x y color))
(define-struct brick (x y color))






; INITIAL STATE for 2 state = '(#brick #ball)
(define initial-state (list (ball 20 20 "red") (brick 50 50 "blue")))
(define tstate (list (ball 20 20 "red") (brick 50 50 "blue")))






; DRAW STATE for 2 state = '(#brick #ball)
(define (draw-frame-helper state)
    ; draw ball and brick
    (match-let* ([minge (car state)]
                 [patrat (car (cdr state))]
                 [(ball ball_x ball_y ball_color) minge]
                 [(brick brick_x brick_y brick_color) patrat])
      (place-image
       (rectangle 250 250 "solid" "black") 250 250
      (place-image
           (circle 250 "solid" ball_color)  250 250 
           (place-image
               (rectangle 500 500 "solid" brick_color) 250 250 
               (empty-scene 1000 1000))))))
(define (draw-frame state) (draw-frame-helper state))





; NEXT STATE for 2 state = '(#brick #ball)
(define (next-state-helper state)
        (match-let* ([old_m (car state)]
                     [old_p (car(cdr state))]   
                     [(ball ball_x ball_y  ball__) old_m]
                     [(brick brick_x brick_y brick__) old_p]
                     [new_b (ball (+ ball_x 1) (+ ball_y 1) ball__)]
                     [new_p (brick (+ brick_x 1) (+ brick_y 1) brick__)])
                     (list new_b new_p)))
(define (next-state state) (next-state-helper state))


; control ball
(define (change state pressed-key)
  (match-let* ([minge (car state)]
               [patrat (car (cdr state))]
               [(ball ball_x ball_y  ball__) minge]
               [(brick brick_x brick_y brick__) patrat])
    (cond [(key=? pressed-key " ") (list minge (brick (+ brick_x 1) (+ brick_y 1) brick__))]
          [else (list minge patrat)])))


; run
(big-bang initial-state
  [on-tick next-state (/ 1.0 30)]  ; fps
  [to-draw draw-frame]
  [on-key change])