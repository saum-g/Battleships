#lang racket
(require "state-class.rkt" 2htdp/universe 2htdp/image lang/posn)

;change the screen size variables so that display occupies full screen
(provide screen-resize)
(define (screen-resize state x y)
  (send state set-screen-size! x y)
  "Resized")

;makes the grid for a player during play mode depending on the strikes made by other player, should be called with start-row-no=0
(define (play-grid oth-pl-strikes start-row-no w)
  ;to make a row for a the grid
  (define (play-grid-row oth-pl-strk-row start-no)
    (cond [(= start-no 9) (grid-square-play (vector-ref oth-pl-strk-row 9))]
          [else (beside (grid-square-play (vector-ref oth-pl-strk-row start-no)) (play-grid-row oth-pl-strk-row (+ 1 start-no)))]))
  ;for making a particular square during play mode
  (define (grid-square-play sqr-state)
    (cond [(= sqr-state 0) (overlay (square (* 0.03 w) 'solid "lightblue")
                                    (square (* 0.03 w) 'outline "white"))]
          [(= sqr-state 1) (overlay (line (* 0.03 w) (* 0.03 w) "black")
                                    (line (- 0 (* 0.03 w)) (* 0.03 w) "black")
                                    (square (* 0.03 w) 'solid "lightblue")
                                    (square (* 0.03 w) 'outline "white"))]
          [(= sqr-state 2) (overlay (line (* 0.03 w) (* 0.03 w) "black")
                                    (line (- 0 (* 0.03 w)) (* 0.03 w) "black")
                                    (square (* 0.03 w) 'solid "red")
                                    (square (* 0.03 w) 'outline "white"))]
          [(= sqr-state 3) (overlay (line (* 0.03 w) (* 0.03 w) "black")
                                    (line (- 0 (* 0.03 w)) (* 0.03 w) "black")
                                    (square (* 0.03 w) 'solid "brown")
                                    (square (* 0.03 w) 'outline "white"))]))
  (cond [(= start-row-no 9) (play-grid-row (vector-ref oth-pl-strikes start-row-no))]
        [else (above (play-grid-row (vector-ref oth-pl-strikes start-row-no)) (play-grid oth-pl-strikes (+ 1 start-row-no)))]))

(define (convert-to-grid ships-vector)
  (define ships-grid (build-grid 10 10 0))
  (define (convert-helper start-no)
    (cond [(= start-no 5) ships-grid]
          [else (update-grid (cdr (vector-ref ships-vector start-no)))
                (convert-helper (+ 1 start-no))]))
  (define (update-grid coord-vec start)
    (cond [(= start (vector-length coord-vec)) ]
          [else (vector-set!(vector-ref ships-grid (cdr (vector-ref coord-vec start))) (car (vector-ref coord-vec start)) 1)]))
  (convert-helper 0))

;makes the grid during the placement mode
(define (place-grid ships-grid w)
  ;for making individual squares depending on the initial state
  (define (grid-square-place sqr-state)
    (cond [(= sqr-state 0) (overlay (square (* 0.03 w) 'solid "lightblue")
                                    (square (* 0.03 w) 'outline "white"))]
          [(= sqr-state 1) (overlay (square (* 0.03 w) 'solid "red")
                                    (square (* 0.03 w) 'outline "white"))]))
  (define (show-grid start-no)
    (cond [(= start-no 9) (show-row (vector-ref ships-grid 9) 0)]
          [else (above (show-row (vector-ref ships-grid start-no) 0) (show-grid (+ start-no 1)))]))
  (define (show-row ships-row start-no)
    (cond [(= start-no 9) (grid-square-place (vector-ref ships-row 9))]
          [else (beside (grid-square-place (vector-ref ships-row start-no)) (show-row ships-row (+ 1 start-no)))]))
  (show-grid 0))

;parameter of to-draw, draws the screen depending on state
(define (screen state)
  (define w (car (send state get-screen-size)))
  (define h (cdr (send state get-screen-size)))
  (define bckg (rectangle w h 'solid "white"))
  (define pl1-grid (cond [(= (send state get-player) 1) (place-grid (convert-to-grid (send state get-pl1-ships)) w)]
                         [else (build-grid 10 10 0)]))
  (define pl2-grid (cond [(= (send state get-player) 2) (place-grid (convert-to-grid (send state get-pl1-ships)) w)]
                         [else (build-grid 10 10 0)]))
  (place-images (list pl1-grid pl2-grid) (list (make-posn (* 0.25 w) (* 0.5 h)) (make-posn (* 0.75 w) (* 0.5 h))) bckg))


;initial state
(define init-state
  (new state%
       [md 'placement]
       [plyr 1]
       [pl1 (new player%)]
       [pl2 (new player%)]))
(define (stop-fn state)
  (pair? (send state get-screen-size)))

(big-bang init-state
  ;(display-mode 'fullscreen screen-resize)
  (to-draw screen)
  
  (stop-when stop-fn screen)
  )