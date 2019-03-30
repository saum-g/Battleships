#lang racket
(require "state-class.rkt" 2htdp/universe 2htdp/image lang/posn)

;change the screen size variables so that display occupies full screen
(provide screen-resize)
(define (screen-resize state x y)
  (send state set-screen-size! x y)
  "Resized")

;parameter of to-draw, draws the screen depending on state
(define (screen state)
  (define w (car (send state get-screen-size)))
  (define h (cdr (send state get-screen-size)))
  (define bckg (rectangle w h 'solid "white"))
  (define pl1-grid (square (* 0.3 w) 'solid "lightblue"))
  (define pl2-grid (square (* 0.3 w) 'solid "lightblue"))
  ;(place-image (square (* 0.3 w) 'solid "lightblue") (* 0.25 w) (* 0.5 h) bckg)
  ;(place-image (square (* 0.3 w) 'solid "lightblue") (* 0.75 w) (* 0.5 h) bckg))
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