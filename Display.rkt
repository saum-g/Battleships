#lang racket
(require "state-class.rkt" "best-move.rkt" 2htdp/universe 2htdp/image lang/posn)

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

;change the screen size variables so that display occupies full screen
(provide screen-resize)
(define (screen-resize state x y)
  (send state set-screen-size! x y))

;makes the grid for a player during play mode depending on the strikes made by other player, should be called with start-row-no=0
(define (play-grid oth-pl-strikes w)
  ;to make a row for a the grid
  (define (grid-square-place sqr-state)
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
  (define (show-grid start-no)
    (cond [(= start-no 9) (show-row (vector-ref oth-pl-strikes 9) 0)]
          [else (above (show-row (vector-ref oth-pl-strikes start-no) 0) (show-grid (+ start-no 1)))]))
  (define (show-row ships-row start-no)
    (cond [(= start-no 9) (grid-square-place (vector-ref ships-row 9))]
          [else (beside (grid-square-place (vector-ref ships-row start-no)) (show-row ships-row (+ 1 start-no)))]))
  (show-grid 0))

;convert vector to grid
(define (convert-to-grid ships-vector)
  (define ships-grid (build-grid 10 10 0))
  (define (convert-helper start-no)
    (cond [(= start-no 5) ships-grid]
          [else (update-grid (cdr (vector-ref ships-vector start-no)) 0)
                (convert-helper (+ 1 start-no))]))
  (define (update-grid coord-vec start)
    (cond [(= start (vector-length coord-vec)) (void) ]
          [(= -1 (cdr (vector-ref coord-vec start))) (update-grid coord-vec (+ start 1))]
          [else (vector-set!(vector-ref ships-grid (exact-floor (cdr (vector-ref coord-vec start))))
                            (exact-floor (car (vector-ref coord-vec start))) 1)
                (update-grid coord-vec (+ start 1))]))
  (convert-helper 0))

;makes the grid during the placement mode.
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
  (define pl1-grid (cond [(equal? (send state get-mode) 2) (play-grid (get-field strikes-grid-2 state) w)]
                         [(= (send state get-player) 1) (place-grid (convert-to-grid (send state get-sv1)) w)]
                         [else (place-grid (build-grid 10 10 0) w)]))
  (define pl2-grid (cond [(equal? (send state get-mode) 2) (play-grid (get-field strikes-grid-1 state) w)]
                         [(= (send state get-player) 2) (place-grid (convert-to-grid (send state get-sv2)) w)]
                         [else (place-grid (build-grid 10 10 0) w)]))
  (define pl1-show (text "Player 1:" (- (* 0.125 h) (* 0.0375 w)) "Black"))
  (define pl2-show (text "Player 2:" (- (* 0.125 h) (* 0.0375 w)) "Black"))
  (place-images (list pl1-show pl2-show pl1-grid pl2-grid)
                (list (make-posn (* 0.25 w) (- (* 0.375 h) (* 0.1125 w))) (make-posn (* 0.75 w) (- (* 0.375 h) (* 0.1125 w)))
                      (make-posn (* 0.25 w) (* 0.5 h)) (make-posn (* 0.75 w) (* 0.5 h)))
                bckg))

(define (click-handler state x y event)
  ;(displayln state)
  (define grid-coord (send state return-grid-coord x y))
  (cond [(not (mouse=? event "button-down")) (void)]
        [(equal? (cons -1 -1) grid-coord) (void)]
        [(= (send state get-mode) 1) (send state fill-ships grid-coord)]
        [else (send state hit grid-coord)])
  state)

;initial state
(define init-state
  (new state%))

;returns #t or #f depending on whether all ships have been sunk or not
(define (all-ships-sunk strikes-grid-1 r c k)
  (cond[(and (= r 9) (= c 9)) (if(= (grid-ref strikes-grid-1 r c) 3) (equal? 17 (+ k 1)) (equal? 17 k))]
       [(equal? c 9) (if(= (grid-ref strikes-grid-1 r c) 3) (all-ships-sunk strikes-grid-1 (+ r 1) 0 (+ k 1)) (all-ships-sunk strikes-grid-1 (+ r 1) 0 k))]
       [else (cond[(equal? (grid-ref strikes-grid-1 r c) 3) (all-ships-sunk strikes-grid-1 r (+ c 1) (+ k 1))]
                  [else (all-ships-sunk strikes-grid-1 r (+ c 1) k)])]))
;to determine when to stop the program
(define (stop-cond state)
  (or (all-ships-sunk (get-field strikes-grid-1 state) 0 0 0) (all-ships-sunk (get-field strikes-grid-2 state) 0 0 0)))
(define (end-screen state)
  (define w (car (send state get-screen-size)))
  (define h (cdr (send state get-screen-size)))
  (define bckg (rectangle w h 'solid "white"))
  (cond (all-ships-sunk (get-field strikes-grid-1 state) 0 0 0)
        (place-images (list (text "Player1 Wins!!" (* 0.01 w) "Black")) (list (make-posn (* 0.5 w) (* 0.5 h))) bckg)
        (place-images (list (text "Player2 Wins!!" (* 0.01 w) "Black")) (list (make-posn (* 0.5 w) (* 0.5 h))) bckg)))

(big-bang init-state
  (display-mode 'fullscreen screen-resize)
  (to-draw screen)
  (on-mouse click-handler)
  (stop-when stop-cond end-screen)
  )

(define sample-grid (build-grid 10 10 0))
(set-grid! sample-grid 2 3 2)
(set-grid! sample-grid 1 4 1)