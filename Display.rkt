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

;used to find the largest value in a given grid, used to scale the values for showing gradient in single player implementation
(define (grid-max max-value prob-grid r c)
  (cond[(and (= c 9) (= r 9)) (cond [(> (grid-ref prob-grid 9 9) max-value) (grid-ref prob-grid 9 9)]
                                    [else max-value])]
       [(and (= c 9) (< r 9)) (cond[(> (grid-ref prob-grid r c) max-value) (grid-max (grid-ref prob-grid r c) prob-grid (+ r 1) 0)]
                                   [else (grid-max max-value prob-grid (+ r 1) 0)])]
       [else  (cond [(> (grid-ref prob-grid r c) max-value) (grid-max (grid-ref prob-grid r c) prob-grid r (+ c 1))]
                    [else (grid-max max-value prob-grid r (+ c 1))])]))

;makes the grid for a player during play mode depending on the strikes made by other player, should be called with start-row-no=0
;turn is 0 if its not the player's turn, 1 if it is, -1 if its single player game and we are making grid of computer's moves
;numb-grid is 'nil if turn isnt -1 else it is the grid which is used to finally compute the best move
(define (play-grid oth-pl-strikes w turn numb-grid)
  (define max-val (if (equal? numb-grid 'nil) 'nil (grid-max -1 numb-grid 0 0)))
  ;val is not equal to 'nil if turn is -1, in that case it is the ratio the gradient should have
  (define (grid-square-place sqr-state val)
    (cond [(= turn -1) (cond [(= sqr-state 0) (overlay (square (* 0.03 w) (exact-floor (* val 127)) "lightblue")
                                                       (square (* 0.03 w) 'outline "black"))]
                             [(= sqr-state 1) (overlay (line (* 0.03 w) (* 0.03 w) "black")
                                                       (line (- 0 (* 0.03 w)) (* 0.03 w) "black")
                                                       (square (* 0.03 w) 0 "lightblue")
                                                       (square (* 0.03 w) 'outline "black"))]
                             [(= sqr-state 2) (overlay (line (* 0.03 w) (* 0.03 w) "black")
                                                       (line (- 0 (* 0.03 w)) (* 0.03 w) "black")
                                                       (square (* 0.03 w) 127 "red")
                                                       (square (* 0.03 w) 'outline "black"))]
                             [(= sqr-state 3) (overlay (line (* 0.03 w) (* 0.03 w) "black")
                                                       (line (- 0 (* 0.03 w)) (* 0.03 w) "black")
                                                       (square (* 0.03 w) 127 "brown")
                                                       (square (* 0.03 w) 'outline "black"))])]
          [else (cond [(= sqr-state 0) (overlay (square (* 0.03 w) (* (+ turn 1) 127) "lightblue")
                                                (square (* 0.03 w) 'outline "black"))]
                      [(= sqr-state 1) (overlay (line (* 0.03 w) (* 0.03 w) "black")
                                                (line (- 0 (* 0.03 w)) (* 0.03 w) "black")
                                                (square (* 0.03 w) (* (+ turn 1) 127) "lightblue")
                                                (square (* 0.03 w) 'outline "black"))]
                      [(= sqr-state 2) (overlay (line (* 0.03 w) (* 0.03 w) "black")
                                                (line (- 0 (* 0.03 w)) (* 0.03 w) "black")
                                                (square (* 0.03 w) (* (+ turn 1) 127) "red")
                                                (square (* 0.03 w) 'outline "black"))]
                      [(= sqr-state 3) (overlay (line (* 0.03 w) (* 0.03 w) "black")
                                                (line (- 0 (* 0.03 w)) (* 0.03 w) "black")
                                                (square (* 0.03 w) (* (+ turn 1) 127) "brown")
                                                (square (* 0.03 w) 'outline "black"))])]))
  (define (show-grid start-no)
    (cond [(= start-no 9) (show-row (vector-ref oth-pl-strikes 9) 0 9)]
          [else (above (show-row (vector-ref oth-pl-strikes start-no) 0 start-no) (show-grid (+ start-no 1)))]))
  ;to make a row for a the grid
  (define (show-row ships-row start-no row-no)
    (cond [(= start-no 9) (if (equal? 'nil max-val) (grid-square-place (vector-ref ships-row 9) 'nil) (grid-square-place (vector-ref ships-row 9) (/ (grid-ref numb-grid row-no 9) max-val)))]
          [else (beside (if (equal? 'nil max-val)
                            (grid-square-place (vector-ref ships-row start-no) 'nil)
                            (grid-square-place (vector-ref ships-row start-no) (/ (grid-ref numb-grid row-no start-no) max-val)))
                        (show-row ships-row (+ 1 start-no) row-no))]))
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
(define (place-grid ships-grid w turn?)
  ;for making individual squares depending on the initial state
  (define (grid-square-place sqr-state)
    (cond [(= sqr-state 0) (overlay (square (* 0.03 w) (* (+ turn? 1) 127) "lightblue")
                                    (square (* 0.03 w) 'outline "black"))]
          [(= sqr-state 1) (overlay (square (* 0.03 w) (* (+ turn? 1) 127) "red")
                                    (square (* 0.03 w) 'outline "black"))]))
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
  (define pl1-grid (cond [(and (equal? (send state get-mode) 2) (= (send state get-no-of-players) 1))
                          (play-grid (get-field strikes-grid-2 state) w -1 (numbers-grid (get-field strikes-grid-2 state) (send state get-rem-lengths) (send state get-learn)))]
                         [(and (= (send state get-player) 1) (equal? (send state get-mode) 2)) (play-grid (get-field strikes-grid-2 state) w 0 'nil)]
                         [(equal? (send state get-mode) 2) (play-grid (get-field strikes-grid-2 state) w 1 'nil)]
                         [(= (send state get-player) 1) (place-grid (convert-to-grid (send state get-sv1)) w 1)]
                         [else (place-grid (build-grid 10 10 0) w 0)]))
  (define pl2-grid (cond [(and (= (send state get-player) 2) (equal? (send state get-mode) 2))
                          (play-grid (get-field strikes-grid-2 state) w 0 'nil)]
                         [(equal? (send state get-mode) 2) (play-grid (get-field strikes-grid-1 state) w 1 'nil)]
                         [(= (send state get-player) 2) (place-grid (convert-to-grid (send state get-sv2)) w 1)]
                         [else (place-grid (build-grid 10 10 0) w 0)]))
  (define pl1-show (text "Player 1:" (exact-floor (- (* 0.125 h) (* 0.0375 w))) "Black"))
  (define pl2-show (text "Player 2:" (exact-floor (- (* 0.125 h) (* 0.0375 w))) "Black"))
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
  (if (all-ships-sunk (get-field strikes-grid-1 state) 0 0 0)
      (place-images (list (text "Player1 Wins!!" (exact-floor (* 0.05 w)) "Black")) (list (make-posn (* 0.5 w) (* 0.5 h))) bckg)
      (place-images (list (text "Player2 Wins!!" (exact-floor (* 0.05 w)) "Black")) (list (make-posn (* 0.5 w) (* 0.5 h))) bckg)))

(big-bang init-state
  (display-mode 'fullscreen screen-resize)
  (to-draw screen)
  (on-mouse click-handler)
  (stop-when stop-cond end-screen)
  )

(define sample-grid (build-grid 10 10 0))
(set-grid! sample-grid 2 3 2)
(set-grid! sample-grid 1 4 1)