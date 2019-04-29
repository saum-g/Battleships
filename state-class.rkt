#lang racket
(require "best-move.rkt")
(require 2htdp/batch-io)
(provide build-grid)
(provide set-grid!)
(define (build-grid r c v)
  (build-vector r (lambda (x) (make-vector c v))))
(define (set-grid! grid r c v)
  (vector-set! (vector-ref grid (exact-floor r)) (exact-floor c) v))
(provide grid-ref)
(define (grid-ref grid r c)
  (vector-ref (vector-ref grid (exact-floor r)) (exact-floor c)))
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))



(define (ship-length? ship-no)
      (cond [(= ship-no 1) 2]
            [(= ship-no 2) 3]
            [(= ship-no 3) 3]
            [(= ship-no 4) 4]
            [(= ship-no 5) 5]
            [else "anda-danda-funda"]))


(define (to n) (if (= n 0) '() (append (to (- n 1)) (list n))))
(provide state%)
(define state%
  (class object%
    (super-new)
    
    ;  data members (all private)
    (define mode 0) (define/public (change-mode) (set! mode 2))   ; two values : 1=placement 2=play
    (define player 1)  ; two values 1 2
    (define no-of-players 2)
    (define learn 'off)
    (define last-move (cons -1 -1))

    
    (define/public (set-modes! which-option?)
      (cond [(equal? which-option? 'one-player-easy)
             (begin (set! mode 1) (set! no-of-players 1) (set! learn 'off))]
            [(equal? which-option? 'one-player-difficult)
             (begin (set! mode 1) (set! no-of-players 1) (set! learn 'on))]
            [(equal? which-option? 'two-player)
             (begin (set! mode 1) (set! no-of-players 2))]))
    
    (define/public (change-player)
      (begin (set! count 1) (if (= player 1) (set! player 2) (set! player 1))))
    (define/public (get-mode) mode)
    (define/public (get-player) player)         ;  Saumya is using these two functions in display
    (define/public (get-learn) learn)
    (define/public (get-no-of-players) no-of-players)
    ; when player is changed in placement mode we set count to 0   
    (define ships-vector-1
      (build-vector 5 (lambda (x) (cons (string-append "ship" (~a (+ x 1)))
                                        (make-vector (ship-length? (+ x 1)) (cons -1 -1))))))
    ;  to be changed in display.rkt : initialisation is not null
    (define ships-vector-2
      (build-vector 5 (lambda (x) (cons (string-append "ship" (~a (+ x 1)))
                                        (make-vector (ship-length? (+ x 1)) (cons -1 -1))))))

    (define/public (get-sv1)
      ships-vector-1)
    (define/public (get-sv2)
      ships-vector-2)
    
    (field [strikes-grid-1 (build-grid 10 10 0)])
    (field [strikes-grid-2 (build-grid 10 10 0)])
    ; where all he has hit so far on the opponent's grid
    ; 0 -> no attempt  1 -> miss  2 -> hit  3-> ship sunk

    (field [count 1]) 
    (define/public (which-ship? n)
      (cond [(and (>= n 1) (<= n 2)) (cons 0 (- (remainder n 3) 1))]
            [(and (>= n 3) (<= n 5)) (cons 1 (- (remainder n 6) 3))]
            [(and (>= n 6) (<= n 8)) (cons 2 (- (remainder n 9) 6))]
            [(and (>= n 9) (<= n 12)) (cons 3 (- (remainder n 13) 9))]
            [(and (>= n 13) (<= n 17)) (cons 4 (- (remainder n 18) 13))]))
    



    (define (fill-for-comp)
      (map (lambda (x) (send this fill-ships x)) (random-ships))
      (set! mode 2))

    (define direction "undefined")
    
    (define (diag-checker coord)
      (cond[(or (= count 1) (= count 3) (= count 6) (= count 9) (= count 13)) #t]
           [(or (= count 2) (= count 4) (= count 7) (= count 10) (= count 14))
            (let([res (direction-finder coord)])
              ;(displayln res)
              (cond[(eq? res "undefined") #f]
                   [else (begin (set! direction (direction-finder coord)) #t)]))]
           [else (eq? direction (direction-finder coord))]))
    
    (define (direction-finder coord)
      (cond [(= player 1) (let* ([pair (which-ship? (- count 1))]
                                 [vec (get-ship-coord (+ 1 (car pair)) player)]
                                 [ref-coord (vector-ref (cdr (vector-ref ships-vector-1 (car pair))) (cdr pair) )]
                                 [xr (car ref-coord)]
                                 [yr (cdr ref-coord)]
                                 [x (car coord)]
                                 [y (cdr coord)])
                            ; (displayln pair)
                            (cond [(and (= (- x xr) 1) (= y yr)) "right"]
                                  [(and (= (- xr x) 1) (= y yr)) "left"]
                                  [(and (= x xr) (= (- y yr) 1)) "up"]
                                  [(and (= x xr) (= (- yr y) 1)) "down"]
                                  [else "undefined"]))]
            [(= player 2) (let* ([pair (which-ship? (- count 1))]
                                 [vec (get-ship-coord (+ 1 (car pair)) player)]
                                 [ref-coord (vector-ref (cdr (vector-ref ships-vector-2 (car pair))) (cdr pair))]
                                 [xr (car ref-coord)]
                                 [yr (cdr ref-coord)]
                                 [x (car coord)]
                                 [y (cdr coord)])
                            (cond[(and (= (- x xr) 1) (= y yr)) "right"]
                                 [(and (= (- xr x) 1) (= y yr)) "left"]
                                 [(and (= x xr) (= (- y yr) 1)) "up"]
                                 [(and (= x xr) (= (- yr y) 1)) "down"]
                                 [else "undefined"]))]))
                           
      
    
    (define/public (fill-ships coord)
      (define pair (which-ship? count))
      (cond [(equal? player 1) (cond [(search1 coord) (void)]
                                     ;  if he tries to fill the same ship twice I am returning void 
                                     [(diag-checker coord)
                                      (begin
                                        (vector-set! (cdr (vector-ref ships-vector-1 (exact-floor (car pair))))
                                                     (exact-floor (cdr pair)) coord)
                                        (set! count (+ 1 count)))]
                                     [else (begin
                                             (map (lambda (x) (vector-set! (cdr (vector-ref ships-vector-1 (exact-floor (car pair))))
                                                                           x (cons -1 -1)))
                                                  (cons 0 (to (exact-floor (cdr pair)))))
                                             (set! count (- count (exact-floor (cdr pair)))))])]
            [else               (cond [(search2 coord) (void)]
                                      [(diag-checker coord)
                                       (begin
                                         (vector-set! (cdr (vector-ref ships-vector-2 (exact-floor (car pair))))
                                                      (exact-floor (cdr pair)) coord)
                                         (set! count (+ 1 count)))]
                                      [else (begin
                                              (map (lambda (x) (vector-set! (cdr (vector-ref ships-vector-2 (exact-floor (car pair))))
                                                                            x (cons -1 -1)))
                                                   (cons 0 (to (exact-floor (cdr pair)))))
                                              (set! count (- count (exact-floor (cdr pair)))))])])
      
      (cond [(= count 18)
             (cond [(equal? player 1) (displayln "player 1 filled ships")
                                      (change-player) (set! count 1)
                                      (if (= no-of-players 1)
                                          (fill-for-comp)
                                          (void))]
                   [else (set! mode 2) (displayln "Fill ships over.") (change-player)])]))

    
    ;  we need to change player 1 to 2 when the ships-vector-1 is full.
    ;  we need to change mode  1 to 2 when ships-vector-2 is also full.
    
    (define/public (get-ship-coord ship-no player)    ;  change to private later ;  returns the coordinate vector of ship-no of player
      (if (= player 1)
          (cdr (vector-ref ships-vector-1 (- ship-no 1)))
          (cdr (vector-ref ships-vector-2 (- ship-no 1)))))

    
    (define/public (full-ship-hit? ship-no player)  
      (if (= player 1)   ;  checks in player 1's strikes grid (where all he has hit so far (on player 2's "ship grid"))
          ;  whether coordinates of player 2's ship with given ship-no are all hit (value 2).
          (if (vector-member 0
                             (vector-map (lambda (x) (grid-ref strikes-grid-1 (cdr x) (car x)))  
                                         (get-ship-coord ship-no 2)))  ; find val on strikes grid for each ship coord
              #f #t)
          (if (vector-member 0
                             (vector-map (lambda (x) (grid-ref strikes-grid-2 (cdr x) (car x)))
                                         (get-ship-coord ship-no 1)))  ; find val on strikes grid for each ship coord
              #f #t)))
          
    (define (lookup p? v i)
      (if (= i (vector-length v)) #f
          (if (p? (vector-ref v i)) (cons (+ 1 i) (+ 1 (vector-ref v i)))  
              (lookup p? v (+ i 1)))))

    ;  search returns (ship-no . position) if found. Else #f

    (define/public (search1 coord)
      (let* ([formatted (vector-map (lambda (x) (cdr x)) ships-vector-1)]  
             [searched (vector-map (lambda (x) (vector-member coord x)) formatted)])
        (lookup (lambda (x) (not (eq? #f x))) searched 0)))   ;  search1 searches in ships-vector-1
    (define/public (search2 coord)
      (let* ([formatted (vector-map (lambda (x) (cdr x)) ships-vector-2)]
             [searched (vector-map (lambda (x) (vector-member coord x)) formatted)])
        (lookup (lambda (x) (not (eq? #f x))) searched 0)))
    
    (define/public (get-rem-lengths)
      (map (lambda (x) (ship-length? x))
           (lc x : x <- (to 5) @(not (full-ship-hit? x 2)))))
    
    (define (hit-for-comp)
      (define rem-length  (map (lambda (x) (ship-length? x))
                               (lc x : x <- (to 5) @(not (full-ship-hit? x 2)))))
      (cond [(null? rem-length) (void)]
            [else (displayln rem-length)
                  (displayln "with the forbidden points: ")
                  (displayln (car (forbidden-and-hit-points strikes-grid-2 0 0 '() '())))
                  (define best-move (determine-move strikes-grid-2 last-move rem-length learn))                                                                         
                  (displayln best-move)
                  (let ([search-result (search1 best-move)])
                    (if (not search-result)
                        (begin (set-grid! strikes-grid-2 (cdr best-move) (car best-move) 1))  ; set it to 1 if it's a miss
                        (begin (set-grid! strikes-grid-2 (cdr best-move) (car best-move) 2)  ; set it to 2 if it's a strike
                               ;if learning is to used
                               (cond [(equal? learn 'on)
                                      (let* ([mcontent (list->vector (read-csv-file "Posn-frequency.csv"))]
                                             [newstr ""])
                                        (begin ;to add 1 at the right place in mcontent
                                          (vector-set! mcontent (exact-floor (cdr best-move))
                                                       (foldr (lambda (x y) (if (= (length y) (- 9 (car best-move)))
                                                                                (cons (~a (+ 1 (string->number x))) y)
                                                                                (cons x y)))
                                                              '() (vector-ref mcontent (exact-floor (cdr best-move)))))
                                          ;convert to a list of strings
                                          (set! newstr (foldr (lambda (x y) (cons (string-join x ",") y)) '() (vector->list mcontent)))
                                          ;convert to a single string
                                          (set! newstr (string-join newstr "\n"))
                                          (write-file "Posn-frequency.csv" newstr)))])
                               (if (full-ship-hit? (car search-result) 2)  ; if full ship is sunk set all its best-move as 3 in strikes grid
                                   (vector-map (lambda (x) (set-grid! strikes-grid-2 (cdr x) (car x) 3))
                                               (get-ship-coord (car search-result) 1))
                                   (void))))
                    (set! last-move best-move)
                    (displayln (numbers-grid strikes-grid-2 rem-length learn))
                    (newline)
                    (if (= 1 (grid-ref strikes-grid-2 (cdr best-move) (car best-move)))
                        (void)
                        (hit-for-comp)))]))


    (define/public (hit coord)
      ;(define ans 0)
      (if (= player 1)
          (if (not (= 0 (grid-ref strikes-grid-1 (cdr coord) (car coord)))) (void)  ;  if hitting at same spot twice, do nothing
              (let ([search-result (search2 coord)])
                (if (not search-result)
                    (begin (set-grid! strikes-grid-1 (cdr coord) (car coord) 1))  ; set it to 1 if it's a miss
                    (begin (set-grid! strikes-grid-1 (cdr coord) (car coord) 2)  ; set it to 2 if it's a strike
                           (if (full-ship-hit? (car search-result) 1)  ; if full ship is sunk set all its coord as 3 in strikes grid
                               (vector-map (lambda (x) (set-grid! strikes-grid-1 (cdr x) (car x) 3))
                                           (get-ship-coord (car search-result) 2))
                               (void))))
                (if (= 1 (grid-ref strikes-grid-1 (cdr coord) (car coord)))
                    (if (= no-of-players 1)
                        (begin (displayln "giving control to computer")    ; this line transfers control to computer
                               (hit-for-comp))  
                        (begin
                          (displayln "changing player to 2 (in two player mode)")   ;  this line is normal control change to player 2 (on click input)
                          (change-player)))  
                    (void))))     ; this line of code gives him a second chance if he has sunk part of a ship

          (if (not (= 0 (grid-ref strikes-grid-2 (cdr coord) (car coord)))) (void)
              (let ([search-result (search1 coord)])
                (if(not search-result)
                   (begin (set-grid! strikes-grid-2 (cdr coord) (car coord) 1))
                   (begin (set-grid! strikes-grid-2 (cdr coord) (car coord) 2)
                          (if (full-ship-hit? (car search-result) 2)
                              (vector-map (lambda (x) (set-grid! strikes-grid-2 (cdr x) (car x) 3))
                                          (get-ship-coord (car search-result) 1))
                              (void)))) ; common to both
                ;                (if (= no-of-players 1)
                ;                    (begin (set! last-move coord)
                ;                           (display "numbers grid: ")
                ;                           (displayln (numbers-grid strikes-grid-2 '(2 3 3 4 5)))
                ;                           (newline)
                ;                           (if (= 1 (grid-ref strikes-grid-2 (cdr coord) (car coord)))
                ;                               (begin (displayln "player changing to 1")
                ;                                      (change-player))
                ;                               (hit (determine-move strikes-grid-2 last-move '(2 3 3 4 5)))))
                
                (if (= 1 (grid-ref strikes-grid-2 (cdr coord) (car coord)))
                    (begin (displayln "changing player to 1 (in two player mode)")
                           (change-player))
                    (void))))))
  

    (define/public (return-grid-coord x y)
      (cond [(and (= mode 1) (= player 1)) (if (and (<= x (* 0.4 screen-width)) (>= x (* 0.1 screen-width))
                                                    (<= y (+ (* 0.5 screen-height) (* 0.15 screen-width)))
                                                    (>= y (- (* 0.5 screen-height) (* 0.15 screen-width))))
                                               (cons (floor (/ (- x (* 0.1 screen-width)) (* 0.03 screen-width)))
                                                     (floor (/ (- y (- (* 0.5 screen-height) (* 0.15 screen-width))) (* 0.03 screen-width))))
                                               (cons -1 -1))]
            [(and (= mode 1) (= player 2)) (if (and (<= x (* 0.9 screen-width)) (>= x (* 0.6 screen-width))
                                                    (<= y (+ (* 0.5 screen-height) (* 0.15 screen-width)))
                                                    (>= y (- (* 0.5 screen-height) (* 0.15 screen-width))))
                                               (cons (floor (/ (- x (* 0.6 screen-width)) (* 0.03 screen-width)))
                                                     (floor (/ (- y (- (* 0.5 screen-height) (* 0.15 screen-width))) (* 0.03 screen-width))))
                                               (cons -1 -1))]
            [(and (= mode 2) (= player 1)) (if (and (<= x (* 0.9 screen-width)) (>= x (* 0.6 screen-width))
                                                    (<= y (+ (* 0.5 screen-height) (* 0.15 screen-width)))
                                                    (>= y (- (* 0.5 screen-height) (* 0.15 screen-width))))
                                               (cons (floor (/ (- x (* 0.6 screen-width)) (* 0.03 screen-width)))
                                                     (floor (/ (- y (- (* 0.5 screen-height) (* 0.15 screen-width))) (* 0.03 screen-width))))
                                               (cons -1 -1))]
            [(and (= mode 2) (= player 2)) (if (and (<= x (* 0.4 screen-width)) (>= x (* 0.1 screen-width))
                                                    (<= y (+ (* 0.5 screen-height) (* 0.15 screen-width)))
                                                    (>= y (- (* 0.5 screen-height) (* 0.15 screen-width))))
                                               (cons (floor (/ (- x (* 0.1 screen-width)) (* 0.03 screen-width)))
                                                     (floor (/ (- y (- (* 0.5 screen-height) (* 0.15 screen-width))) (* 0.03 screen-width))))
                                               (cons -1 -1))]
            [(= mode 0)                    (cond[ (and (<= x (* 0.75 screen-width)) (>= x (* 0.25 screen-width))
                                                       (<= y (* 0.62 screen-height)) (>= y (* 0.52 screen-height)))
                                                  (set-modes! 'one-player-easy)]
                                                [ (and (<= x (* 0.75 screen-width)) (>= x (* 0.25 screen-width))
                                                       (<= y (* 0.765 screen-height)) (>= y (* 0.665 screen-height)))
                                                  (set-modes! 'one-player-difficult)]
                                                [ (and (<= x (* 0.75 screen-width)) (>= x (* 0.25 screen-width))
                                                       (<= y (* 0.91 screen-height)) (>= y (* 0.81 screen-height)))
                                                  (set-modes! 'two-player)]
                                                [else (void)])]))


    ;storing the size of screen, assuming 1000X500 initially, changed during execution
    (define screen-width 1000)
    (define screen-height 500)
    (define/public (set-screen-size! x y)
      (set! screen-width x)
      (set! screen-height y)
      this)
    (define/public (get-screen-size)
      (cons screen-width screen-height))))

