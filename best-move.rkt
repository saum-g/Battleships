#lang racket
(require 2htdp/batch-io)
(provide all-defined-out)
(define (grid-ref grid r c)
  (vector-ref (vector-ref grid (exact-floor r)) (exact-floor c)))
(define (build-grid r c v)
  (build-vector r (lambda (x) (make-vector c v))))
(define (set-grid! grid r c v)
  (vector-set! (vector-ref grid r) c v))


(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

;returns pair of two lists, first one has coordinates of points having 1/3 and second has coords with 2. to be called with 0 0 '() '()
;;OPTIMISATION: should be updated after move of computer, not calculated every time
(provide forbidden-and-hit-points)
;(define (forbidden-and-hit-points strikes-grid r c forbid-points hit-points)
;  (let ([unformatted (forbidden-and-hit-points-help strikes-grid r c forbid-points hit-points)])
;    (cons (map (lambda (x) (cons (+ 0.0 (car x)) (+ 0.0 (cdr x)))) (car unformatted))
;          (map (lambda (x) (cons (+ 0.0 (car x)) (+ 0.0 (cdr x)))) (cdr unformatted)))))
(define (forbidden-and-hit-points strikes-grid r c forbid-points hit-points)
  (cond[(and (= r 9) (= c 9)) (cond[(or (= (grid-ref strikes-grid r c) 1) (= (grid-ref strikes-grid r c) 3)) (cons (cons (cons c r) forbid-points) hit-points)]
                                   [(= (grid-ref strikes-grid r c) 2) (cons forbid-points (cons (cons c r) hit-points))]
                                   [else (cons forbid-points hit-points)])]
       [(equal? c 9) (cond[(or (= (grid-ref strikes-grid r c) 3) (= (grid-ref strikes-grid r c) 1)) (forbidden-and-hit-points strikes-grid (+ r 1) 0 (cons (cons c r) forbid-points) hit-points)]
                          [(= (grid-ref strikes-grid r c) 2) (forbidden-and-hit-points strikes-grid (+ r 1) 0 forbid-points (cons (cons c r) hit-points))]
                          [else (forbidden-and-hit-points strikes-grid (+ r 1) 0 forbid-points hit-points)])]
       [else (cond[(or (= (grid-ref strikes-grid r c) 1) (= (grid-ref strikes-grid r c) 3)) (forbidden-and-hit-points strikes-grid r (+ c 1) (cons (cons c r) forbid-points) hit-points)]
                  [(= (grid-ref strikes-grid r c) 2) (forbidden-and-hit-points strikes-grid r (+ c 1) forbid-points (cons (cons c r) hit-points))]
                  [else (forbidden-and-hit-points strikes-grid r (+ c 1) forbid-points hit-points)])]))


;calculates the number of ways to place a ship given free-blocks-pos and neg as the number of blocks between the current block and the nearest forbidden one in the pos and neg direction
;ship-length>=0
(define (calc_ways free-blocks-pos free-blocks-neg ship-length)
  (begin (set! free-blocks-pos (min free-blocks-pos (- ship-length 1)))
         (set! free-blocks-neg (min free-blocks-neg (- ship-length 1)))
         (cond [(and (= free-blocks-pos (- ship-length 1)) (= free-blocks-neg (- ship-length 1))) ship-length]
               [(= free-blocks-pos (- ship-length 1)) (+ free-blocks-neg 1)]
               [(= free-blocks-neg (- ship-length 1)) (+ free-blocks-pos 1)]
               [(>= (+ free-blocks-pos free-blocks-neg 1) ship-length) (+ (- (+ free-blocks-pos free-blocks-neg 1) ship-length) 1)]
               [else 0])))

;returns the possible ways a part of a ship could be at x1,y1, provided the list of forbidden points and the length of the ship
;forbid-pt-list is list of points with status 1 or 3, through-pt-list is list of points with status 2
(define (ways-for-sqr x1 y1 forbid-pt-list through-pt-list ship-length)
  (let ([forbid-x-right 10]
        [forbid-y-down 10]
        [forbid-x-left -1]
        [forbid-y-up -1]
        ;check that they may remian these values till the end
        [through-x-right 10]
        [through-y-down 10]
        [through-x-left -1]
        [through-y-up -1]
        [no-of-ways 0])
    (cond [(or (member (cons x1 y1) forbid-pt-list) (member (cons x1 y1) through-pt-list)) 0];test if the given point itslef is forbidden
          [(null? through-pt-list) 
           (begin 
             ;determine nearest points in all four directions which are forbidden
             (map (lambda (coord) (cond [(= (car coord) x1) (cond [(and (> (cdr coord) y1) (< (cdr coord) forbid-y-down)) (set! forbid-y-down (cdr coord))]
                                                                  [(and (< (cdr coord) y1) (> (cdr coord) forbid-y-up)) (set! forbid-y-up (cdr coord))])]
                                        [(= (cdr coord) y1) (cond [(and (> (car coord) x1) (< (car coord) forbid-x-right)) (set! forbid-x-right (car coord))]
                                                                  [(and (< (car coord) x1) (> (car coord) forbid-x-left)) (set! forbid-x-left (car coord))])]))
                  forbid-pt-list)
             (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-x-right x1) 1) (- (- x1 forbid-x-left) 1) ship-length)))
             (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-y-down y1) 1) (- (- y1 forbid-y-up) 1) ship-length)))
             ;calculate the number of blocks where a part of ship could be placed in each direction, keeping in mind that x1,y1 should always be occupied
             no-of-ways)]
          [else
           (begin
             ;determine nearest points in all four directions which are forbidden
             (map (lambda (coord) (cond [(= (car coord) x1) (cond [(and (> (cdr coord) y1) (< (cdr coord) forbid-y-down)) (set! forbid-y-down (cdr coord))]
                                                                  [(and (< (cdr coord) y1) (> (cdr coord) forbid-y-up)) (set! forbid-y-up (cdr coord))])]
                                        [(= (cdr coord) y1) (cond [(and (> (car coord) x1) (< (car coord) forbid-x-right)) (set! forbid-x-right (car coord))]
                                                                  [(and (< (car coord) x1) (> (car coord) forbid-x-left)) (set! forbid-x-left (car coord))])]))
                  forbid-pt-list)
             ;determine nearest points in all four directions which are compulsory
             (map (lambda (coord) (cond [(= (car coord) x1) (cond [(and (> (cdr coord) y1) (< (cdr coord) through-y-down)) (set! through-y-down (cdr coord))]
                                                                  [(and (< (cdr coord) y1) (> (cdr coord) through-y-up)) (set! through-y-up (cdr coord))])]
                                        [(= (cdr coord) y1) (cond [(and (> (car coord) x1) (< (car coord) through-x-right)) (set! through-x-right (car coord))]
                                                                  [(and (< (car coord) x1) (> (car coord) through-x-left)) (set! through-x-left (car coord))])]))
                  through-pt-list)
             (cond [(and (<= forbid-x-right through-x-right) (>= forbid-x-left through-x-left)) (void)]
                   [(>= forbid-x-left through-x-left) (if (< (- through-x-right x1) ship-length)
                                                          (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-x-right through-x-right) 1)
                                                                                                    (- (- x1 forbid-x-left) 1) (- ship-length (- through-x-right x1)))))
                                                          (void))]
                   [(<= forbid-x-right through-x-right) (if (< (- x1 through-x-left) ship-length)
                                                            (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-x-right x1) 1)
                                                                                                      (- (- through-x-left forbid-x-left) 1) (- ship-length (- x1 through-x-left)))))
                                                            (void))]
                   [else (begin (if (< (- x1 through-x-left) ship-length)
                                    (set! no-of-ways (+ no-of-ways (calc_ways (- (- through-x-right x1) 1)
                                                                              (- (- through-x-left forbid-x-left) 1) (- ship-length (- x1 through-x-left)))))
                                    (void))
                                (if (< (- through-x-right x1) ship-length)
                                    (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-x-right through-x-right) 1)
                                                                              (- (- x1 through-x-left) 1) (- ship-length (- through-x-right x1)))))
                                    (void))
                                (if (< (- through-x-right through-x-left) ship-length)
                                    (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-x-right through-x-right) 1)
                                                                              (- (- through-x-left forbid-x-left) 1) (- ship-length (- through-x-right through-x-left)))))
                                    (void)))])
             (cond [(and (<= forbid-y-down through-y-down) (>= forbid-y-up through-y-up)) (void)]
                   [(>= forbid-y-up through-y-up) (if (< (- through-y-down y1) ship-length)
                                                          (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-y-down through-y-down) 1)
                                                                                                    (- (- y1 forbid-y-up) 1) (- ship-length (- through-y-down y1)))))
                                                          (void))]
                   [(<= forbid-y-down through-y-down) (if (< (- y1 through-y-up) ship-length)
                                                            (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-y-down y1) 1)
                                                                                                      (- (- through-y-up forbid-y-up) 1) (- ship-length (- y1 through-y-up)))))
                                                            (void))]
                   [else (begin (if (< (- y1 through-y-up) ship-length)
                                    (set! no-of-ways (+ no-of-ways (calc_ways (- (- through-y-down y1) 1)
                                                                              (- (- through-y-up forbid-y-up) 1) (- ship-length (- y1 through-y-up)))))
                                    (void))
                                (if (< (- through-y-down y1) ship-length)
                                    (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-y-down through-y-down) 1)
                                                                              (- (- y1 through-y-up) 1) (- ship-length (- through-y-down y1)))))
                                    (void))
                                (if (< (- through-y-down through-y-up) ship-length)
                                    (set! no-of-ways (+ no-of-ways (calc_ways (- (- forbid-y-down through-y-down) 1)
                                                                              (- (- through-y-up forbid-y-up) 1) (- ship-length (- through-y-down through-y-up)))))
                                    (void)))])
             no-of-ways)])))

(provide numbers-grid)
(define (numbers-grid strikes-grid rem-lengths)
  (define grid (build-grid 10 10 0))
  (define pair (forbidden-and-hit-points strikes-grid 0 0 '() '()))
  (lc (set-grid! grid r c (+ (grid-ref grid r c) (ways-for-sqr c r (car pair) (cdr pair) length))) :
      r <- (list 0 1 2 3 4 5 6 7 8 9) c <- (list 0 1 2 3 4 5 6 7 8 9) length <- rem-lengths)
  grid)


(define (min-of-list l)
  (min-help l (car l)))
(define (min-help l acc)
  (if (null? l) acc
      (if (< (car l) acc)
          (min-help (cdr l) (car l))
          (min-help (cdr l) acc))))

(define (max-prob prob-grid)
  (define x '())
  (define max-till-now 0)
  (max-helper x max-till-now prob-grid 0 0))

(define (max-helper cons-list max-value prob-grid r c)
  (cond[(and (= c 9) (= r 9)) (cond[(> (grid-ref prob-grid 9 9) max-value)
                                    (list (cons 9 9))]
                                   [(= (grid-ref prob-grid 9 9) max-value)
                                    (cons (cons 9 9) cons-list)]
                                   [else cons-list])]
       [(and (= c 9) (< r 9)) (cond[(> (grid-ref prob-grid r c) max-value)
                                 (max-helper (cons (cons c r) '()) (grid-ref prob-grid r c) prob-grid (+ r 1) 0)]
                                   [(= (grid-ref prob-grid r c) max-value)
                                     (max-helper (cons (cons c r) cons-list) max-value prob-grid (+ r 1) 0)]
                                 [else (max-helper cons-list max-value prob-grid (+ r 1) 0)])]
       [else  (cond[(> (grid-ref prob-grid r c) max-value)
                 (max-helper (cons (cons c r) '()) (grid-ref prob-grid r c) prob-grid r (+ c 1))]
                   [(= (grid-ref prob-grid r c) max-value)
                 (max-helper (cons (cons c r) cons-list) max-value prob-grid r (+ c 1))]
                   [else (max-helper cons-list max-value prob-grid r (+ c 1))])]))

(define (parity coord base) (modulo (+ (car coord) (cdr coord)) base))

(define (parity-narrow list-of-moves prev-move length)
  (let ([prev (parity prev-move length)])
    (filter (lambda (x) (= (parity x length) prev))
            list-of-moves)))

(provide determine-move)
(define (determine-move strikes-grid prev-move rem-lengths)
  (cond[(equal? learn 'on) (let*([ML-string-grid (list->vector (map list->vector (read-csv-file "Posn-frequency.csv")))]
                                 [ML-grid (vector-map (lambda (y) (vector-map string->number y)) ML-string-grid)]
                                 [merged-grid (merge strikes-grid ML-grid)]
                                 [coord (determine-help merged-grid prev-move rem-lengths)])
                             (cons (+ 0.0 (car coord)) (+ 0.0 (cdr coord))))]
       [else (let ([coord (determine-help strikes-grid prev-move rem-lengths)])
               (cons (+ 0.0 (car coord)) (+ 0.0 (cdr coord))))]))

(define (merge strikes-grid ML-grid)
  '())

(define (determine-help strikes-grid prev-move rem-lengths)
  (define prob-grid (numbers-grid strikes-grid rem-lengths))
  (define list-of-moves (max-prob prob-grid))
  (cond [(null? (cdr list-of-moves)) (car list-of-moves)]
        [(null? (parity-narrow list-of-moves prev-move (min-of-list rem-lengths))) (car list-of-moves)]
        [else (car (parity-narrow list-of-moves prev-move (min-of-list rem-lengths)))]))
      



; generates a random list of 17 coordinates which make sense as ships 1 player mode (computer mode)

(define (random-ship length)
  (map (lambda (x) (cons (+ 0.0 (car x)) (+ 0.0 (cdr x)))) (random-ship-help length)))
(define (random-ship-help length)
  (let ([line (random-line length)])
    (if (makes-sense? line) line (random-ship-help length))))
 
(define (random-line length)
  (define start (cons (random 0 10) (random 0 10)))
  (define dirn (list-ref '("up" "down" "left" "right") (random 0 4)))
  (extend-line start dirn length))

(define (extend-line start dirn len)
  (if (= len 1) (list start)
      (cond [(eq? dirn "up")
             (cons start (extend-line (cons (car start) (+ 1 (cdr start))) dirn (- len 1)))]
            [(eq? dirn "down")
             (cons start (extend-line (cons (car start) (- (cdr start) 1)) dirn (- len 1)))]
            [(eq? dirn "right")
             (cons start (extend-line (cons (+ (car start) 1) (cdr start)) dirn (- len 1)))]
            [(eq? dirn "left")
             (cons start (extend-line (cons (- (car start) 1) (cdr start)) dirn (- len 1)))])))

(define (flatten l)
  (if (null? l) '()
      (if (list? (car l))
          (append (flatten (car l)) (flatten (cdr l)))
          (cons (car l) (flatten (cdr l))))))

(define (makes-sense? line)   ;  makes sure no coordinate is out of the grid
  (define (inside? coord)
    (if (and (and (> (car coord) -1) (< (car coord) 10))
             (and (> (cdr coord) -1) (< (cdr coord) 10)))
        #t
        #f))
  (andmap inside? line))
(provide random-ships)
(define (random-ships)
  (define l '(2 3 3 4 5))
  (ships-helper l '()))

(define (ships-helper l c)
  (cond[(null? l) (append* (reverse c))]
       [else (let ([res (random-ship (car l))])
               (cond[(satisfy? res c) (ships-helper (cdr l) (cons res c))]
                    [else (ships-helper l c)]))]))

(define (satisfy? res c)
  (let* ([res-list  (lc (member x y) : x <- res y <- c )]  ;(map (lambda (x) (member res x)) c)]
         [all (filter (lambda (x) (not (equal? x #f))) res-list)])
    (cond[(= (length all) 0) #t]
         [else #f])))
    





