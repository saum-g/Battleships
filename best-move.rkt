#lang racket

(require "state-class.rkt")
(define (set-grid! grid r c v)
  (vector-set! (vector-ref grid r) c v))

;returns the possible ways a part of a ship could be at x1,y1, provided the list of forbidden points and the length of the ship
(define (ways-for-sqr x1 y1 forbid-pt-list ship-length)
  (define highest-x 10)
  (define highest-y 10)
  (define least-x -1)
  (define least-y -1)
  ;determine highest and lowest x and y value in the same row/column which is forbidden
  (map (lambda (coord) (cond [(= (car coord) x1) (cond [(< (cdr coord) highest-y) (set! highest-y (cdr coord))]
                                                       [(> (cdr coord) least-y) (set! least-y (cdr coord))])]
                             [(= (cdr coord) y1) (cond [(< (car coord) highest-x) (set! highest-x (car coord))]
                                                       [(> (car coord) least-x) (set! least-x (car coord))])]))
       forbid-pt-list)
  ;calculate the number of blocks where a part of ship could be placed in each direction, keeping in mind that x1,y1 should always be occupied
  (define free-blocks-right (if (>= (- (- highest-x x1) 1) (- ship-length 1)) (- ship-length 1) (- (- highest-x x1) 1)))
  (define free-blocks-left (if (>= (- (- x1 least-x) 1) (- ship-length 1)) (- ship-length 1) (- (- x1 least-x) 1)))
  (define free-blocks-down (if (>= (- (- highest-y y1) 1) (- ship-length 1)) (- ship-length 1) (- (- highest-y y1) 1)))
  (define free-blocks-up (if (>= (- (- y1 least-y) 1) (- ship-length 1)) (- ship-length 1) (- (- y1 least-y) 1)))
  
  (define no-of-ways 0)
  (cond [(and (= free-blocks-right (- ship-length 1)) (= free-blocks-left (- ship-length 1))) (set! no-of-ways (+ no-of-ways ship-length))]
        [(= free-blocks-right (- ship-length 1)) (set! no-of-ways (+ no-of-ways free-blocks-left 1))]
        [(= free-blocks-left (- ship-length 1)) (set! no-of-ways (+ no-of-ways free-blocks-right 1))]
        [(>= (+ free-blocks-right free-blocks-left 1) ship-length) (set! no-of-ways (+ no-of-ways (- (+ free-blocks-right free-blocks-left 1) ship-length) 1))])
  (cond [(and (= free-blocks-down (- ship-length 1)) (= free-blocks-up (- ship-length 1))) (set! no-of-ways (+ no-of-ways ship-length))]
        [(= free-blocks-down (- ship-length 1)) (set! no-of-ways (+ no-of-ways free-blocks-up 1))]
        [(= free-blocks-up (- ship-length 1)) (set! no-of-ways (+ no-of-ways free-blocks-down 1))]
        [(>= (+ free-blocks-down free-blocks-up 1) ship-length) (set! no-of-ways (+ no-of-ways (- (+ free-blocks-down free-blocks-up 1) ship-length) 1))])
  no-of-ways)


    

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
                                 (max-helper (cons (cons r c) '()) (grid-ref prob-grid r c) prob-grid (+ r 1) 0)]
                                   [(= (grid-ref prob-grid r c) max-value)
                                     (max-helper (cons (cons r c) cons-list) max-value prob-grid (+ r 1) 0)]
                                 [else (max-helper cons-list max-value prob-grid (+ r 1) 0)])]
       [else  (cond[(> (grid-ref prob-grid r c) max-value)
                 (max-helper (cons (cons r c) '()) (grid-ref prob-grid r c) prob-grid r (+ c 1))]
                   [(= (grid-ref prob-grid r c) max-value)
                 (max-helper (cons (cons r c) cons-list) max-value prob-grid r (+ c 1))]
                   [else (max-helper cons-list max-value prob-grid r (+ c 1))])]))

(define (parity coord) (modulo (+ (car coord) (cdr coord)) 2))

(define (parity-narrow list-of-moves prev-move)
  (let ([prev (parity prev-move)])
    (filter (lambda (x) (= (parity x) prev))
            list-of-moves)))

(define (determine-move list-of-moves prev-move)
  (if (null? (cdr list-of-moves))
      (car list-of-moves)
      (parity-narrow list-of-moves prev-move)))
      









;;fills ships randomly for player mode-1 (computer mode)

(define (random-ship length)
  (let ([line (random-line length)])
    (if (makes-sense? line) line (random-ship length))))
 
(define (random-line length)
  (define start (cons (random 1 11) (random 1 11)))
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
    (if (and (and (> (car coord) 0) (< (car coord) 11))
             (and (> (cdr coord) 0) (< (cdr coord) 11)))
        #t
        #f))
  (andmap inside? line))

  











;;special function that saumya asked to form. returns two lists . First one has coordinates of points having 1/3 and second has coords with 2.

(define (spec-func strikes-grid r c l1 l2)
  (cond[(and (= r 9) (= c 9)) (cond[(or (= (grid-ref strikes-grid r c) 1) (= (grid-ref strikes-grid r c) 3)) (cons (cons (cons r c) l1) l2)]
                                   [(= (grid-ref strikes-grid r c) 2) (cons l1 (cons (cons r c) l2))]
                                   [else (cons l1 l2)])]
       [(equal? c 9) (cond[(or (= (grid-ref strikes-grid r c) 3) (= (grid-ref strikes-grid r c) 1)) (spec-func strikes-grid (+ r 1) 0 (cons (cons r c) l1) l2)]
                          [(= (grid-ref strikes-grid r c) 2) (spec-func strikes-grid (+ r 1) 0 l1 (cons (cons r c) l2))]
                          [else (spec-func strikes-grid (+ r 1) 0 l1 l2)])]
       [else (cond[(or (= (grid-ref strikes-grid r c) 1) (= (grid-ref strikes-grid r c) 3)) (spec-func strikes-grid r (+ c 1) (cons (cons r c) l1) l2)]
                  [(= (grid-ref strikes-grid r c) 2) (spec-func strikes-grid r (+ c 1) l1 (cons (cons r c) l2))]
                  [else (spec-func strikes-grid r (+ c 1) l1 l2)])]))

