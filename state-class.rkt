#lang racket

(define (build-grid r c v)
  (build-vector r (lambda (x) (make-vector c v))))
(define (set-grid! grid r c v)
  (vector-set! (vector-ref (vector-ref grid r) c) v))
(define (grid-ref grid r c)
  (vector-ref (vector-ref grid r) c))

(provide state%)
(define state%
  (class object%
    (super-new)

    ;data members (all private)
    (init md) (define mode md)  ; two values : 'placement 'play
    (init plyr) (define player plyr)  ; two values 1 2
    (init pl1) (define player1 pl1)  ; these are instances (objects) of the class player%
    (init pl2) (define player2 pl2)

    ;storing the size of screen, assuming 100X50 initially, changed during execution
    (define screen-width 1000)
    (define screen-height 500)
    (define/public (set-screen-size! x y)
      (set! screen-width x)
      (set! screen-height y)
      (cons x y))
    (define/public (get-screen-size)
      (cons screen-width screen-height))

    ;member functions
    
    
    ))

(provide player%)
(define player%
  (class object%
    (super-new)
    (define ships-vector
      (build-vector 5 (lambda (x) (cons (string-append "ship" (~a (+ x 1)))
                                                       (make-vector 5 (cons -1 -1))))))
    ; vector containing 5 ships and their coordinates packed in a vector #((ship1 . #((1,2)(1,3))), (ship2 . #((4,5)(5,5)(6,5)) ...)
    (define ships-grid (build-grid 10 10 0))  ; grid pe kaha kaha ship hai
    (define strikes-grid (build-grid 10 10 0))  ; where all he has hit so far on the opponent's grid
    ; 0 -> no attempt  1 -> miss  2 -> hit  3-> ship sunk
    ))
    ;member functions to change these private fields (read data members)

    ;(define (fill-ships 

