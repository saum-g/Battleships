#lang racket
(provide build-grid)
(define (build-grid r c v)
  (build-vector r (lambda (x) (make-vector c v))))
(define (set-grid! grid r c v)
  (vector-set! (vector-ref (vector-ref grid r) c) v))
(define (grid-ref grid r c)
  (vector-ref (vector-ref grid r) c))
(define (size_ship ship_no)
  (cond [(= ship_no 1) 2]
        [(or (= ship_no 2) (= ship_no 3)) 3]
        [(= ship_no 4) 4]
        [(= ship_no 5) 5]
        [else "anda"]))

(define (to n) (if (= n 0) '() (append (to (- n 1)) (list n))))
(provide state%)
(define state%
  (class object%
    (super-new)
    
    ;  data members (all private)
    (define mode 0) (define/public (change-mode) (set! mode 1))   ; two values : 0=placement 1=play
    (define player 1)  ; two values 1 2
    (define/public (change-player)
      (begin (set! count 1) (if (= player 1) (set! player 2) (set! player 1))))
    ; when player is changed in placement mode we set count to 0   
    (define ships-vector-1
            (build-vector 5 (lambda (x) (cons (string-append "ship" (~a (+ x 1)))
                                              (make-vector (size_ship (+ x 1)) (cons -1 -1))))))
    ;  to be changed in display.rkt : initialisation is not null
    (define ships-vector-2
            (build-vector 5 (lambda (x) (cons (string-append "ship" (~a (+ x 1)))
                                              (make-vector (size_ship (+ x 1)) (cons -1 -1))))))
    
    (field [strikes-grid-1 (build-grid 10 10 0)])
    (field [strikes-grid-2 (build-grid 10 10 0)])
    ; where all he has hit so far on the opponent's grid
    ; 0 -> no attempt  1 -> miss  2 -> hit  3-> ship sunk

    (define count 1) 
    (define (which-ship? n)
      (cond [(and (>= n 1) (<= n 2)) (cons 0 (- (remainder n 3) 1))]
            [(and (>= n 3) (<= n 5)) (cons 1 (- (remainder n 6) 3))]
            [(and (>= n 6) (<= n 8)) (cons 2 (- (remainder n 9) 6))]
            [(and (>= n 9) (<= n 12)) (cons 3 (- (remainder n 13) 9))]
            [(and (>= n 13) (<= n 17)) (cons 4 (- (remainder n 18) 13))]))
    (define/public (fill-ships coord)
       (define pair (which-ship? count))
      (cond[(equal? player 1) (begin
                              (vector-set! (cdr (vector-ref ships-vector-1 (car pair))) (cdr pair) coord)
                              (set! count (+ 1 count)))]
           [else (begin
                              (vector-set! (cdr (vector-ref ships-vector-2 (car pair))) (cdr pair) coord)
                              (set! count (+ 1 count)))]))
    ;  we need to change player 1 to 2 when the ships-vector-1 is full.
    ;  we need to change mode  1 to 2 when ships-vector-2 is also full.

    ;play time
    (define (lookup p? v i)
      (if (= i (vector-length v)) #f
          (if (p? (vector-ref v i)) (cons (+ 1 i) (+ 1 (vector-ref v i)))
              (lookup p? v (+ i 1)))))
    ;  search returns (ship . position) if found else #f
    (define/public (search1 coord)
      (let* ([formatted (vector-map (lambda (x) (cdr x)) ships-vector-1)]
             [searched (vector-map (lambda (x) (vector-member coord x)) formatted)])
        (lookup (lambda (x) (not (eq? #f x))) searched 0)))
     (define/public (search2 coord)
      (let* ([formatted (vector-map (lambda (x) (cdr x)) ships-vector-2)]
             [searched (map (lambda (x) (vector-member coord x)) formatted)])
        (lookup (lambda (x) (not (eq? #f x))) searched 0)))
        
        
        
   ; (define/public (hit? coord player)
    ;  (if (= player 1)
          
    
    ;storing the size of screen, assuming 1000X500 initially, changed during execution
    
    (define screen-width 1000)
    (define screen-height 500)
    (define/public (set-screen-size! x y)
      (set! screen-width x)
      (set! screen-height y)
      (cons x y))
    (define/public (get-screen-size)
      (cons screen-width screen-height))))

