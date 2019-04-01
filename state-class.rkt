#lang racket
(provide build-grid)
(define (build-grid r c v)
  (build-vector r (lambda (x) (make-vector c v))))
(define (set-grid! grid r c v)
  (vector-set! (vector-ref grid r) (vector-ref (vector-ref grid r) c) v))
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
    (define/public (get-mode) mode)
    (define/public (get player) player)         ;  Saumya is using these two functions in display
    ; when player is changed in placement mode we set count to 0   
    (define ships-vector-1
            (build-vector 5 (lambda (x) (cons (string-append "ship" (~a (+ x 1)))
                                              (make-vector (size_ship (+ x 1)) (cons -1 -1))))))
    ;  to be changed in display.rkt : initialisation is not null
    (define ships-vector-2
            (build-vector 5 (lambda (x) (cons (string-append "ship" (~a (+ x 1)))
                                              (make-vector (size_ship (+ x 1)) (cons -1 -1))))))

    (define/public (get-sv1)
      ships-vector-1)
    (define/public (get-sv2)
      ships-vector-2)
    
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
                   (set! count (+ 1 count)))])
      (cond [(= count 17) (cond[(equal? player 1) (begin (set! player 2) (set! count 1))]
                               [else (set! mode 2)])]))

    
    ;  we need to change player 1 to 2 when the ships-vector-1 is full.
    ;  we need to change mode  1 to 2 when ships-vector-2 is also full.

    (define/public (get-ship-coord ship_no player)    ;change to private later
      (if (= player 1)
          (cdr (vector-ref ships-vector-1 (- ship_no 1)))
          (cdr (vector-ref ships-vector-2 (- ship_no 1)))))

    
    (define/public (full-ship-hit? ship_no player)
      (if (= player 1)
          (if (vector-member 0
               (vector-map (lambda (x) (grid-ref strikes-grid-1 (- (cdr x) 1) (- (car x) 1)))  ;  always remember we havent changed (car x) according to 1 based indexing
                           (get-ship-coord ship_no 1)))  ; find val on strikes grid for each ship coord
              #f #t)
          (if (vector-member 0
               (vector-map (lambda (x) (grid-ref strikes-grid-2 (- (cdr x) 1) (- (car x) 1)))
                           (get-ship-coord ship_no 2)))  ; find val on strikes grid for each ship coord
              #f #t)))
          
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
    
    (define/public (hit? coord player)
      (if (= player 1)
          (let ([search-result (search1 coord)])
            (if (not search-result)
                (begin (set-grid! strikes-grid-1 (- (cdr coord) 1) (car coord) 1) 1)
                (begin (set-grid! strikes-grid-1 (- (cdr coord) 1) (car coord) 2)
                       (if (full-ship-hit? (car search-result) 1)
                           (begin (vector-map (lambda (x) (set-grid! strikes-grid-1 (- (cdr x) 1) (car x) 3))
                                              (get-ship-coord (car search-result) 1)) 3)
                           2))))
     (let ([search-result (search2 coord)])
            (if (not search-result)
                (begin (set-grid! strikes-grid-2 (cdr coord) (car coord) 1) 1)
                (begin (set-grid! strikes-grid-2 (cdr coord) (car coord) 2)
                       (if (full-ship-hit? (car search-result) 1)
                           (vector-map (lambda (x) (set-grid! strikes-grid-2 (- (cdr x) 1) (car x) 3))
                                       (get-ship-coord (car search-result) 1))
                           2))))))
      
     ;  to decide between 2 or 3 we'll have to check if the other coordinates of the ship are 0 or 2 in strikes-grid 
          
    
    ;storing the size of screen, assuming 1000X500 initially, changed during execution
    
    (define screen-width 1000)
    (define screen-height 500)
    (define/public (set-screen-size! x y)
      (set! screen-width x)
      (set! screen-height y)
      (cons x y))
    (define/public (get-screen-size)
      (cons screen-width screen-height))))

