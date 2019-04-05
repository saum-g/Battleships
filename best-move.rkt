#lang racket

(define (build-grid r c v)
  (build-vector r (lambda (x) (make-vector c v))))
(define (set-grid! grid r c v)
  (vector-set! (vector-ref grid r) c v))


(define (grid-ref grid r c)
  (vector-ref (vector-ref grid r) c))
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
      
  