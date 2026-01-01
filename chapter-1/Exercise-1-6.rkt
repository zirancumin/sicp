#lang sicp

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; (define (new-if predicate then-clause else-clause)
;;   (cond (predicate then-clause)
;;         (else else-clause)))
;;
;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;;       guess
;;       (sqrt-iter (improve guess x)
;;                  x)))
;;
;; Out of memory. New-if uses application-order evaluation, when calculating
;; actual argument (sqrt-iter ...), it will get stuck in infinite recursion.