#lang sicp

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-good-enough? guess x)
  (< (/ (abs (- guess (improve guess x))) x) 0.0001))

;; For small number, 0.001 is too large as tolerance between x^2 and guess^2.
;; The real tolerance between guess and (sqrt x) may larger than 0.001.
;; For example, (sqrt 0.002) => 0.03335, (- 0.002 (square 0.03335)) => -0.0009, (- (real-sqrt 0.002) 0.03335) => -0.019
;;
;; For big number, it will take a lot of time to calculate, but the accuracy is good enough.
;;
;; For our new-good-enough?(calculate with ratio), it works good with small number. But when x is big, the accuracy is bad than the previous algorithm.