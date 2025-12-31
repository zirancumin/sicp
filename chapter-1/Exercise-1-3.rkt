#lang sicp

(define (max x y)
  (if (> x y) x y))

(define (fun a b c)
  (if (> a b) (+ a (max c b)) (+ b (max c a))))