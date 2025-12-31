#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
;; #lang sicp use application-order evaluation, which will lead to a dead loop.
;; Normal(regular)-order evalution will not lead to dead loop.