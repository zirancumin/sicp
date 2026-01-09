#lang sicp

(define (double n) (+ n n))
(define (halve n) (/ n 2))
(define (even? n)
  (= (remainder n 2) 0))

;; (define (* a b)
;;   (cond ((= b 0) 0)
;;         ((even? b) (* (double a) (halve b)))
;;         (else (+ a (* a (- b 1))))))

(define (* a b)
  (x a b 0))

(define (x a counter product)
  (cond ((= counter 0) product)
        ((not (even? counter)) (x a (- counter 1) (+ a product)))
        (else (x (double a)
                 (halve counter)
                 product))))
