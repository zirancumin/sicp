#lang sicp

;recursion
(define (f n)
  (cond ((< n 3) n)
        ((= n 3) (+
                  (f (- n 1))
                  (* 2 (f (- n 2)))
                  (* 3 (f (- n 3)))))
        ((> n 3) (+
                  (f (- n 1))
                  (* 2 (f (- n 2)))
                  (* 3 (f (- n 3)))))))

;iteration
(define (g n)
  (cond ((< n 3) n)
        ((= n 3) 4)
        ((> n 3) (g-iter 4 2 1 (- n 3)))))
(define (g-iter a b c count)
  (if (= count 0)
      a
      (g-iter (+ a (* 2 b) (* 3 c))
              a
              b
              (- count 1))))