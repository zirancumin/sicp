#lang sicp

(define (square n) (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; ========================================

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
  (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; =======================================

(define (search-for-primes n count)
  (cond ((= count 0) (newline))
        ((prime? n)
         (timed-prime-test n)
         (search-for-primes (+ n 1) (- count 1)))
        (else (search-for-primes (+ n 1) count))))

;; ============== SPEED TEST =============
;; (search-for-primes 1000 3)
;; 1009 *** 5
;; 1013 *** 3
;; 1019 *** 3
;; (search-for-primes 10000 3)
;; 10007 *** 11
;; 10009 *** 12
;; 10037 *** 9
;; (search-for-primes 100000 3)
;; 100003 *** 33
;; 100019 *** 29
;; 100043 *** 31
;; (search-for-primes 1000000 3)
;; 1000003 *** 92
;; 1000033 *** 95
;; 1000037 *** 98