#lang sicp

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;; ========================================

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
  (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; =======================================

(define (search-for-primes n count)
  (cond ((= count 0) (newline))
        ((fast-prime? n 100)
         (timed-prime-test n)
         (search-for-primes (+ n 1) (- count 1)))
        (else (search-for-primes (+ n 1) count))))

;; (search-for-primes 1000 3)
;; 1009 *** 228
;; 1013 *** 230
;; 1019 *** 203
;; (search-for-primes 10000 3)
;; 10007 *** 231
;; 10009 *** 201
;; 10037 *** 225
;; (search-for-primes 100000 3)
;; 100003 *** 223
;; 100019 *** 224
;; 100043 *** 223
;; (search-for-primes 1000000 3)
;; 1000003 *** 323
;; 1000033 *** 283
;; 1000037 *** 289