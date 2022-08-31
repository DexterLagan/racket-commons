#lang racket/base
(require racket/format
         racket/list
         racket/string)

(provide hash->string-list             ; (hash->string-list h)
         hash->sorted-string-list      ; (hash->sorted-string-list h)
         hash->flat-sorted-string-list ; (hash->flat-sorted-string-list h)
         maybe-hash)                   ; (maybe-hash cond possible-hash)
         
(module+ test
  (require rackunit))
  
;;; purpose

; to provide several useful hash functions.
  
;;; defs

;; used along hash-union can conditionally and funcitonally build a hash bit by bit
(define (maybe-hash cond possible-hash)
  (and (hash? possible-hash)
    (if cond possible-hash (hash))))

;; converts a hash table to a list of list of strings
(define (hash->string-list h)
  (map (λ (i) (list (~a (car i)) (~a (cdr i)))) (hash->list h)))
; unit test
(module+ test
  (check-equal? (hash->string-list
                 (hash 'a 1 'b 2))
                '(("b" "2") ("a" "1"))))

;; converts a hash table to a list of list of strings
(define (hash->sorted-string-list h)
  (let ((unsorted (map (λ (i)
                         (if (hash? (cdr i))
                             (list (~a (car i)) (hash->sorted-string-list (cdr i)))
                             (list (~a (car i)) (~a (cdr i)))))
                       (hash->list h))))
    (sort unsorted (λ (x y) (string<? (car x) (car y))))))
; unit test
(module+ test
  (check-equal? (hash->sorted-string-list
                 (hash 'a 1 'b 2))
                '(("a" "1") ("b" "2")))
  (check-equal? (hash->sorted-string-list
                 (hash 'a 1 'b (hash 'c 2 'd 3)))
                '(("a" "1") ("b" (("c" "2") ("d" "3"))))))

;; converts a hash table to a flat list of strings, ignoring first items in second lists
(define (hash->flat-sorted-string-list h)
  (let ((unsorted (map (λ (i)
                         (if (hash? (cdr i))
                             (flatten (list (~a (car i)) (hash->sorted-string-list (cdr i))))
                             (list (~a (car i)) (~a (cdr i)))))
                       (hash->list h))))
    (sort unsorted (λ (x y) (string<? (car x) (car y))))))
; unit test
(module+ test
  (check-equal? (hash->flat-sorted-string-list
                 (hash 'a 1 'b 2))
                '(("a" "1") ("b" "2")))
  (check-equal? (hash->flat-sorted-string-list
                 (hash 'a 1 'b (hash 'c 2 'd 3)))
                '(("a" "1") ("b" "c" "2" "d" "3"))))
                
                
; EOF
