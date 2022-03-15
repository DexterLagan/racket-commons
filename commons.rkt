;;; purpose

; a collection of common racket functions

; Fancy varargs println
(define echo
  (lambda args
    (displayln (apply ~a args))))
    
; General list printing
(define (display-list l)
  (for-each displayln l))
  
; Like prefix but one letter
(define (first-letter s)
  (string-ref s 0))
  
; returns the second last item of a list
(define (second-last l)
  (second (reverse l)))

; zips two lists together
(define zip
  (lambda (l1 l2) (map list l1 l2)))

