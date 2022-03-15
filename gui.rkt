#lang racket/gui

(provide get-bitmap               ; (get-bitmap filename width height)
         is-checked?              ; (is-checked? check-box)
         make-elastic-frame)      ; (make-elastic-frame appname)

;;; purpose

; to provide a collection of useful gui functions

;;; version history

; v1.0 - this version.

;;; defs

;; returns true if check-box is checked, false otherwise
(define (is-checked? cb)
  (send cb get-value))

;; generates a small frame to fill with controls
(define (make-elastic-frame appname)
  (new (class frame% (super-new)
         (define/augment (on-close)
           (exit 0)))
       [label appname]
       [width 0]
       [height 0]
       [stretchable-width 1024]
       [stretchable-height #f]))

;; generate a bitmap from a filename
(define (get-bitmap filename width height)
  (let ((b (make-object bitmap% width height #f #t 1.0)))
    ; (make-object bitmap% (/ button-size 2) (/ button-size 2))
    (send b load-file filename)
    b))
    
    
; EOF
