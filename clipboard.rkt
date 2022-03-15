#lang racket/gui

(provide get-clipboard       ; (get-clipboard-text)
         set-clipboard-text) ; (set-clipboard-text s)

;;; defs

; returns the contents of the cliboard as text
(define (get-clipboard-text)
  (send the-clipboard get-clipboard-string 0))

; set the contents of the cliboard as text
(define (set-clipboard-text s)
  (send the-clipboard set-clipboard-string s 0))
  
  
; EOF
