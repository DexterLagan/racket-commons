#lang racket/gui
(provide menu-bar
         ------------------------
         file-menu
         file-new
         file-open
         file-save
         file-save-as
         file-exit
         edit-menu
         edit-copy
         edit-paste
         edit-select-all
         help-menu
         help-about)

;;; purpose

; to provide a nice default menu bar.

;;; utilities

;; makes a separator
(define (------------------------)
  (void (new separator-menu-item% (parent file-menu))))

;;; defs

;; main menu bar
(define menu-bar
  (new menu-bar% (parent main-window)))

;; [file] menu
(define file-menu
  (new menu% (parent menu-bar)
       (label "&File")))

; new menu item
(define file-new
  (new menu-item% (parent file-menu)
       (label "&New")
       (callback (λ (c e) (void)))))

(------------------------)

; open menu item
(define file-open
  (new menu-item% (parent file-menu)
       (label "&Open...")
       (callback (λ (c e) (void)))))

; save menu item
(define file-save
  (new menu-item% (parent file-menu)
       (label "&Save")
       (callback (λ (c e) (void)))))

; save as menu item
(define file-save-as
  (new menu-item% (parent file-menu)
       (label "Save &As...")
       (callback (λ (c e) (void)))))

(------------------------)

; exit menu item
(define file-exit
  (new menu-item% (parent file-menu)
       (label "&Exit")
       (callback file-exit-callback)))

;; [edit] menu
(define edit-menu
  (new menu% (parent menu-bar)
       (label "Edit")))

; copy menu item
(define edit-copy
  (new menu-item% (parent edit-menu)
       (label "&Copy")
       (callback (λ (c e) (void)))))

; paste menu item
(define edit-paste
  (new menu-item% (parent edit-menu)
       (label "&Paste")
       (callback (λ (c e) (void)))))

; select all menu item
(define edit-select-all
  (new menu-item% (parent edit-menu)
       (label "Select &All")
       (callback (λ (c e) (void)))))
 
;; [help] menu
(define help-menu
  (new menu% (parent menu-bar)
       (label "Help")))

; about menu item
(define help-about
  (new menu-item% (parent help-menu)
       (label "&About...")
       (callback help-about-callback)))


; EOF
