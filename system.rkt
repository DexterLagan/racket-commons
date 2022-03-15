#lang racket/gui
(provide execute-async) ; (execute-async startup-path program-binary-path command-line-parameters)

;;; defs

;; launches a program in a cross-platform way
(define (execute-async startup-path program-binary-path command-line-parameters)
  (if (and (non-empty-string? startup-path)
           (non-empty-string? program-binary-path)
           (file-exists? program-binary-path))
      (if (equal? (system-type 'os) 'windows)
          (shell-execute #f
                         program-binary-path
                         command-line-parameters
                         startup-path
                         'sw_shownormal) ; possible values: 'sw_shownormal 'sw_hide 'sw_minimize
          (process program-binary-path))
      (show-error-message "This program is not installed.")))


; EOF