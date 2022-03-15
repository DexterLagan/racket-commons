# Racket Commons

A collection of libraries for Racket.

## Commons Contents
<pre>
(provide chop                       ; (chop l n)
         echo                       ; (echo ... ... ...)
         define-command-line-params ; (define-command-line-params appname param1 ...)
         list-to-string-lines       ; (list-to-string-lines l)
         string-nth                 ; (string-nth str nth [sep #px"\\s+"])
         mask                       ; (mask l1 l2)
         mask-not                   ; (mask-not l1 l2)
         non-empty-list?            ; (non-empty-list? l)
         non-empty-same-length?     ; (non-empty-same-length? l1 l2 l3 ...)
         currym                     ; ((currym func param1 param3) param2) (func param1 param2 param3)
         curry-ref                  ; (curry-ref id params ref)
         get-matching-seconds       ; (get-matching-seconds lst key)
         string-replace-list        ; (string-replace-list source pattern-list destination)
         multi-replace-line         ; (multi-replace-line line source-list destination)
         multi-replace-lines        ; (multi-replace-lines lines source-list destination)
         license-expired?           ; (license-expired? license-year)
         license-almost-expired?    ; (license-almost-expired? license-month)
         print-list                 ; (print-list l)
         list-of-one?               ; (list-of-one? l)
         ++                         ; (++ ... ... ...)
         zip                        ; (zip l1 l2)
         second-last                ; (second-last l)
         replace-filename-in-path   ; (replace-filename-in-path full-path new-filename)
         first-letter               ; (first-letter str)
         if-defined                 ; (if-defined some-symbol 'defined 'not defined)
         write-log                  ; (write-log str1 str2 ...)
         combine-with)              ; (combine-with f l1 l2)
</pre>
