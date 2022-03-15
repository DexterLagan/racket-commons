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

## IO Contents

<pre>
(provide add-file-extension            ; (add-file-extension filename extension)
         add-file-extensions           ; (add-file-extensions filenames extension)
         copy-file-to-folders          ; (copy-file-to-folders source-path destination-list overwrite?)
         create-list-of-files          ; (create-list-of-files filenames content)
         directories-exist?            ; (directories-exist? list-of-dirs)
         directory-list-str            ; (directory-list-str path)
         display-error-count           ; (display-error-count result msg)
         filename-path->string         ; (filename-path->string filename-path)
         find-files#                   ; (find-files# pred path)
         get-error-count               ; (get-error-count result msg)
         get-file-extension            ; (get-file-extension filename-path)
         get-file-extensions           ; (get-file-extensions filenames)
         get-file-lines                ; (get-file-lines path)
         get-file-list-from-prefix-ext ; (get-file-list-from-prefix-ext path prefix extension)
         get-filename                  ; (get-filename filename-w-ext)
         get-filenames                 ; (get-filenames filenames-w-ext)
         get-last-path-part            ; (get-last-path-part path)
         make-backup-file              ; (make-backup-file path)
         path!                         ; (path! path-or-string)
         process-text-files            ; (process-text-files process file-list)
         replace-filename-in-path      ; (replace-filename-in-path full-path new-filename)
         run-if-not-exists             ; (run-if-not-exists list-of-files operation)
         write-file-lines)             ; (write-file-lines lines path)
</pre>
