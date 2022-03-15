# Racket Commons

A collection of libraries for Racket.

## Commons Contents

<pre>
(provide chop                            ; (chop l n)
         echo                            ; (echo ... ... ...)
         define-command-line-params      ; (define-command-line-params appname param1 ...)
         list-to-string-lines            ; (list-to-string-lines l)
         string-nth                      ; (string-nth str nth [sep #px"\\s+"])
         mask                            ; (mask l1 l2)
         mask-not                        ; (mask-not l1 l2)
         non-empty-list?                 ; (non-empty-list? l)
         non-empty-same-length?          ; (non-empty-same-length? l1 l2 l3 ...)
         currym                          ; ((currym func param1 param3) param2) (func param1 param2 param3)
         curry-ref                       ; (curry-ref id params ref)
         get-matching-seconds            ; (get-matching-seconds lst key)
         grep                            ; (grep lines regex-pattern)
         grepl                           ; (grep lines prefix)
         string-replace-list             ; (string-replace-list source pattern-list destination)
         multi-replace-line              ; (multi-replace-line line source-list destination)
         multi-replace-lines             ; (multi-replace-lines lines source-list destination)
         license-expired?                ; (license-expired? license-year)
         license-almost-expired?         ; (license-almost-expired? license-month)
         print-list                      ; (print-list l)
         list-of-one?                    ; (list-of-one? l)
         ++                              ; (++ ... ... ...)
         zip                             ; (zip l1 l2)
         second-last                     ; (second-last l)
         replace-filename-in-path        ; (replace-filename-in-path full-path new-filename)
         first-letter                    ; (first-letter str)
         if-defined                      ; (if-defined some-symbol 'defined 'not defined)
         write-log                       ; (write-log str1 str2 ...)
         take-everything-until-including ; (take-everything-until-including l starts-with)
         take-everything-starts-with     ; (take-everything-starts-with l prefix)
         take-everything-after-including ; (take-everything-after-including l starts-with)
         get-clipboard-text              ; (get-clipboard-text)
         set-clipboard-text              ; (set-clipboard-text s)
         get-unique-prefix-line          ; (get-unique-prefix-line lst prefix)
         label->filename                 ; (label->filename label ext)
         execute-async                   ; (execute-async startup-path program-binary-path command-line-parameters)
         combine-with)                   ; (combine-with f l1 l2)
</pre>

## IO Contents

<pre>
(provide add-file-extension            ; (add-file-extension filename extension)
         add-file-extensions           ; (add-file-extensions filenames extension)
         copy-file-to-folders          ; (copy-file-to-folders source-path destination-list overwrite?)
         copy-or-die                   ; (copy-or-die src dest)
         create-folders-or-die         ; (create-folders-or-die paths)
         create-list-of-files          ; (create-list-of-files filenames content)
         directories-exist?            ; (directories-exist? list-of-dirs)
         directory-list-str            ; (directory-list-str path)
         display-error-count           ; (display-error-count result msg)
         filename-path->string         ; (filename-path->string filename-path)
         find-files#                   ; (find-files# pred path)
         file-path?                    ; (file-path? path)
         file-paths?                   ; (file-paths? paths)
         folder-path?                  ; (folder-path? path)
         folder-paths?                 ; (folder-paths? paths)
         get-error-count               ; (get-error-count result msg)
         get-file-extension            ; (get-file-extension filename-path)
         get-file-extensions           ; (get-file-extensions filenames)
         get-file-lines                ; (get-file-lines path)
         get-file-list-from-prefix-ext ; (get-file-list-from-prefix-ext path prefix extension)
         get-filename                  ; (get-filename filename-w-ext)
         get-filenames                 ; (get-filenames filenames-w-ext)
         get-file-name                 ; (get-file-name path)
         get-last-path-part            ; (get-last-path-part path)
         make-backup-file              ; (make-backup-file path)
         maybe-copy-file               ; (maybe-copy-file source destination error-message exists-ok?)
         move-or-die                   ; (move-or-die src dest)
         path!                         ; (path! path-or-string)
         path&lt?                        ; (path&lt? p1 p2)
         process-text-files            ; (process-text-files process file-list)
         replace-filename-in-path      ; (replace-filename-in-path full-path new-filename)
         run-if-not-exists             ; (run-if-not-exists list-of-files operation)
         sort-paths                    ; (sort-paths paths)
         write-file-lines)             ; (write-file-lines lines path)
</pre>

## Dialogs Contents

<pre>
(provide die                         ; (die msg)
         get-directory-list          ; (get-directory-list title msg path)
         get-directory-list-w-prefix ; (get-directory-list-w-prefix title msg path folder_prefix)
         get-single-directory        ; (get-single-directory title msg path)
         hide-loading                ; (hide-loading)
         listbox-dialog              ; (listbox-dialog title message initial-listbox-contents style)
         listbox-selectall           ; (listbox-selectall list-box item-count select?)
         my-get-file-list            ; (my-get-file-list message path filetype_name filetype_pattern)
         msgbox                      ; (msgbox appname message)
         populate-listbox            ; (populate-listbox listbox listbox-contents)
         show-error-message          ; (show-error-message message)
         show-loading                ; (show-loading)
         show-confirmation-dialog    ; (show-confirmation-dialog appname message)
         show-warning-message)       ; (show-warning-message message)
</pre>

## GUI Contents

<pre>
(provide get-bitmap               ; (get-bitmap filename width height)
         is-checked?              ; (is-checked? check-box)
         make-elastic-frame)      ; (make-elastic-frame appname)
</pre>

## XML Contents

<pre>
(provide get-xml-value-from-id) ; (get-xml-value-from-id file pattern)
</pre>

## License
Racket Commons is free software; see [LICENSE](https://github.com/DexterLagan/racket-commons/blob/main/LICENSE) for more details.
