# Racket Commons

A collection of useful procedures divided into modules for Racket.

## Version history

- v1.0.24 - added clean-var, inspect macro
- v1.0.23 - added sort-filepaths-by-inc-size amd formatted-file-size to common module
- v1.0.22 - improved group procedure safety in common module
- v1.0.21 - updated get-latest-version
- v1.0.20 - sorted provide in common module
- v1.0.19 - added get-current-executable-path
- v1.0.18 - added non-empty-or-none-string?
- v1.0.17 - added get-latest-version-number
- v1.0.16 - added non-empty-list-of-numbers?
- v1.0.15 - added media-file?.
- v1.0.14 - added list! and maybe-hash
- v1.0.13 - updated transpose and improved unit test coverage.
- v1.0.12 - added remove-indexed-items.
- v1.0.11 - added system->ports.
- v1.0.10 - added string->list-of-numbers.
- v1.0.9  - added dies.
- v1.0.8  - added string-contains-one-of?.
- v1.0.7  - added str-list-contains.
- v1.0.6  - added strip-newlines-returns.
- v1.0.5  - added string-replace3.
- v1.0.5  - added auto-quote.
- v1.0.4  - added take-up-to and group (cuts a list into chunks).
- v1.0.3  - added remove-non-alphanumeric-or-underscore.
- v1.0.2  - added string-chop.
- v1.0.1  - added non-empty-list-of-strings?.
- v1.0    - initial release.

## Commons procedures

<pre>
(provide all-but-last                          ; (all-but-last l)
         auto-quote                            ; (auto-quote str)
         clean-var-name                        ; (clean-var-name var)
         chop                                  ; (chop l n)
         combine-with                          ; (combine-with f l1 l2)
         comp_                                 ; (comp_ stx) [MACRO]
         curry-ref                             ; (curry-ref ... ) [MACRO]
         currym                                ; (currym func param1 param3)
         define-command-line-params            ; (define-command-line-params ... ) [MACRO]
         dies                                  ; (dies s1 s2 ...)
         each-of-each                          ; (each-of-each l)
         echo                                  ; (echo s1 s2 ...)
         filter-zip                            ; (filter-zip pred-list list)
         first-letter                          ; (first-letter s)
         first-of-each                         ; (first-of-each l)
         first-two-of-each                     ; (first-two-of-each l)
         formatted-file-size                   ; (formatted-file-size filepath)
         get-current-executable-path           ; (get-current-executable-path)
         get-file-content-type                 ; (get-file-content-type filepath)
         get-latest-version-number             ; (get-latest-version-number versions prefix)
         get-matching-seconds                  ; (get-matching-seconds lst key)
         get-unique-prefix-line                ; (get-unique-prefix-line lst prefix)
         grep                                  ; (grep lines regex-pattern)
         grepl                                 ; (grepl lines prefix)
         group                                 ; (group n lst)
         if-defined                            ; (if-defined stx) [MACRO]
         inspect                               ; (inspect x ...)
         label->filename                       ; (label->filename label ext)
         license-almost-expired?               ; (license-almost-expired? license-month)
         license-expired?                      ; (license-expired? license-year)
         list!                                 ; (list! v)
         list-of-one?                          ; (list-of-one? l)
         list-to-string-lines                  ; (list-to-string-lines l)
         mask                                  ; (mask l1 l2)
         mask-not                              ; (mask-not l1 l2)
         media-file?                           ; (media-file? f)
         multi-replace-line                    ; (multi-replace-line line source-list destination)
         multi-replace-lines                   ; (multi-replace-lines lines source-list destination)
         non-empty-list-of-list?               ; (non-empty-list-of-list? l)
         non-empty-list-of-numbers?            ; (non-empty-list-of-numbers? l)
         non-empty-list-of-strings?            ; (non-empty-list-of-strings? l)
         non-empty-list?                       ; (non-empty-list? l)
         non-empty-or-none-string?             ; (non-empty-or-none-string? s)
         pad                                   ; (pad l len default)
         pad*                                  ; (pad* l default)
         print-list                            ; (print-list l)
         remove-indexed-items                  ; (remove-indexed-items items indexes)
         remove-non-alphanumeric-or-underscore ; (remove-non-alphanumeric-or-underscore s)
         remove-nth                            ; (remove-nth lst n)
         replace-filename-in-path              ; (replace-filename-in-path full-path new-filename)
         rest-of-each                          ; (rest-of-each l)
         second-last                           ; (second-last l)
         second-of-each                        ; (second-of-each l)
         second-true?                          ; (second-true? l)
         second?                               ; (second? l)
         sort-filepaths-by-inc-size            ; (sort-filepaths-by-inc-size filepaths)
         str-list-contains                     ; (str-list-contains l s)
         str-list-contains?                    ; (str-list-contains? l s)
         string->label                         ; (string->label s)
         string->list-of-numbers               ; (string->list-of-numbers str)
         string-chop                           ; (string-chop s len)
         string-contains-one-of?               ; (string-contains-one-of? s l)
         string-nth                            ; (string-nth str nth (sep #px"\\s+"))
         string-replace-list                   ; (string-replace-list source pattern-list destination)
         string-replace2                       ; (string-replace2 s from1 to1 from2 to2)
         string-replace3                       ; (string-replace3 s from1 to1 from2 to2 from3 to3)
         strip-newlines-returns                ; (strip-newlines-returns str)
         swap-columns-and-rows                 ; (swap-columns-and-rows l)
         swap-columns-to-rows-vector           ; (swap-columns-to-rows-vector v)
         system->ports                         ; (system->ports command)
         take-everything-after-including       ; (take-everything-after-including l starts-with)
         take-everything-starts-with           ; (take-everything-starts-with l prefix)
         take-everything-until-including       ; (take-everything-until-including l starts-with)
         take-up-to                            ; (take-up-to n lst)
         transpose                             ; (transpose l)
         zip)                                  ; (zip l1 l2)
</pre>

## IO procedures

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
         list->file                    ; (list->file l file)
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

## Dialogs procedures

<pre>
(provide centered-listbox-dialog     ; (centered-listbox-dialog title message initial-listbox-contents style width-ratio height-ratio)
         die                         ; (die msg)
         die#                        ; (die# msg)
         get-directory-list          ; (get-directory-list title msg path)
         get-directory-list-w-prefix ; (get-directory-list-w-prefix title msg path folder_prefix)
         get-single-directory        ; (get-single-directory title msg path)
         get-string-or-die           ; (get-string-or-die msg error)
         hide-loading                ; (hide-loading)
         listbox-dialog              ; (listbox-dialog title message initial-listbox-contents style)
         listbox-dialog#             ; (listbox-dialog# title message headers initial-listbox-contents selection-type width height)
         listbox-selectall           ; (listbox-selectall list-box item-count select?)
         my-get-file-list            ; (my-get-file-list message path filetype_name filetype_pattern)
         msgbox                      ; (msgbox message)
         populate-listbox            ; (populate-listbox listbox listbox-contents)
         show-error-message          ; (show-error-message message)
         show-loading                ; (show-loading)
         show-confirmation-dialog    ; (show-confirmation-dialog message)
         show-warning-message)       ; (show-warning-message message)
</pre>

## GUI procedures

<pre>
(provide get-bitmap               ; (get-bitmap filename width height)
         is-checked?              ; (is-checked? check-box)
         make-elastic-frame)      ; (make-elastic-frame appname)
</pre>

## Menu-bar procedures

This is intended to be a good copy-paste candidate when one needs a simple, standard-looking menu bar.
<pre>
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
</pre>

## SQL procedures

<pre>
(provide query-execute                                 ; (query-execute db query)
         query-record                                  ; (query-record db query)
         query-string                                  ; (query-string db query)
         get-query-headers                             ; (get-query-headers query)    ('AS' required)
         get-query-headers#                            ; (get-query-headers# query)    ('AS' not required, ignores subqueries)
         get-query-headers*                            ; (get-query-headers* db query)      ('AS' not required, but does not support complex sub-queries)
         get-query-results                             ; (get-query-results db query wildcard-list)
         list->file                                    ; (list->file l file)
         ml->sl                                        ; (ml->sl l)
         sql-ml->sl                                    ; (sql-ml->sl l)
         get-tables                                    ; (get-tables db)
         get-tables*                                   ; (get-tables* db-schema)
         get-table-columns                             ; (get-table-columns db table)
         get-table-columns-and-types                   ; (get-table-columns-and-types db table)
         get-db-schema                                 ; (get-db-schema db tables)
         make-select-query                             ; (make-select-query db table columns)
         table-contains?                               ; (table-contains? db column table)
         which-tables-contain?                         ; (which-tables-contain? db tables column)
         which-tables-contain?*                        ; (which-tables-contain?* db-schema column)
         write-db-schema-to-file                       ; (write-db-schema-to-file db tables file)
         read-db-schema-from-file                      ; (read-db-schema-from-file file)
         update-db-schema                              ; (update-db-schema db db-schema)
         get-tables-that-contain-each-column-in-query) ; (get-tables-that-contain-each-column-in-query db db-schema query)
</pre>

## Hash procedures

<pre>

(provide hash->string-list             ; (hash->string-list h)
         hash->sorted-string-list      ; (hash->sorted-string-list h)
         hash->flat-sorted-string-list ; (hash->flat-sorted-string-list h)
         maybe-hash)                   ; (maybe-hash cond possible-hash)
</pre>

## Clipboard procedures

<pre>
(provide get-clipboard       ; (get-clipboard-text)
         set-clipboard-text) ; (set-clipboard-text s)
</pre>

## CSV-import procedures

See csv-import-example.rkt for a sample implementation of the following module.

<pre>
(provide import-csv) ; (import-csv file processor-func (delimiter #\,))
</pre>

## XML procedures

<pre>
(provide get-xml-value-from-id) ; (get-xml-value-from-id file pattern)
</pre>

## System procedures

<pre>
(provide execute-async  ; (execute-async startup-path program-binary-path command-line-parameters)
         system->ports) ; (system->ports command)
</pre>

## License
Racket Commons is free software; see [LICENSE](https://github.com/DexterLagan/racket-commons/blob/main/LICENSE) for more details.
