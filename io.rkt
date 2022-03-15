#lang racket
(require "commons.rkt")                ; (multi-replace-lines lines source-list destination)
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
(module+ test
  (require rackunit))

;;; purpose

; a library of file I/O functions

;;; version history

; v1.0 - initial release for Overrider
; v1.1 - for TextureFixer
; v1.2 - this version, for frame-copier. Added find-files#, directory-list-str

;;; defs
         
;; find-files replacement
(define (find-files# pred path)
  (define lst (directory-list path))
  (define results (map pred lst))
  (define filenames (mask results lst))
  (define (build filename) (build-path path filename))
  (map build filenames))
; unit test
;(module+ test
;  (define (frame? filename-path)
;    (let ((filename-text (if (path? filename-path)
;                             (filename-path->string filename-path)
;                             filename-path))) ; convert path to string if necessary
;      (or (string-suffix? filename-text ".sgi")
;          (string-suffix? filename-text ".tga")
;          (string-suffix? filename-text ".dpx"))))
;  
;  (find-files# frame? "\\\\zeus\\usadata0\\Proteus\\usadata0\\833T-303\\scene-026\\frames"))

;; returns a string list of filenames from the path
(define (directory-list-str path)
  (map (λ (path) (path->string path))
       (directory-list path #:build? #t)))

;; convert filename path to string if necessary
(define (filename-path->string filename-path)
  (if (path? filename-path) (path-element->string filename-path) filename-path))

;; create a list of files with the same provided content
(define (create-list-of-files filenames content)
  (let ((create-text-file (λ (file) (display-lines-to-file content file
                                                           #:separator #"\n"
                                                           #:mode 'binary
                                                           #:exists 'replace))))
    (map create-text-file filenames)))

;; Runs a file operation only if the first file in the list doesn't exist
(define (run-if-not-exists list-of-files operation)
  (if (not (file-exists? (first list-of-files)))
      (operation)
      (display "Files already exist. Skipping...\n")))

;; Displays a file operation's error count with custom message from its result.
(define (get-error-count result msg)
  (if (null? result) (string-append "No " msg ".")
      (string-append (number->string (length result)) " " msg " with "
                     (number->string (count (λ (r) (equal? r 'error)) result)) " error(s).")))
; unit test
;(check-equal? (get-error-count (process-text-files my-process list-of-files) "files processed")
;              "3 text files processed with 0 error(s).")

;; Displays a file operation's error count with custom message from its result.
(define (display-error-count result msg)
  (if (null? result) (display (string-append "No " msg "."))
      (display
       (string-append (number->string (length result)) " " msg " with "
                      (number->string (count (λ (r) (equal? r 'error)) result)) " error(s)."))))
; unit test
;(display-error-count (process-text-files my-process list-of-files) "files processed")
; --> "X text files processed with 0 error(s)."

;; applies an operation to a list of files, line by line :
;   - load each file as a list;
;   - apply the process to the list;
;   - save the file.
(define (process-text-files process file-list)
  (if (and (procedure? process)
           (non-empty-list? file-list))
      (let ((process (λ (file)
                       (let* ((file-lines (file->lines file #:line-mode 'any #:mode 'binary))
                              (new-lines (map process file-lines)))
                         (begin (make-backup-file file) ; make a backup file before update
                                (display-lines-to-file new-lines file
                                                       #:separator #"\n"
                                                       #:mode 'binary
                                                       #:exists 'replace))))))
        (map process file-list))
      null))
; unit test
;(define (my-process line) (string-replace line "source" "destination"))
;(display-error-count (process-text-files my-process list-of-files) "text files processed")
; --> "3 text files processed with 0 error(s)."

; Predicate that tests of all items in the list are paths of directories and exist
(define (directories-exist? list-of-dirs)
  (if (list? list-of-dirs)
      (if (andmap path? list-of-dirs)
          (andmap directory-exists? list-of-dirs)
          #f)
      #f))

; Writes a backup file in the same folder - supports paths and string-paths
(define (make-backup-file path)
  (if (path-for-some-system? path)
      (copy-file path (string->path (string-append (path->string path) ".old")) #t)
      (copy-file path (string-append path ".old") #t)))

; Add a file extension to a string if string isn't empty
(define (add-file-extension filename extension)
  (if (non-empty-string? filename)
      (string-append filename extension)
      ""))

; Add a file extension to a list of strings if not empty
(define (add-file-extensions filenames extension)
  (if (non-empty-list? filenames)
      (map (curryr string-append extension) filenames)
      null))

; Copy one file to multiple folders
(define (copy-file-to-folders source-path destination-list overwrite?)
  (let ((copy-func (lambda (dest)
                     (begin ; Delete file prior to copy as copy-file doesn't seem to overwrite files
                       (if (and overwrite? (file-exists? dest)) (delete-file dest) null) 
                       (copy-file source-path dest overwrite?))))
        (dest-list (map
                    (curry ; curry fonction to build destination path
                     (lambda (source destination)
                       (build-path destination (last (explode-path source))))
                     source-path)
                    destination-list)))
    (for-each copy-func dest-list)))

; returns the last part of a path (the file name)
(define (get-last-path-part path)
  (if (non-empty-string? path)
      (if (string-contains? path "\\")
          (last (string-split path "\\"))
          (last (string-split path "/")))
      ""))
; unit test
(module+ test
  (check-equal?
   (get-last-path-part "/USA_DB/jobs/833T-201/palette-library/00C_BNTWNB_Baron_Thug_Twin_B.plt")
   "00C_BNTWNB_Baron_Thug_Twin_B.plt")
  (check-equal?
   (get-last-path-part "00C_BNTWNB_Baron_Thug_Twin_B.plt")
   "00C_BNTWNB_Baron_Thug_Twin_B.plt")
  (check-equal?
   (get-last-path-part "")
   ""))

; returns the filename without extension
(define (get-filename filename-w-ext)
  (if (non-empty-string? filename-w-ext)
      (first (string-split filename-w-ext "."))
      ""))
; unit test
(module+ test
  (check-equal? (get-filename "00C_BNTWNB_Baron_Thug_Twin_B.plt") "00C_BNTWNB_Baron_Thug_Twin_B")
  (check-equal? (get-filename "") ""))

; returns a list of filenames without their extension
(define (get-filenames filenames-w-ext)
  (if (list? filenames-w-ext)
      (map get-filename filenames-w-ext)
      null))
; unit test
(module+ test
  (check-equal?
   (get-filenames '("00C_BNTWNB_Baron_Thug_Twin_A.plt" "00C_BNTWNB_Baron_Thug_Twin_B.plt"))
   '("00C_BNTWNB_Baron_Thug_Twin_A" "00C_BNTWNB_Baron_Thug_Twin_B"))
  (check-equal? (get-filenames '()) null))

;; returns filename extension
(define (get-file-extension filename-path)
  (let ((filename-text (filename-path->string filename-path))) ; convert path to string if necessary
    (string-append "." (last (string-split filename-text ".")))))
; unit test
(module+ test
  (check-equal? (get-file-extension "final-0001.cgi") ".cgi"))

;; returns filename extensions from a list of filenames
(define (get-file-extensions filenames)
  (if (non-empty-list? filenames) (map get-file-extension filenames)
      null))
; unit test
(module+ test
  (check-equal? (get-file-extensions (list "final-0001.cgi" "frame-0590.tga")) (list ".cgi" ".tga")))

;; automatically converts a string to path if it's a string, else keeps the path
(define (path! path-or-string)
  (if (path-for-some-system? path-or-string) path-or-string (string->path path-or-string)))

;; returns a list of lines from a file - supports paths and string-paths
(define (get-file-lines path)
  (file->lines (path! path) #:mode 'binary #:line-mode 'any))
; unit test
;(get-file-lines "C:\\TEMP\\_TX-palette-changes\\scene-382\\elements\\VRNSTCU_Staff_2\\PALETTE_LIST")

;; writes a list of file lines to disk - supports paths and string-paths
(define (write-file-lines lines path)
  (display-lines-to-file lines (path! path) #:mode 'binary #:exists 'replace))

;; returns a list of files which match a prefix and an extension from a root path
(define (get-file-list-from-prefix-ext path prefix extension)
  (define (file-pred filepath)
    (let ((filename (path->string (file-name-from-path filepath))))
      (if (and (string-prefix? filename prefix)
               (string-suffix? filename extension)) #t #f)))
  (find-files file-pred path))

; Predicate that tests of all items in the list are paths of directories and exist
(define (directories-exist? l)
  (if (list? l)
      (if (andmap path? l)
          (andmap directory-exists? l)
          #f)
      #f))
  
   
;; replace a filename in a full path with another filename
(define (replace-filename-in-path full-path new-filename)
  (string-append (path->string (path-only full-path)) new-filename))
; unit test
(module+ test
  (check-equal? (replace-filename-in-path (string->path "C:\\test\\path\\some-folder") "package-name")
                "C:\\test\\path\\package-name"
                "get-package-path-string"))
                
           
; EOF
