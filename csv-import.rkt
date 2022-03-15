#lang racket/base
(require csv-reading
         "pmap.rkt") ; see pmap package from the same author
(provide import-csv) ; (import-csv file processor-func (delimiter #\,))

;;; purpose

; an easy-to-use library to import data from CSV files

;;; version history

; v1.1 - parallelized using pmap
; v1.0 - initial version

;;; defs

;; imports the given CSV file using the provided processor-func and optional, default comma delimiter
;; returns a list of results from the application of the processor-func
(define (import-csv file processor-func (delimiter #\,))
  (let/cc return
    ; if no file, return
    (unless file
      (return #f))
    ; create a CSV parser with Excel's default UTF8 CSV specs
    (define make-partner-csv-reader
      (make-csv-reader-maker
       (list (cons 'separator-chars            (list delimiter))
             (cons 'strip-leading-whitespace?  #t)
             (cons 'strip-trailing-whitespace? #t))))
    ; create a reader for our CSV file
    (define reader
      (make-partner-csv-reader (open-input-file file)))
    ; skip headers
    (reader)
    ; process lines, gather results
    (csv-map processor-func reader)))

;; parallel version of the previous procedure
;; returns a list of results from the application of the processor-func
(define (import-csv! file processor-func (delimiter #\,))
  (let/cc return
    ; if no file, return
    (unless file
      (return #f))
    ; create a CSV parser with Excel's default UTF8 CSV specs
    (define make-partner-csv-reader
      (make-csv-reader-maker
       (list (cons 'separator-chars            (list delimiter))
             (cons 'strip-leading-whitespace?  #t)
             (cons 'strip-trailing-whitespace? #t))))
    ; create a reader for our CSV file
    (define reader
      (make-partner-csv-reader (open-input-file file)))
    ; skip headers
    (reader)
    ; read csv as a list
    (define csv-lines (csv->list reader))
    ; process lines in parallel, gather results
    (pmap processor-func csv-lines)))



; EOF
