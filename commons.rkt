#lang racket
(require racket/date)
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

(module+ test
  (require rackunit))

;;; purpose

; a library of common useful functions

;;; version history

; 1.0 - initial release for other projects
; 1.1 - added chop, list-of-one?, get-matching-seconds
; 1.2 - added string-nth, mask, mask-not and replace-filename-in-path,
;       write-log and the if-defined macro

;;; defs

;; macro executes the second form if symbol is defined, the third if not
;; i.e. (if-defined some-symbol (display "defined") (display "not defined"))
(define-syntax (if-defined stx)
  (syntax-case stx ()
    [(_ id iftrue iffalse)
     (let ([where (identifier-binding #'id)])
       (if where #'iftrue #'iffalse))]))

;; writes an entry in a log file
;; the file is created if it doesn't exist, updated otherwise
;; i.e. (write-log "hello, " world ", you're beautiful")
(if-defined *log-file* (void) (define *log-file* "c:\\temp\\csv-exporter.log"))
(define write-log
  (λ args
    (display-lines-to-file (list (apply ~a args))
                           *log-file*
                           #:separator "\r\n"
                           #:exists 'append)))

;; replace a filename in a full path with another filename
(define (replace-filename-in-path full-path new-filename)
  (string-append (path->string (path-only full-path)) new-filename))
; unit test
(module+ test
  (check-equal? (replace-filename-in-path (string->path "C:\\test\\path\\some-folder") "package-name")
                "C:\\test\\path\\package-name"
                "get-package-path-string"))

;; returns the nth part of a string split on the given separator (whitespaces by default)
(define (string-nth str nth [sep #px"\\s+"])
  (if (non-empty-string? str)
      (let ((parts (string-split str sep)))
        (if (>= (length parts) nth)
            (list-ref parts (- nth 1))
            #f))
      #f))
; unit test
(module+ test
  (check-equal? (string-nth "hello beautiful world" 2)
                "beautiful"
                "string-nth")
  (check-equal? (string-nth "hello beautiful world" 4)
                #f
                "string-nth")
  (check-equal? (string-nth "hello_beautiful_world" 3 "_")
                "world"
                "string-nth"))


;; mask l1 onto l2
(define (mask l1 l2)
  (if (= (length l1) (length l2))
      (let ((results (map (λ (a b) (if a b #f)) l1 l2)))
        (filter identity results))
      null))
; unit test
(module+ test
  (check-equal? (mask '(#t #t #f #f #t)
                      '(1 2 3 4 5))
                '(1 2 5)
                "mask"))

;; reverse mask l1 onto l2
(define (mask-not l1 l2)
  (if (= (length l1) (length l2))
      (let ((results (map (λ (a b) (if a #f b)) l1 l2)))
        (filter identity results))
      null))
; unit test
(module+ test
  (check-equal? (mask-not '(#t #t #f #f #t)
                              '(1 2 3 4 5))
                '(3 4)
                "reverse-mask"))

;; returns a chopped list of n lists
(define (chop l n)
  (if (null? l) null
      (cons (take l n) (chop (drop l n) n))))

;; returns a list of matches (second list item) given one first list item
;; in a list of lists
(define (get-matching-seconds lst key)
  (if (and (non-empty-list? lst)
           (non-empty-string? key))
      (filter identity ; anything not false
              (map (λ (element)
                     (if (string=? key (first element))
                         (second element)
                         #f))
                   lst))
      #f))
; unit test
(module+ test
  (check-equal? (get-matching-seconds '(("Varian_Nose_TX" "some-path")
                                        ("XAVIER_Cheek_TX" 'some-other-thing)
                                        ("XAVIER_Nose_TX" "yet-another-path"))
                                      "XAVIER_Cheek_TX")
                '('some-other-thing))
  (check-equal? (get-matching-seconds '(("key"       "first-string")
                                        ("not-key"   "more-string")
                                        ("key"       "third-string")
                                        ("other-key" "last-string"))
                                      "key")
                '("first-string" "third-string"))
  (check-equal? (get-matching-seconds null "key") #f)
  (check-equal? (get-matching-seconds '(("key"       "first-string")
                                        ("not-key"   "more-string")
                                        ("key"       "third-string")
                                        ("other-key" "last-string"))
                                      "") #f))

;; predicate returns true if list contains one element
(define (list-of-one? l)
  (and (list? l)
       (= 1 (length l))))
; unit test
(module+ test
  (check-equal? (list-of-one? '()) #f)
  (check-equal? (list-of-one? '(1)) #t)
  (check-equal? (list-of-one? '(1 2)) #f)
  (check-equal? (list-of-one? 'test) #f))

;; predicate returns true if list is non-empty
(define (non-empty-list? l)
  (and (list? l)
       (not (null? l))))

;; predicate returns true if all lists given are non-empty and of equal length
(define non-empty-same-length?
  (λ args (and (andmap non-empty-list? args)
               (apply = (map length args)))))

;; Check that license is valid for a given license year limit
(define (license-expired? license-year)
  (let ((this-year (date-year (current-date))))
    (if (<= this-year license-year) #f #t)))

;; Check that license is valid for a given license year limit
(define (license-almost-expired? license-month)
  (let ((this-month (date-month (current-date))))
    (if (>= this-month license-month) #t #f)))

;; Curries a function that takes 3 parameters by its middle parameter
(define (currym func param1 param3)
  (lambda (param2)
    (func param1 param2 param3)))
;; unit test
(module+ test
  (check-equal? ((currym ~a 'a 'c) 'b) "abc"))

;; Macro that curries a function by replacing the referred parameter
;; by the single parameter provided to the curried function
(define-syntax curry-ref
  (syntax-rules ()
    ((curry-ref id params ref)
     (lambda args
       (apply id (list-set params ref (car args)))))))

;; macro that defines whichever parameters are fed to it and fills them in from command line
(define-syntax define-command-line-params
  (syntax-rules ()
    ((define-command-line-params appname param1 ...)
     (define-values (param1 ...)
       (command-line #:program appname
                     #:args (param1 ...)
                     (values param1 ...))))))

;; Fancy varargs println
(define echo
  (lambda args
    (displayln (apply ~a args))))

;; General list printing
(define (print-list l)
  (for-each displayln l))

;; Concatenate all strings in a list to a single string with line feeds
(define (list-to-string-lines l)
  (if (non-empty-list? l)
      (let* ((parts (map (curryr string-append "\n") l)))
        (apply string-append parts))
      ""))
; unit test
(module+ test
  (check-equal? (list-to-string-lines '("Sweet" "Jesus")) "Sweet\nJesus\n"))

;; Shortcut for string-append
(define ++ string-append)

;; Like prefix but one letter
(define (first-letter s)
  (string-ref s 0))

;; Name says it all
(define (second-last l)
  (second (reverse l)))

;; Zip two lists
(define zip
  (lambda (l1 l2) (map list l1 l2)))

;; Find the first orrurence of source-list in str and
;; return str with this occurence replaced by destination
(define (string-replace-list source pattern-list destination)
  (if (and (non-empty-string? source)
           (list? pattern-list)
           (not (null? pattern-list))
           (string? destination))
      (if (string-contains? source (car pattern-list))           ; if pattern is found in source
          (string-replace source (car pattern-list) destination) ; replace pattern and return result
          (string-replace-list source (rest pattern-list) destination)) ; else call proc recursively
      source)) ; else return original string
; unit test
(module+ test
  (check-equal? (string-replace-list
                 "sweet jesus lord"
                 '("sweet" "lord")
                 "awesome")
                "awesome jesus lord"))

;; Replace a list of strings by a single destination in a line
(define (multi-replace-line line source-list destination)
  (if (and (non-empty-string? line)
           (list? source-list)
           (non-empty-string? destination))
      (curryr string-replace-list line source-list destination)
      null))
; unit test
(module+ test
  (check-equal?
   (multi-replace-line "<color palette=\"/USA_DB/jobs/833T-201/palette-library/test1.plt\">"
                       '("test1.plt" "test2.plt")
                       "test4.plt")
   "<color palette=\"/USA_DB/jobs/833T-201/palette-library/test4.plt\">"))

;; Replace a list of strings by a single destination in a list of lines
(define (multi-replace-lines lines source-list destination)
  (if (and (list? lines)
           (list? source-list)
           (non-empty-string? destination))
      (map (curryr string-replace-list source-list destination) lines)
      null))
; unit test
(module+ test
  (check-equal?
   (multi-replace-lines '("<color palette=\"/USA_DB/jobs/833T-201/palette-library/test1.plt\">"
                          "<color palette=\"/USA_DB/jobs/833T-201/palette-library/test2.plt\">"
                          "<color palette=\"/USA_DB/jobs/833T-201/palette-library/test3.plt\">")
                        '("test1.plt" "test2.plt")
                        "test4.plt")
   '("<color palette=\"/USA_DB/jobs/833T-201/palette-library/test4.plt\">"
     "<color palette=\"/USA_DB/jobs/833T-201/palette-library/test4.plt\">"
     "<color palette=\"/USA_DB/jobs/833T-201/palette-library/test3.plt\">")))

;; combine two lists using the provided function as glue
(define (combine-with f l1 l2)
  (let ((combiner (lambda (a1 a2 acc) (cons (f a1 a2) acc))))
    (foldr combiner '() l1 l2)))
; unit test
(module+ test
  (check-equal? (combine-with string-append '("a" "b" "c") '("d" "e" "f")) '("ad" "be" "cf")))


; EOF