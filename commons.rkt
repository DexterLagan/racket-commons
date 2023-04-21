#lang racket
(require racket/date)
(provide chop                            ; (chop l len)
         string-chop                     ; (string-chop s len)
         echo                            ; (echo ... ... ...)
         dies                            ; (dies ... ... ...)
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
         string-replace2                 ; (string-replace2 s from1 to1 from2 to2)
         string-replace3                 ; (string-replace3 s from1 to1 from2 to2 from3 to3)
         multi-replace-line              ; (multi-replace-line line source-list destination)
         multi-replace-lines             ; (multi-replace-lines lines source-list destination)
         license-expired?                ; (license-expired? license-year)
         license-almost-expired?         ; (license-almost-expired? license-month)
         list!                           ; (list! v)
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
         get-unique-prefix-line          ; (get-unique-prefix-line lst prefix)
         label->filename                 ; (label->filename label ext)
         str-list-contains               ; (str-list-contains l s)
         str-list-contains?              ; (str-list-contains? l s)
         string-contains-one-of?         ; (string-contains-one-of? s l)
         combine-with                    ; (combine-with f l1 l2)
         pad                             ; (pad l len default)
         pad*                            ; (pad* l default)
         first-of-each                   ; (first-of-each l)
         first-two-of-each               ; (first-two-of-each l)
         rest-of-each                    ; (rest-of-each l)
         second-of-each                  ; (second-of-each l)
         swap-columns-and-rows           ; (swap-columns-and-rows l)
         swap-columns-to-rows-vector     ; (swap-columns-to-rows-vector v)
         all-but-last                    ; (all-but-last l)
         filter-zip                      ; (filter-zip pred-lst lst)
         string->label                   ; (string->label s)
         second?                         ; (second? l)
         second-true?                    ; (second-true? l)
         non-empty-list-of-list?         ; (non-empty-list-of-list? l)
         non-empty-list-of-strings?      ; (non-empty-list-of-strings? l)
         remove-non-alphanumeric-or-underscore ; (remove-non-alphanumeric-or-underscore s)
         take-up-to                      ; (take-up-to n lst)
         group                           ; (group n lst)
         auto-quote                      ; (auto-quote str)
         strip-newlines-returns          ; (strip-newlines-returns str)
         string->list-of-numbers         ; (string->list-of-numbers str)
         remove-indexed-items            ; (remove-indexed-items items indexes)
         media-file?                     ; (media-file? f)
         non-empty-list-of-numbers?      ; (non-empty-list-of-numbers? l)
         get-latest-version-number       ; (get-latest-version-number versions prefix)
         non-empty-or-none-string?       ; (non-empty-or-none-string? s)
         get-current-executable-path     ; (get-current-executable-path)
         transpose)                      ; (transpose l)
         
(module+ test
  (require rackunit))

;;; purpose

; a library of common useful functions

;;; defs

;; returns the current executable's path
;; sample result: C:\Users\baxter\OneDrive\Documents\Projects\Racket\Common\superlative.exe
(define (get-current-executable-path)
  (find-system-path 'run-file))

;; helper function returns #t if given string isn't empty or "None"
  (define (non-empty-or-none-string? s)
    (and (non-empty-string? s)
         (not (string=? s "None"))))
; unit test
(module+ test
  (check-false (non-empty-or-none-string? ""))
  (check-false (non-empty-or-none-string? "None"))
  (check-true  (non-empty-or-none-string? "Something")))

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

;; forces any value as a list
(define (list! v)
  (if (list? v) v
      (list v)))

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

;; returns true if a list is not empty, false otherwise
(define (non-empty-list? l)
  (if (list? l)
      (not (empty? l))
      #f))
; unit test
(module+ test
  (check-false (non-empty-list? #f))
  (check-false (non-empty-list? '()))
  (check-true (non-empty-list? '(1 2)))
  (check-true (non-empty-list? '("a" "b" "c"))))

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
    
;; displays an error message and quit with an error code
(define dies
  (lambda args
    (displayln (apply ~a args))
    (exit 1)))

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

;; returns all the lines, including the one starting with, drop the rest
(define (take-everything-until-including l starts-with)
  (dropf-right l (λ (s) (not (string-prefix? s starts-with)))))

;; returns all the lines that start with the given prefix
(define (take-everything-starts-with l prefix)
  (filter (λ (s) (string-prefix? s prefix)) l))

;; returns all the lines after the line that starts with given prefix
(define (take-everything-after-including l starts-with)
  (dropf l (λ (s) (not (string-prefix? s starts-with)))))

;; returns the matching unique line starting with prefix
;; ensures a single element line exists in the list
;; ensures the prefix exists
;; returns false otherwise
(define (get-unique-prefix-line lst prefix)
  (if (not (list? lst)) #f
      (let ((bin-line (grepl lst prefix)))
        (if (and (list? bin-line)
                 (= (length bin-line) 1))
            (string-replace (first bin-line) prefix "")
            #f))))

;; generate a filename from a title
;; i.e. The Lion Guard --> the-lion-guard
(define (label->filename label ext)
  (string-append
   (string-downcase
    (string-replace label " " "-"))
   ext))

;; grep using a regex
(define (grep lines regex-pattern)
  (filter (λ (line) (regexp-match? regex-pattern line)) lines))

;; grep using a prefix only
(define (grepl lines prefix)
  (filter (λ (line) (string-prefix? line prefix)) lines))

;; transpose a list of lists
;; returns the transposed list of lists if lists are equal lengths, #f otherwise
(define (transpose l)
  (let/cc return
    (unless (and (non-empty-list? l)
                 (non-empty-list? (first l)))
      (return #f))
    ; consider first list as the length to match
    (define standard-length
      (length (first l)))
    (if (andmap (λ (line)
                  (equal? (length line) standard-length))
                l)
        (apply map list l)
        #f)))
; unit test
(module+ test
  (check-equal? (transpose '((1 2 3) (4 5 6)))
                '((1 4) (2 5) (3 6)))
  (check-equal? (transpose '((1 2 3) (4 5 6 7)))
                #f)
  (check-equal? (transpose '("hello" (4 5 6 7)))
                #f))

;; searches a string in a list. Returns #t if found, #f otherwise
(define (str-list-contains? l s)
  (ormap (λ (search)
           (string-contains? search s))
         l))
; unit test
(module+ test
  (check-true (str-list-contains? '("a" "b" "c") "b"))
  (check-false (str-list-contains? '("a" "b" "c") "e")))

;; searches a string in a list. Returns the string if found, #f otherwise
;; same as previous procedure, but returns the line containing the needle instead
(define (str-list-contains l s)
  (define results
    (filter-map (λ (str)
                  (if (string-contains? str s)
                      str
                      #f))
                l))
  (if (non-empty-list? results)
      (first results)
      #f))
; unit test
(module+ test
  (check-equal? (str-list-contains '("a" "b" "c") "b") "b")
  (check-equal? (str-list-contains '("a" "b" "c") "e") #f)
  (check-equal? (str-list-contains '("sweet" "cool stuff" "naice") "cool") "cool stuff"))

;; returns #t if the string contains one of the listed strings, #f otherwise
(define (string-contains-one-of? s l)
  (and (non-empty-list? l)
       (ormap (λ (str)
                (string-contains? s str))
              l)))
; unit test
(module+ test
  (check-true (string-contains-one-of? "b" '("a" "b" "c")))
  (check-false (string-contains-one-of? "e" '("a" "b" "c")))
  (check-true (string-contains-one-of? "souris usb" '("batterie" "adaptateur" "clé" "clavier" "dalle" "souris" "encadrement" "capot" "haut parleur" "carte pci"))))

(define (first-of-each l) ; -> list of atoms!
  (if (null? (car l)) null; make sure there is a first
      (map car l)))
;(first-of-each '((1 2 3) (a b c) ("str1" "str2" "str3")))
;'(1 a "str1")

(define (first-two-of-each l) ; -> list of lists
  (cond [(null? (caar l)) null] ; if the first item of the first list if there
        [(null? (second (car l))) null]; if the second item of the first list
        [else (let ((first-two (lambda (l) (list (car l) (second l)))))
                (map first-two l))]))
;(first-two-of-each '((1 2 3) (a b c) ("str1" "str2" "str3")))
;'((1 2) (a b) ("str1" "str2"))

(define (rest-of-each l) ; -> list of lists!
  (if (null? (car l)) null
      (map cdr l)))
;(rest-of-each '((1 2 3) (a b c) ("str1" "str2" "str3")))
;'((2 3) (b c) ("str2" "str3"))

(define (second-of-each l) ; -> list of atoms!
  (first-of-each
   (rest-of-each l)))
;(second-of-each '((1 2 3) (a b c) ("str1" "str2" "str3")))
;'(2 b "str2")

(define (each-of-each l) ; a.k.a. swap columns and rows
  (if (null? (car (cdr l))) null
        (cons (first-of-each l) (each-of-each (rest-of-each l)))))
;(each-of-each '((1 2 3) (a b c) ("str1" "str2" "str3")))
;'((1 a "str1") (2 b "str2") (3 c "str3"))

; Better version of each-of-each
(define (swap-columns-and-rows l)
  (apply map list l))

; Same as swap-columns-and-rows but works with vectors
(define (swap-columns-to-rows-vector v)
  (apply vector-map vector (vector->list v)))

; Filters a list by another list of booleans
(define (filter-zip pred-list list)
  (if (null? list) null
      (let ((bool (car pred-list))
            (elem (car list)))
        (if bool (cons elem (filter-zip (cdr pred-list) (cdr list)))
            (filter-zip (cdr pred-list) (cdr list))))))

; Pad a list with default. Returns list if length is same or inferior to the list's
(define (pad l len default)
  (if (< (- len (length l)) 1) l
      (append l (make-list (- len (length l)) default))))
; Test
(module+ test
  (check-equal? (pad '(1 2 3 4) 6 0) '(1 2 3 4 0 0)))

; Automatically pad each list of a list to the longest list using default as padding (can be "", 0, null or anything)
(define (pad* l default)
  (let* ((lengths (map length l))
         (max-length (apply max lengths)))
    (map (curryr pad max-length default) l)))
; Test
(module+ test
  (check-equal? (pad* '((1 2 3) (1 2 3 4 5) (1 2 3) (1 2)) 9) '((1 2 3 9 9) (1 2 3 4 5) (1 2 3 9 9) (1 2 9 9 9))))

;; convert a string so it complies to a label-string's requirements
;; (200 chars max string)
(define (string->label s)
  (~a s #:max-width 200 #:limit-marker "..."))
(module+ test
  (check-equal? (string->label "The expense is accounted for when a vendor bill is validated, except in anglo-saxon accounting with perpetual inventory valuation in which case the expense (Cost of Goods Sold account) is recognized at the customer invoice validation.")
                "The expense is accounted for when a vendor bill is validated, except in anglo-saxon accounting with perpetual inventory valuation in which case the expense (Cost of Goods Sold account) is recognize..."))

;; all but last
(define (all-but-last l)
  (reverse (cdr (reverse l))))

; Predicate that returns true if the list has a second element
(define (second? l)
  (if (list? l)
      (if (>= (length l) 2)
          (if (second l) #t #f)
          #f) #f))
; unit test
(module+ test
  (check-equal? (second? '(1)) #f)
  (check-equal? (second? '(1 2)) #t)
  (check-equal? (second? '(1 2 3)) #t))

; Predicate that returns true if the second element of the list is '#true (and nothing else!)
(define (second-true? l)
  (if (second? l)
      (if (equal? (second l) #t) #t #f)
      #f))
; unit test
(module+ test
  (check-equal? (second-true? '(1 anything)) #f)
  (check-equal? (second-true? '(1 #t)) #t))

; Replace two strings, not just one
(define (string-replace2 s from1 to1 from2 to2)
  (string-replace (string-replace s from1 to1) from2 to2))
  
; Replace three strings, not just two
(define (string-replace3 s from1 to1 from2 to2 from3 to3)
  (string-replace (string-replace (string-replace s from1 to1) from2 to2) from3 to3))

;; returns true if l is a non-empty list of list, #f otherwise
(define (non-empty-list-of-list? l)
  (and (non-empty-list? l)
       (non-empty-list? (car l))))
; unit test
(module+ test
  (check-false (non-empty-list-of-list? '("" "")))
  (check-false (non-empty-list-of-list? '("abc" "")))
  (check-false (non-empty-list-of-list? '("" "abc")))
  (check-false (non-empty-list-of-list? '(1 2 3)))
  (check-false (non-empty-list-of-list? '("abc" #f)))
  (check-false (non-empty-list-of-list? '("abc" '("test") #f "abc")))
  (check-false (non-empty-list-of-list? '("abc" 12 "cde")))
  (check-true  (non-empty-list-of-list? '(("test"))))
  (check-true  (non-empty-list-of-list? '(("test" "test2"))))
  (check-true  (non-empty-list-of-list? '(("test" "test2") ("test3" "test4")))))

;; returns true if l is a non-empty list of strings, #f otherwise
(define (non-empty-list-of-strings? l)
  (and (non-empty-list? l)
       (andmap non-empty-string? l)))
; unit test
(module+ test
  (check-false (non-empty-list-of-strings? '("")))
  (check-false (non-empty-list-of-strings? '("abc" "")))
  (check-false (non-empty-list-of-strings? '("" "abc")))
  (check-false (non-empty-list-of-strings? '(1 2 3)))
  (check-false (non-empty-list-of-strings? '("abc" #f)))
  (check-false (non-empty-list-of-strings? '("abc" "def" #f "abc")))
  (check-false (non-empty-list-of-strings? '("abc" 12 "cde")))
  (check-true  (non-empty-list-of-strings? '("abc" "def" "ghf"))))

;; returns #t if a list of numbers, #f otherwise
(define (non-empty-list-of-numbers? l)
  (and (list? l)
       (not (empty? l))
       (andmap number? l)))
; unit test
(module+ test
  (check-false (non-empty-list-of-numbers? #f))
  (check-false (non-empty-list-of-numbers? '()))
  (check-true  (non-empty-list-of-numbers? '(1 2)))
  (check-false (non-empty-list-of-numbers? '("a" "b" "c"))))
  
;; chops a string at the desired length
(define (string-chop s len)
  (and (non-empty-string? s)
       (if (> (string-length s) len)
           (substring s 0 len)
           s)))
;unit test
(module+ test
  (check-equal? (string-chop "12345" 3) "123")
  (check-equal? (string-chop "123" 5) "123")
  (check-equal? (string-chop 12345 3) #f))

;; returns a string with non-alphanumeric characters removed, leaves underscores alone
(define (remove-non-alphanumeric-or-underscore s)
  (define (alphanum? c)
    (or (char-numeric? c)
        (char-alphabetic? c)
        (eq? c #\_)))
  (list->string (filter alphanum? (string->list s))))
(module+ test
  (check-equal? (remove-non-alphanumeric-or-underscore "a0 9-14_*&(") "a0914_"))

;; returns a string with non-alphanumeric characters removed, leaves underscores alone
(define (remove-non-alphanumeric-or-underscore s)
  (define (alphanum? c)
    (or (char-numeric? c)
        (char-alphabetic? c)
        (eq? c #\_)))
  (list->string (filter alphanum? (string->list s))))
(module+ test
  (check-equal? (remove-non-alphanumeric-or-underscore "a0 9-14_*&(") "a0914_"))

;; returns a list of up to n items off the given list lst
(define (take-up-to n lst)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take-up-to (- n 1) (cdr lst)))))
(module+ test
  (check-equal? (take-up-to 0 '("a" "b" "c")) '())
  (check-equal? (take-up-to 2 '("a" "b" "c")) '("a" "b"))
  (check-equal? (take-up-to 1 '("a" "b" "c")) '("a"))
  (check-equal? (take-up-to 4 '("a" "b" "c")) '("a" "b" "c")))

;; split/group a list into n equal chunks
(define (group n lst)
  (define (loop grouped lst l)
    (cond
      [(empty? lst)
       (reverse grouped)]
      [(<= l n)
       (loop (cons lst grouped) '() 0)]
      [else (let-values ([(taken dropped) (split-at lst n)])
              (loop (cons taken grouped)
                    dropped
                    (- l n)))]))
  (let ([l (length lst)])
    (if (>= n l)
        (list lst)
        (loop '() lst l))))
; unit test
(module+ test
  (check-equal? (group 3 '(1 2 3)) '((1 2 3)))
  (check-equal? (group 3 '("sweet" "naice" "cool" "extra")) '(("sweet" "naice" "cool") ("extra")))
  (check-equal? (group 2 '("sweet" "naice" "cool" "extra")) '(("sweet" "naice") ("cool" "extra"))))

;; utility function double-quotes a string if it contains a comma, and doubles double-quotes
(define (auto-quote str)
  (if (string-contains? str ",")
      (string-append "\"" (string-replace str "\"" "\"\"") "\"")
      (string-replace str "\"" "\"\"")))
; unit test
(module+ test
  (check-equal? (auto-quote "no quotes!") "no quotes!")
  (check-equal? (auto-quote "there is a comma in this string, I should quote it")
                "\"there is a comma in this string, I should quote it\"")
  (check-equal? (auto-quote "there is a comma and \"double-quotes\" in this string, I should double-quote it")
                "\"there is a comma and \"\"double-quotes\"\" in this string, I should double-quote it\""))

;; strips both newlines and line feeds
(define (strip-newlines-returns str)
  (string-replace2 str
                   "\n" ""
                   "\r" ""))
                   

;; returns a list of numbers, given a string containing a list of numbers
;; returns #f otherwise
(define (string->list-of-numbers str)
  (let/cc return
    ((comp_ (string-split _ ",")
            (if (or (null? _)
                    (< (length _) 2))
                (return #f)
                (map string-trim _))
            (map string->number _)
            (if (andmap number? _)
                _
                #f))
     str)))
; unit test
(module+ test
  (check-equal? (string->list-of-numbers "0,2") '(0 2))
  (check-equal? (string->list-of-numbers "1, 2, 3") '(1 2 3))
  (check-equal? (string->list-of-numbers "1,") #f)
  (check-equal? (string->list-of-numbers ",2") #f)
  (check-equal? (string->list-of-numbers "hello") #f)
  (check-equal? (string->list-of-numbers "a,b") #f)
  (check-equal? (string->list-of-numbers "1,a") #f)
  (check-equal? (string->list-of-numbers "b,2") #f))
  
;; removes items from a list which are matching the given indexes
(define (remove-indexed-items items indexes)
  (filter (λ (item)
            (not (member (index-of items item)
                         indexes)))
          items))
; unit test
(module+ test
  (check-equal? (remove-indexed-items '(1 2 3 4) '(0 1)) '(3 4))
  (check-equal? (remove-indexed-items '("001" "002" "003" "004") '(1 3)) '("001" "003")))  

;; matches media files, returns #t otherwise.
;; to be used in combination with find-files
(define (media-file? f)
  (let/cc return
    ; ignore directories
    (unless (file-exists? f)
      (return #f))
    ; get extension
    (define ext
      (path-get-extension f))
    ; match media file extensions
    (or (eq? ext #".tga")
        (eq? ext #".png")
        (eq? ext #".sgi")
        (eq? ext #".exr")
        (eq? ext #".mov")
        (eq? ext #".jpg")
        (eq? ext #".jpeg")
        (eq? ext #".psd")
        (eq? ext #".psb")
        (eq? ext #".gif")
        (eq? ext #".mp4")
        (eq? ext #".avi"))))

;; determines the last version number, given a list of versions and the expected prefix
;; sample version list: '("SOMEPREFIX_SOMETHING_V1" "SOMEPREFIX_SOMETHING_V2" "SOMEPREFIX_SOMETHING_V3" "OTHERPREFIX_SOMETHINGELSE_V1")
;; sample prefix: "SOMEPREFIX_SOMETHING_V"
(define/contract (get-latest-version-number versions prefix)
  (non-empty-list? string? . -> . number?)
  (let/cc return
    ; if stump not found in the list of versions, return 0
    (unless (str-list-contains? versions prefix)
      (return 0))
    ; grab only the version names containing the target stump
    (define target-versions
      (filter (curryr string-contains? prefix)
              versions))
    ; remove stump from object-version-names
    (define version-number-strings?
      (map (curryr string-replace prefix "")
           target-versions))
    ; convert to a list of numbers
    (define version-numbers?
      (if (non-empty-list-of-strings? version-number-strings?)
          (map string->number version-number-strings?)
          '(0)))
    ; sort list of resulting numbers
    (if (non-empty-list-of-numbers? version-numbers?)
        (car (sort version-numbers? >))
        0)))
; unit test
(module+ test
  (check-equal? (get-latest-version-number '("GOODSTUMP_V1" "GOODSTUMP_V2" "GOODSTUMP_V3" "BADSTUMP_V1") "GOODSTUMP_V")
                3)
  (check-equal? (get-latest-version-number '("GOODSTUMP_V1" "GOODSTUMP_V2" "GOODSTUMP_V3" "BADSTUMP_V1") "BADSTUMP_V")
                1)
  (check-equal? (get-latest-version-number '("GOODSTUMP_V1" "GOODSTUMP_V2" "GOODSTUMP_V3" "BADSTUMP_V1") "OTHERSTUMP_V")
                0))


; EOF
