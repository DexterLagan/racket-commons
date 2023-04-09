#lang racket
(provide get-xml-value-from-id) ; (get-xml-value-from-id file pattern)

;;; purpose

; to provide a useful collection of functions for handling XML and XML files.

;;; version history

; v1.0 - this version.

;;; defs

;; returns the value of an XML line given its id (harmony format with value before id)
;; returns the username given the contents of a localprefs.xml file
;; <string value="usabatch" id="LAST_LOGIN_NAME"/>
;; sample use:
;; (get-xml-value-from-id "C:\\Users\\user\\Desktop\\test.xml" "SOME_XML_VALUE")
(define (get-xml-value-from-id file pattern)
  ; read file to lines
  (define prefs-lines
    (file->lines file))
  ; helper func to find value line
  (define (login line)
    (string-contains? line pattern))
  ; helper func to extract value alone
  (define (clean line)
    (define parts (string-split line "\""))
    (second parts))
  ; build list of matches and return unique match if found
  (define matches (filter login prefs-lines))
  (if (= (length matches) 1)
      (clean (first matches))
      #f))
      
      
; EOF
