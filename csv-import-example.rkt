#lang racket/gui
(require "dialogs.rkt"
         "commons.rkt"
         "csv-import.rkt"
         csv-reading)

(provide partners-import)   ; (import-contact name email company-name company-type buyer-name salesperson-name customer-axis-name customer-type customer-subtype code-site code-site-description street city country-name phone sage-ref siret)

;;; purpose

; import a CSV file containing partners into Odoo (an open-source ERP)

;;; consts

(define *appname*         "Sample CSV Importer")
(define *total-lines*     0)
(define *lines-processed* 0)

;;; defs

;; extract and process a list of CSV line's fields
(define (line-processor l)
  ; extract CSV columns
  (define company-type     (list-ref l 0))  ; 0  Company Type  
  (define sage-ref         (list-ref l 1))  ; 1  Référence 
  (define name             (list-ref l 2))  ; 2  Nom affiché 
  (define street           (list-ref l 3))  ; 3  Rue 
  (define postal-code      (list-ref l 4))  ; 4  Code Postal 
  (define city             (list-ref l 5))  ; 5  Ville 
  (define country          (list-ref l 6))  ; 6  Pays
  (define company-group    (list-ref l 7))  ; 7  Groupe de Sociétés
  (define site-code        (list-ref l 8))  ; 8  Code Site 
  (define site-code-desc   (list-ref l 9))  ; 9  Description code site
  (define tva              (list-ref l 10)) ; 10 N° TVA  
  (define siret            (list-ref l 11)) ; 11 SIRET 
  (define phone            (list-ref l 12)) ; 12 Téléphone 
  (define email            (list-ref l 13)) ; 13 Email 
  (define etiquette        (list-ref l 14)) ; 14 Étiquettes
  (define contact-name     (list-ref l 15)) ; 15 Contact/Nom 
  (define salesperson      (list-ref l 16)) ; 16 Vendeur
  (define buyer            (list-ref l 17)) ; 17 Buyer
  (define fiscal-pos       (list-ref l 18)) ; 18 Position fiscale
  (define price-list       (list-ref l 19)) ; 19 Liste de prix
  (define customer-axis    (list-ref l 20)) ; 20 Customer axis
  (define customer-type    (list-ref l 21)) ; 21 Customer type
  (define customer-subtype (list-ref l 22)) ; 22 Customer Subtype
  (define commercial-team  (list-ref l 23)) ; 23 Équipe Commerciale
  ; create the partner
  (define result (get/create-partner name
                                     email
                                     name
                                     company-type
                                     buyer
                                     salesperson
                                     customer-axis
                                     customer-type
                                     customer-subtype
                                     site-code
                                     site-code-desc
                                     street
                                     city
                                     postal-code
                                     country
                                     phone
                                     sage-ref
                                     siret
                                     tva
                                     company-group
                                     etiquette
                                     contact-name
                                     fiscal-pos
                                     price-list
                                     commercial-team))
  (set! *lines-processed* (add1 *lines-processed*))
  (echo *lines-processed* " out of " *total-lines* " lines processed.")
  result)

;;; main

;; import partners from a CSV file:
;; - check access rights to the target model;
;; - ask file from user;
;; - count file lines;
;; - import file lines;
;; - display result string.
(define (partners-import)
  (let/cc return
    ; check access rights to the partner model
    (unless (check-access-rights *partner* '(create write))
      (show-error-message "No create right on partner model. Aborting.")
      (return #f))
    ; ask user for file
    (define file (get-file))
    (unless file
      (return #t))
    ; count file lines
    (set! *total-lines* (- (length (file->lines file)) 1))
    (when (= *total-lines* 0)
      (show-error-message "Empty file. Aborting."))
    ; process CSV file
    (define results (import-csv file line-processor))
    ; process results
    (if results
        ; display import log
        (void (message-box *appname* (string-join results "\n")))
        (return #f))))
; unit test
;(partners-import)


; EOF
