#lang racket/gui
(require mrlib/path-dialog)
(provide die                         ; (die msg)
         get-directory-list          ; (get-directory-list title msg path)
         get-directory-list-w-prefix ; (get-directory-list-w-prefix title msg path folder_prefix)
         get-single-directory        ; (get-single-directory title msg path)
         hide-loading                ; (hide-loading)
         listbox-dialog              ; (listbox-dialog title message initial-listbox-contents style)
         listbox-dialog#             ; (listbox-dialog# title message headers initial-listbox-contents selection-type width height)
         listbox-selectall           ; (listbox-selectall list-box item-count select?)
         my-get-file-list            ; (my-get-file-list message path filetype_name filetype_pattern)
         msgbox                      ; (msgbox appname message)
         populate-listbox            ; (populate-listbox listbox listbox-contents)
         show-error-message          ; (show-error-message message)
         show-loading                ; (show-loading)
         show-confirmation-dialog    ; (show-confirmation-dialog appname message)
         show-warning-message)       ; (show-warning-message message)
(module+ test
  (require rackunit))

;;; purpose

; a library of useful dialogs

;;; version history

; v1.0 - this version.

;;; defs

;; Generic MsgBox with App name as title
(define (msgbox appname message)
  (message-box appname message #f (list 'ok 'no-icon)))

;; a generic die dialog that quits
(define (die msg)
  (void (message-box "Fatal Error" msg #f (list 'ok 'stop)))
  (exit 1))

;; Generic confirmation dialog
(define (show-confirmation-dialog appname message)
  (if (equal? (message-box appname message #f (list 'yes-no 'caution)) 'yes)
      #t
      #f))

;; a generic loading dialog
(define loading-dialog
  (let* ((new-dialog (new dialog%
                          [label "Loading"]
                          [parent #f]
                          [width 225]
                          [height 80]
                          [min-width 225]
                          [min-height 80]
                          [stretchable-width #f]
                          [stretchable-height #f]))
         (new-panel (new vertical-panel% [parent new-dialog]
                         [alignment '(center center)]))
         (new-message (new message% [parent new-panel]
                           [label "Please wait..."])))
    new-dialog))

; Helper function to show the loading dialog
(define (show-loading)
  (send loading-dialog show-without-yield))

; Helper function to hide the loading dialog
(define (hide-loading)
  (send loading-dialog show #f))

; Generic warning message dialog
(define (show-warning-message message)
  (void (message-box "Warning" message #f (list 'ok 'caution))))

; Generic error message dialog
(define (show-error-message message)
  (void (message-box "Error" message #f (list 'ok 'stop))))

; Ask user to select a list of scenes - could be used to obtain scene list.
; Example: (getfilelist "Please select a list of scenes." "C:\\" "Harmony Scene" "scene-*")
(define (my-get-file-list message path filetype_name filetype_pattern)
  (get-file-list message
                 #f ;parent
                 (string->path path) ;directory
                 #f ;filename
                 #f ;extension
                 null ;style
                 (list (list filetype_name filetype_pattern)) )) ;filters

;; Ask user to select a list of scenes - could be used to obtain scene list.
;; Example:
;; (get-directory-list appname "Please select a list of scenes:" (string-append filesystem "\\" job)
;; "scene-")
(define dir-ok?
  (λ (x) (non-empty-string? x)))

(define (get-single-directory title msg path)
  (begin
    (define pathdialog (new path-dialog%
                            [label title]
                            [message msg]
                            [directory path]
                            [dir? #t]
                            [ok? dir-ok?]
                            [filters null]))
    (send pathdialog run)))

;; Ask user to select a list of scenes - could be used to obtain scene list.
;; Example:
;; (get-directory-list appname "Please select a list of scenes:" (string-append filesystem "\\" job)
;; "scene-")
(define (get-directory-list title msg path)
  (begin
    (define pathdialog (new path-dialog%
                            [label title]
                            [message msg]
                            [directory path]
                            [dir? #t]
                            [multi? #t]
                            [ok? dir-ok?]
                            [filters null]))
    (send pathdialog run)))

;; Ask user to select a list of scenes - could be used to obtain scene list.
;; Example:
;; (get-directory-list appname "Please select a list of scenes:" (string-append filesystem "\\" job)
;; "scene-")
(define (get-directory-list-w-prefix title msg path folder_prefix)
  (begin
    (define pathdialog (new path-dialog%
                            [label title]
                            [message msg]
                            [directory path]
                            [dir? #t]
                            [multi? #t]
                            [filters null]
                            ; [show-dir? (λ (x) (string-prefix? x folder_prefix))]
                            [ok? (λ (x) (string-prefix? x folder_prefix))]
                            ))
    (send pathdialog run)))

; Helper function to select/deselect all items in a list-box
(define (listbox-selectall list-box item-count select?)
  (let ((select-item (lambda (item) (send list-box select item select?))))
    (for ([x (in-range item-count)]) (select-item x))))

; Helper function to populate a list-box control
(define (populate-listbox listbox listbox-contents)
  (send listbox set listbox-contents))

; List-box dialog with 'select all' and a name filter
(define (listbox-dialog title message initial-listbox-contents style)
  (let* ((dialog (new dialog%
                      [label title]
                      [parent #f]
                      [width 320]
                      [height 480]
                      [min-width 320]
                      [min-height 480]))
         ;[stretchable-width #f]
         ;[stretchable-height #f]

         ; Currenr listbox contents
         (current-listbox-contents initial-listbox-contents)
         (current-selected-contents null)
         
         ; Control defs
         (top-panel (new vertical-panel% [parent dialog]
                         [alignment '(center center)]))
         
         (new-message (new message% [parent top-panel]
                           [label message]))
         
         (new-listbox
          (new list-box%
               [label #f]
               [choices initial-listbox-contents]
               [parent top-panel]
               [style (list style)]
               [callback
                (lambda (l e)
                  (let ((event-type (send e get-event-type)))
                    (if (equal? event-type 'list-box-dclick)
                        (let* ((current-listbox-selection (send l get-selections))
                               (selected-contents (map (curry list-ref current-listbox-contents)
                                                       current-listbox-selection)))
                          (begin
                            (set! current-selected-contents selected-contents)
                            (send dialog show #f)))
                        null)))]))
                                       
         (bottom-panel (new horizontal-panel%
                            [parent dialog]
                            [alignment '(center bottom)]
                            [stretchable-height #f]))
         
         (bottom-left-panel (new horizontal-panel%
                                 [parent bottom-panel]
                                 [alignment '(left bottom)]))

         (bottom-right-panel (new horizontal-panel%
                                  [parent bottom-panel]
                                  [alignment '(right bottom)]))
         
         ; Callbacks
         (selectall-button-callback
          (lambda (b e)
            (listbox-selectall new-listbox (length current-listbox-contents) #t)))

         (cancel-button-callback
          (lambda (b e)
            (begin
              (set! current-selected-contents #f)
              (send dialog show #f))))

         (ok-button-callback
          (lambda (b e)
            (let* ((current-listbox-selection (send new-listbox get-selections))
                   (selected-contents (map (curry list-ref current-listbox-contents)
                                           current-listbox-selection)))
              (begin
                (set! current-selected-contents selected-contents)
                (send dialog show #f)))))

         (filter-textfield-callback
          (lambda (t e)
            (let ((new-listbox-contents
                   (filter (curryr string-contains? (send t get-value)) initial-listbox-contents)))
              (begin
                (set! current-listbox-contents new-listbox-contents)
                (populate-listbox new-listbox new-listbox-contents)))))

         ; Buttons defs
         (filter-textfield (new text-field%
                                [label "Filter: "]
                                [parent bottom-left-panel]
                                [callback filter-textfield-callback]))

         (selectall-button (new button%
                                [label "Select All"]
                                [enabled (if (equal? style 'single) #f #t)] ; Enable or disable 
                                [parent bottom-left-panel]                  ; the Select All button 
                                [callback selectall-button-callback]))      ; depending on style
         
         (cancel-button (new button%
                             [label "Cancel"]
                             [parent bottom-right-panel]
                             [callback cancel-button-callback]))
         
         (ok-button (new button%
                         [label "OK"]
                         [parent bottom-right-panel]
                         [callback ok-button-callback])))
    (begin
      (send dialog show #t)
      current-selected-contents))) ; Return list of selected items (null if cancel pressed)

; Unit test
;(listbox-dialog "PaletteCopier"
;                "Please select destination elements:"
;                (list "Naice" "Sweet" "Dude")
;                'multiple)

;; List-box dialog with 'select all', name filter and support for columns
;; Returns list of selected items, null otherwise
;; initial-listbox-contents is a list of columns
(define (listbox-dialog# title message headers initial-listbox-contents selection-type width height)
  (let* ((dialog (new dialog%
                      [label title]
                      [parent #f]
                      [border 10]
                      [spacing 10]
                      [style (list 'resize-border)]
                      [width width]
                      [height height]
                      [min-width width]
                      [min-height height]))
         ;[stretchable-width #f]
         ;[stretchable-height #f]

         ; Currenr listbox contents
         (current-listbox-contents initial-listbox-contents)
         (current-selected-contents null)
         
         ; Control defs
         (top-panel (new vertical-panel% [parent dialog]
                         [alignment '(center center)]))
         
         (new-message (new message% [parent top-panel]
                           [label message]))
         
         (new-listbox (new list-box%
                           [label #f]
                           [columns headers]
                           [choices '()]
                           [parent top-panel]
                           [style (cons selection-type
                                        '(variable-columns
                                          clickable-headers
                                          column-headers
                                          reorderable-headers))]
                           [callback
                            (lambda (l e)
                              (let ((event-type (send e get-event-type)))
                                (if (equal? event-type 'list-box-dclick)
                                    (let* ((current-listbox-selection (send l get-selections))
                                           (selected-contents (map (curry list-ref current-listbox-contents)
                                                                   current-listbox-selection)))
                                      (begin
                                        (set! current-selected-contents selected-contents)
                                        (send dialog show #f)))
                                    null)))]))
                                       
         (bottom-panel
          (new horizontal-panel%
               [parent dialog]
               [alignment '(center bottom)]
               [stretchable-height #f]))
         
         (bottom-left-panel
          (new horizontal-panel%
               [parent bottom-panel]
               [alignment '(left bottom)]))

         (bottom-right-panel
          (new horizontal-panel%
               [parent bottom-panel]
               [alignment '(right bottom)]))
         
         ; Callbacks
         (selectall-button-callback
          (lambda (b e)
            (listbox-selectall new-listbox (length current-listbox-contents) #t)))

         (cancel-button-callback
          (lambda (b e)
            (begin
              (set! current-selected-contents #f)
              (send dialog show #f))))

         (ok-button-callback
          (lambda (b e)
            (let* ((current-listbox-selection (send new-listbox get-selections))
                   (selected-contents
                    (map (curry list-ref current-listbox-contents)
                         current-listbox-selection)))
              (begin
                (set! current-selected-contents selected-contents)
                (send dialog show #f)))))
       
         (filter-textfield-callback
          (lambda (t e)
            ; grab user input in text field
            (define search-str
              (send t get-value))
            ; utility func to find out if a list contains a string anywhere
            (define (str-list-contains? l)
              (ormap (λ (s)
                       (string-contains? s search-str))
                     l))
            ; filter list and re-populate listbox
            (let ((new-listbox-contents
                   (filter str-list-contains? initial-listbox-contents)))
              (begin
                (set! current-listbox-contents new-listbox-contents)
                (populate-listbox# new-listbox new-listbox-contents)))))

         ; Buttons defs
         (filter-textfield
          (new text-field%
               [label "Filter: "]
               [parent bottom-left-panel]
               [callback filter-textfield-callback]))

         (selectall-button
          (new button%
               [label "Select All"]
               [enabled (if (equal? selection-type 'single) #f #t)] ; Enable or disable the Select All button depending on global style
               [parent bottom-left-panel]
               [callback selectall-button-callback]))
         
         (cancel-button
          (new button%
               [label "Cancel"]
               [parent bottom-right-panel]
               [callback cancel-button-callback]))
         
         (ok-button
          (new button%
               [label "OK"]
               [parent bottom-right-panel]
               [callback ok-button-callback])))
    (begin
      (populate-listbox# new-listbox headers initial-listbox-contents)
      (send dialog show #t)
      current-selected-contents)))

; Unit test
;(listbox-dialog# "Some Title"
;                 "Please select stuff:"
;  (list "Field" "Value")
;  (list (list "Naice" "Sweet")
;  (list "Dude" "Naice"))
;  'multiple 480 640)



; EOF
