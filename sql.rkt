#lang racket/gui
(require db)
(require srfi/19)
(require db/util/datetime)
(require "../common/commons.rkt")
(provide query-execute                                 ; (query-execute db query)
         query-record                                  ; (query-record db query)
         query-value                                   ; (query-value db query)
         query-string                                  ; (query-string db query)
         get-query-headers                             ; (get-query-headers query)    ('AS' required)
         get-query-headers#                            ; (get-query-headers# query)    ('AS' not required, ignores subqueries)
         get-query-headers*                            ; (get-query-headers* db query)      ('AS' not required, but does not support complex sub-queries)
         get-query-results                             ; (get-query-results db query wildcard-list)
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
(module+ test
  (require rackunit))

;;; purpose

; to provide a collection of useful procedures for handling SQL connections and queries

;;; notes

; Sample queries :

; Straight execution
;(query-exec nodixianet "create temporary table the_numbers (n integer, d varchar(20))")

; Read more than one row
;(query-rows pgc "select n, d from the_numbers where n % 2 = 0")

; Read just one row
;(query-row pgc "select * from the_numbers where n = 0")

; Read just one column as list
;(query-list pgc "select d from the_numbers order by n")

; Read just one value
;(query-value pgc "select count(*) from the_numbers")

; If query returns zero or more rows, use
;(query-maybe-row pgc "select d from the_numbers where n = 5") --> #f  if row, row if any
;(query-maybe-value pgc "select d from the_numbers where n = 5") --> #f  if value, value if any

; Parametrized queries:
;(query-value pgc "select d from the_numbers where n = $1" 2)
;(query-list pgc "select n from the_numbers where n > $1 and n < $2" 0 3)

;;; defs

; Returns a list of list of tables that contain the specified column
(define (get-tables-that-contain-each-column-in-query db db-schema query)
  (let* ((query-columns (get-query-headers* db query))
         (tables-that-contain-one-of-the-columns (map (curry which-tables-contain?* db-schema) query-columns)))
    tables-that-contain-one-of-the-columns))
; Test
;(get-tables-that-contain-each-column-in-query "SELECT produit_num, produit_idp FROM produit WHERE produit_num = 0")

; Update a db schema by reading it from the database (the associated table list is also refreshed with a restart)
(define (update-db-schema db db-schema)
  (begin
    (displayln "Updating db schema. This can take a while...")
    (set! db-schema (get-db-schema db null))
    (displayln "All done!")))

; Returns a list of table names and their respective column list. If [tables] is null, the table list is read from the database
(define (get-db-schema db tables)
  (let* ((table-list (if (null? tables) (get-tables db) tables)) ; gather table list from db if none provided
         (column-list (map (curry get-table-columns db) table-list)))
    (zip table-list column-list)))

; Writes the full database schema to file. [tables] is optional and the function can gather the table list directly if it is not specified
(define (write-db-schema-to-file db tables file)
  (let* ((table-list (if (null? tables) (get-tables db) tables)) ; gather table list from db if none provided
         (tables-and-columns (get-db-schema db table-list)))
    (list->file tables-and-columns file)))
; Test
;(write-db-schema-to-file db nodixia-tables "nodixianet.schema")

; Reads a database schema from file
(define (read-db-schema-from-file file)
  (with-input-from-file file read))

; Return a list of tables from db
(define (get-tables db)
  (flatten (sql-ml->sl (query-rows db "SHOW TABLES"))))

; Same as get-tables but uses cached db schema for best performance
(define (get-tables* db-schema)
  (if (list? db-schema)
      (first-of-each db-schema)
      null))

; Return a list of columns from a table
(define (get-table-columns db table)
  (first-of-each (sql-ml->sl (query-rows db (format "SHOW COLUMNS FROM ~a" table)))))

; Return a list of columns and types from a table
(define (get-table-columns-and-types db table)
  (sql-ml->sl (query-rows db (format "SHOW COLUMNS FROM ~a" table))))

; Predicate that returns true if the specified column name exists in the specified table 
(define (table-contains? db column table)
  (if (and (non-empty-string? table) (non-empty-string? column))
      (let ((columns (get-table-columns db table)))
        (if (member column columns) #t #f)) #f))
; Test
;(table-contains? db "produit" "produit_num")

; Returns a list of tables which contain the given column name
(define (which-tables-contain? db tables column)
  (if (and (list? tables) (non-empty-string? column))
      (let* ((results (map (curry table-contains? db column) tables))
             (zipped (zip tables results)))
        (first-of-each (filter second-true? zipped)))
      null))
; Test
;(which-tables-contain? db nodixia-tables "produit_num")

; same as which-tables-contain? uses stored db schema for best performance
(define (which-tables-contain?* db-schema column)
  (if (and (list? db-schema) (non-empty-string? column))
      (let* ((tables (first-of-each db-schema))                 ; list of strings.
             (list-of-column-lists (second-of-each db-schema)) ; list of lists.
             (column-in-list? (lambda (c l) (if (member c l) #t #f)))
             (results (map (curry column-in-list? column) list-of-column-lists))
             (zipped (zip tables results)))
        (first-of-each (filter second-true? zipped)))
      null))
; Test
;(which-tables-contain?* nodixia-schema "produit_num")

; Generic query launcher with automatic conversion to list of strings and swap columns and rows for display in a listbox
(define (get-query-results db query wildcard-list)
  (if (not (string? query)) #f
      (swap-columns-and-rows (sql-ml->sl (apply query-rows db query wildcard-list)))))

; Get MySQL table column names for listbox headers - TESTED
(define (get-table-headers db table)
  (if (null? table) #f
      (map (lambda (l) (car l))
           (ml->sl (query-rows db (format "SHOW COLUMNS FROM ~a" table))))))

; Get an SQL query's SELECT items as a string (helper for get-query-headers#)
(define (get-query-params query)
  (string-join (match (string-split query " ")
                 [(list "SELECT" params ... "FROM" tables ...) params]) " "))

; Build a SELECT query from the name of a table and a list of columns
(define (make-select-query db table columns)
  (if (and (non-empty-string? table) (list? columns))
      (let* ((first-id (cond [(null? columns) null]
                             [(equal? columns #f) #f]
                             [else (car columns)]))
             (comma-delimited-ids (cond [(null? columns) "*"]
                                        [(equal? columns #f) null]
                                        [else (string-append (apply string-append (map (curryr string-append ", ") (all-but-last columns))) (last columns))])))
        (cond [(equal? columns #f) #f] ; exit
              [(null? columns) (string-append "SELECT " comma-delimited-ids " FROM " table " LIMIT 50")]                                                                                   ; Return SELECT * type query
              [else (string-append "SELECT " comma-delimited-ids " FROM " table " ORDER BY " first-id " LIMIT 50")])) #f))                                                                 ; Return complete query or false if no table provided

; Utility function to convert SQL query string to list the proper way
(define (sql-query->list query-params)
  (map (curryr string-split " ") (map (curryr string-replace2 "(" " ( " ")" " ) ") (map string-trim (string-split query-params ",")))))

; Same as get-query-headers but with support for arbitrary queries mixing 'AS' operators and straight columns without 'AS', as well as subqueries (which are ignored) - TESTED
(define (get-query-headers# query)
  (let ((param-list (sql-query->list (get-query-params query))))
    (match param-list
      [(list (or (list columnname) (list _ "AS" columnname) (list _ ... "AS" columnname)) ...) columnname])))
; Unit-test
(module+ test
  (check-equal? (get-query-headers# "SELECT d.a AS c1, d.b AS c2, d.c AS c3, c4, c5, (SELECT d FROM other-table WHERE e = 9) AS c6 FROM table d LEFT JOING table2 ON (t.column1 = t2.column2) WHERE e = 1 AND f = 2 LIMIT 100") '("c1" "c2" "c3" "c4" "c5" "c6")))

; Same as get-query-headers# but with support for parsing column names from nested queries lacking AS
(define (get-query-headers## query)
  (let ((param-list (sql-query->list (get-query-params query))))
    (match param-list
      [(list (or
              (list columnname)
              (list _ "AS" columnname)
              (list _ ... "AS" columnname)
              (list "(" "SELECT" columnname ... "FROM" _ ... ")")
              (list "(" "IFNULL" columnname _ ... ")") ; IFNULL(d.lig_nodoc, IF(p.produit_palette > '', (SELECT lig_nodoc FROM mw_doclig WHERE lig_ref = p.produit_palette LIMIT 1), ''))
              (list "(" "IFNULL" _ ... "AS" columnname ")")
              )
             ...) columnname])))
; Unit-test
;(check-equal? (get-query-headers## "SELECT d.a AS c1, IFNULL(c2, IF(c25 > '', (SELECT c26 FROM more-tables WHERE c27 = 2 LIMIT 1), '')), d.c AS c3, c4, c5, (SELECT d FROM other-table WHERE e = 9) AS c6 FROM table d WHERE e = 1 AND f = 2 LIMIT 100") '("c1" "c2" "c3" "c4" "c5" "c6"))
;(get-query-headers## "SELECT d.a AS c1, IFNULL(c2, IF(c25 > '', (SELECT c26 FROM more-tables WHERE c27 = 2 LIMIT 1), '')) AS somecolumn, d.c AS c3, c4, c5, (SELECT d FROM other-table WHERE e = 9) AS c6 FROM table d WHERE e = 1 AND f = 2 LIMIT 100")

; Returns a list of the SQL 'AS' columns - TESTED
; Does NOT support nested 'AS' operators!
(define (get-query-headers q)
  (let* ((q1 (string-split q " AS "))            ; split string at " AS "
         (q2 (map (curryr string-split ",") q1)) ; for each element, split again by ","
         (q3 (map car q2))                       ; make a list of the first items in each comma-delimited list
         (q4 (cdr q3))                           ; save the post-AS to another list
         (q5 (all-but-last q4))                  ; 
         (q6 (last q4))
         (q7 (car (string-split q6 " "))))
    (append q5 (list q7))))
; Unit-test
(module+ test
  (check-equal? (get-query-headers "SELECT d.a AS c1, d.b AS c2, d.c AS c3, (SELECT a FROM b WHERE c) AS c4 FROM table d WHERE e = 1 AND f = 2 LIMIT 100") '("c1" "c2" "c3" "c4")))
;(get-query-headers "SELECT d.a AS c1, IFNULL(c2, IF(c25 > '', (SELECT c26 FROM more-tables WHERE c27 = 2 LIMIT 1), '')) AS somecolumn, d.c AS c3, c4, c5, (SELECT d FROM other-table WHERE e = 9) AS c6 FROM table d WHERE e = 1 AND f = 2 LIMIT 100")

; Same as previous but displays the stuff
(define (get-query-headers-parts q)
  (let* ((q1 (string-split q " AS "))            ; split string at " AS "
         (q2 (map (curryr string-split ",") q1)) ; for each element, split again by ","
         (q3 (map car q2))                       ; make a list of the first items in each comma-delimited list
         (q4 (cdr q3))                           ; save the post-AS to another list
         (q5 (all-but-last q4))                  ; 
         (q6 (last q4))
         (q7 (car (string-split q6 " ")))
         (result (append q5 (list q7))))
    (begin
      (echo "Query: \"" q "\"")
      (echo "q1 - split on AS              : " q1)
      (echo "q2 - map split comma q1       : " q2)
      (echo "q3 - map car q2               : " q3)
      (echo "q4 - rest q3                  : " q4)
      (echo "q5 - all-but-last q4          : " q5)
      (echo "q6 - last q4                  : " q6)
      (echo "q7 - car of split q6 on space : " q7)
      (echo "result (q5 + q7) column names : " result))))
; Unit test
;(get-query-headers-parts "SELECT a AS b, c AS d, e AS f, g AS h FROM table WHERE a = 5")

; Same as get-query-headers but with support for '*' statements but limited to somewhat simple queries with mixed columns and AS's - TESTED
(define (get-query-headers* db query)
  (if (string-prefix? query "SELECT *")
      (let ((table (car (match (string-split query " ") ((list "SELECT" stuff ... "FROM" table ...) table))))) ; Pattern-match * SQL query
        (get-table-headers db table))
      (get-query-headers## query)))

; Convert an sql-date, sql-time or sql-timestamp to human-readable format
(define (sql-date->string date)
  (cond [(sql-date? date) (if (equal? (sql-date-month date) 0) "No date"
                              (date->string (sql-datetime->srfi-date date)))]
        [(sql-timestamp? date) (if (equal? (sql-timestamp-month date) 0) "No date"
                                   (date->string (sql-datetime->srfi-date date)))]
        [(sql-time? date) (date->string (sql-datetime->srfi-date date))]
        [else "Unknown date"]))

; Convert a mixed list to a list of strings
; formerly mixed-list->string-list
(define (ml->sl l)
  (cond [(null? l) null]
        [(equal? l #f) null]
        [(list? (car l)) (cons (ml->sl (car l)) (ml->sl (cdr l)))]
        [(vector? (car l)) (cons (ml->sl (vector->list (car l))) (ml->sl (cdr l)))]
        [else (cons (~a (car l)) (ml->sl (cdr l)))]))

; Convert an sql mixed list to a list of strings
(define (sql-ml->sl l)
  (if (null? l) null
      (let ((f (car l))
            (r (cdr l)))
        (cond [(list?          f) (cons (sql-ml->sl f)                (sql-ml->sl r))]
              [(vector?        f) (cons (sql-ml->sl (vector->list f)) (sql-ml->sl r))]
              [(sql-date?      f) (cons (sql-date->string f)          (sql-ml->sl r))]
              [(sql-time?      f) (cons (sql-date->string f)          (sql-ml->sl r))]
              [(sql-timestamp? f) (cons (sql-date->string f)          (sql-ml->sl r))]
              [else (cons (~a  f)                                     (sql-ml->sl r))]))))

;;; Utility functions

;; executes an SQL query. Returns #t if successful, #f otherwise.
(define (query-execute db query)
  (and (non-empty-string? query)
       (query-exec db query)))

;; executes an SQL query and returns rows, returns #f if zero row returned
(define (query-record db query)
  (and (non-empty-string? query)
       (query-rows db query)))

;; executes an SQL query and returns the value, returns #f if zero row returned
(define (query-value db query)
  (and (non-empty-string? query)
       (query-maybe-value db query)))

;; alias for query-value: executes an SQL query and returns the value, returns #f if zero row returned
(define query-string query-value)


; EOF
