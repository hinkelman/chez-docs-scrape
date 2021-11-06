(import (wak irregex)
        (only (wak htmlprag)
              html->sxml)
        (only (wak ssax parsing)
              ssax:xml->sxml)
        (wak sxml-tools sxpath)
        (wak sxml-tools sxml-tools))

;; Process data in CSUG Summary page -----------------------------------------

;; https://lists.gnu.org/archive/html/guile-user/2012-01/msg00049.html
(define summary-matcher (sxpath '(// html body p table tr)))
;; first row is column headers; second row is horizontal rule
(define summary-rows
  (cddr (summary-matcher (html->sxml (open-input-file "html-csug/summary.html")))))

(define (extract-form tt)
  (apply
   string-append
   (map
    (lambda (x) (cond [(string? x) x]
                      [(equal? x '(& nbsp)) " "]
                      [else (cadr x)]))
    (sxml:content tt))))

(define numeric-matcher (sxpath '(// html body p)))
(define numeric-data
  (numeric-matcher (html->sxml (open-input-file "html-csug/numeric.html"))))


;; text strings may have spaces and line breaks embedded, e.g.,
;; maybe this won't be relevant to other sections of document; this is front matter
;; (dd " represent exact integers in the\n" "fixnum range (see "
;;             (tt "most-negative-fixnum") " and\n"
;;             (tt "most-positive-fixnum") ").\n"
;;             "The length of a string, vector, or fxvector is constrained to be a fixnum.\n"
;;             "\n")
;; the spaces shouldn't be a problem, but the line breaks might show up where I don't want them
;; spaces also show up with `(& nbsp)` and need to be replaced with " "

;; The <dt> tag defines a term/name in a description list.
;; The <dt> tag is used in conjunction with <dl> (defines a description list) and <dd> (describes each term/name).


