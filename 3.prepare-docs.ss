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

(define p-matcher (sxpath '(// html body p)))
(define p-data
  (p-matcher (html->sxml (open-input-file "html-csug/numeric.html"))))


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

;; pattern is `(p "\n" (a` or `(p (a`
;; for now, the only information that we want (i.e., procedure, returns, libraries)
;; most of those will be contained in the list headed by that `p`
;; but, if there is text following libraries, need to grab that, too
;; `(p "\n" "\n")` or `(p "\n")` might a reliable 'separator before' code examples
;; not current planning to include code examples in doc output
;; resist temptation to complicate this by adding ANSI escape codes; that could come later

;; might be able to ignore "\n"; don't seem to mean #\newline in most of this SXML
;; need to efficiently pull the "\n" out of text blocks (standalone are easy to deal with)

(define ex-formdef
  '(p "\n" (a (^ (name "./numeric:s15")))
      (span
       (^ (class "formdef"))
       (b "procedure")
       ": "
       (tt "(fx=" (& nbsp) (i "fixnum" (sub "1")) (& nbsp)
           (i "fixnum" (sub "2")) (& nbsp) "...)"))
      "\n" (br)
      (span
       (^ (class "formdef"))
       (b "procedure")
       ": "
       (tt "(fx<" (& nbsp) (i "fixnum" (sub "1")) (& nbsp)
           (i "fixnum" (sub "2")) (& nbsp) "...)"))
      "\n" (br)
      (span
       (^ (class "formdef"))
       (b "procedure")
       ": "
       (tt "(fx>" (& nbsp) (i "fixnum" (sub "1")) (& nbsp)
           (i "fixnum" (sub "2")) (& nbsp) "...)"))
      "\n" (br)
      (span
       (^ (class "formdef"))
       (b "procedure")
       ": "
       (tt "(fx<=" (& nbsp) (i "fixnum" (sub "1")) (& nbsp)
           (i "fixnum" (sub "2")) (& nbsp) "...)"))
      "\n" (br)
      (span
       (^ (class "formdef"))
       (b "procedure")
       ": "
       (tt "(fx>=" (& nbsp) (i "fixnum" (sub "1")) (& nbsp)
           (i "fixnum" (sub "2")) (& nbsp) "...)"))
      "\n" (br) "\n" (b "returns: ") (tt "#t")
      " if the relation holds, " (tt "#f") " otherwise\n" (br)
      "\n" (b "libraries: ") (tt "(chezscheme)") "\n" "\n"))

(define ex-desc1
  '(p "The predicate " (tt "fx=") " returns " (tt "#t")
      " if its arguments are equal.\n" "The predicate " (tt "fx<")
      " returns " (tt "#t")
      " if its arguments are monotonically\n"
      "increasing, i.e., each argument is greater than the preceding ones,\n"
      "while " (tt "fx>") " returns " (tt "#t")
      " if its arguments are monotonically decreasing.\n"
      "The predicate " (tt "fx<=") " returns " (tt "#t")
      " if its arguments are monotonically\n"
      "nondecreasing, i.e., each argument is not less than the preceding ones,\n"
      "while " (tt "fx>=") " returns " (tt "#t")
      " if its arguments are monotonically nonincreasing.\n"
      "When passed only one argument, each of these predicates returns "
      (tt "#t") ".\n" "\n"))

(define ex-desc2
  '(p "\n" "These procedures are similar to the Revised"
      (sup "6") " Report procedures\n" (tt "fx=?") ", "
      (tt "fx<?") ", " (tt "fx>?") ", " (tt "fx<=?") ",\n" "and "
      (tt "fx>=?") " except that the Revised" (sup "6")
      " Report procedures\n"
      "require two or more arguments, and their names have the \""
      (tt "?") "\"\n" "suffix.\n" "\n"))


