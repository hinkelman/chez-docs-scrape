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
