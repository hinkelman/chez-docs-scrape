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

;; key is how procedure/parameter/syntax is looked up
(define (extract-key form)
  (let* ([key0 (car (irregex-split " " form))]
         [key1 (irregex-replace/all '(or #\( #\)) key0 "")])
    (if key1 key1 key0)))

(define (expand-url url)
  (let* ([expand-text "https://cisco.github.io/ChezScheme/csug9.5"]
         [url-expand (irregex-replace '(: bos #\.) url expand-text)])
    (if url-expand url-expand url)))

(define (extract-row-data row)
  (let* ([tds (sxml:content row)]
         [row0 (list-ref tds 0)]
         [form (extract-form (list-ref row0 2))]
         [key (extract-key form)]
         [category (cadr (list-ref tds 1))]
         [a-tag (assoc 'a (sxml:content (list-ref tds 2)))]
         [page (list-ref a-tag 2)]
         [source (if (irregex-search "t" page) "TSPL" "CSUG")] 
         [url-raw (cadadr (list-ref a-tag 1))]
         [url (expand-url url-raw)])
    (list key form category page source url)))

(define summary (map extract-row-data summary-rows))

;; summary data is mostly processed
;; at the point of dealing with multiple forms, which might not be relevant with the new approach of scrping full docs
;; may only end up using this for keys and urls

;; (with-output-to-file "chez-docs-data.scm"
;;   (lambda () (write `(define data ',data))))

(define numeric-matcher (sxpath '(// html body p)))
(define numeric-data
  (numeric-matcher (html->sxml (open-input-file "html-csug/numeric.html"))))
