(import (wak irregex)
        (only (wak htmlprag)
              html->sxml)
        (only (wak ssax parsing)
              ssax:xml->sxml)
        (wak sxml-tools sxpath)
        (wak sxml-tools sxml-tools))

;; Process data from CSUG Summary page -----------------------------------------

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
         ;; bos = beginning of string; colon creates sre 
         [url-expand (irregex-replace '(: bos #\.) url expand-text)])
    (if url-expand url-expand url)))

(define (extract-anchor url)
  (car (reverse (irregex-split #\/ url))))

(define (extract-row-data row)
  (let* ([tds (sxml:content row)]
         [row0 (list-ref tds 0)]
         [form (extract-form (list-ref row0 2))]
         [key (extract-key form)]
         [a-tag (assoc 'a (sxml:content (list-ref tds 2)))]
         [page (list-ref a-tag 2)]
         [source (if (irregex-search "t" page) "TSPL" "CSUG")] 
         [url-raw (cadadr (list-ref a-tag 1))]
         [url (expand-url url-raw)]
         [anchor (extract-anchor url)])
    (list source key anchor url)))

(define summary (map extract-row-data summary-rows))

;; alias appears twice in CSUG as both a procedure and keyword for `import`
;; choosing to drop the keyword version (syntax:s22)
;; maybe append keyword info to end of rest of `alias` docs
(define summary-csug
  (filter (lambda (y) (not (and (string=? (car y) "alias")
                                (string=? (cadr y) "syntax:s22"))))
          (map cdr (filter (lambda (x) (string=? "CSUG" (car x))) summary))))

;; let occurs twice; changing one reference to "named let"
;; won't work as expected for a user in `doc` procedure but will work with `find-proc` 
(define summary-tspl
  (map (lambda (y) (if (and (string=? (car y) "let")
                            (string=? (cadr y) "control:s20"))
                       (cons "named let" (cdr y))
                       y))
       (map cdr (filter (lambda (x) (string=? "TSPL" (car x))) summary))))

(define summary-data (list (cons 'CSUG summary-csug)
                           (cons 'TSPL summary-tspl)))

(with-output-to-file "summary-data.scm"
  (lambda () (write `(define summary-data ',summary-data))))

