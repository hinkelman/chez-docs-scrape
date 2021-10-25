(import (wak irregex)
        (wak htmlprag)
        (wak sxml-tools sxpath)
        (wak sxml-tools sxml-tools))

;; requires curl
(system
 "curl https://cisco.github.io/ChezScheme/csug9.5/summary.html --output summary.html")

;; https://www.rosettacode.org/wiki/Read_entire_file#Scheme
(define html-string
  (with-input-from-file "summary.html"
    (lambda ()
      (list->string (reverse
       (let loop ((char (read-char))
                  (result '()))
         (if (eof-object? char)
             result
             (loop (read-char) (cons char result)))))))))

;; https://lists.gnu.org/archive/html/guile-user/2012-01/msg00049.html
(define matcher (sxpath '(// html body p table tr)))
;; first row is column headers; second row is horizontal rule
(define table-rows (cddr (matcher (html->sxml html-string))))

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

(define test (map extract-row-data table-rows))

;; data is mostly processed
;; at the point of dealing with multiple forms, which might not be relevant with the new approach of scrping full docs
;; may only end up using this for keys and urls




;; (import (chez-stats))

;; ;; cdr to drop column names
;; (define data (list (cons 'csug (cdr (read-delim "R/CSUG.tsv" #\tab)))
;;                    (cons 'tspl (cdr (read-delim "R/TSPL.tsv" #\tab)))))

;; (with-output-to-file "chez-docs-data.scm"
;;   (lambda () (write `(define data ',data))))
