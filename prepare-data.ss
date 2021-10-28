(import (wak irregex)
        (only (wak htmlprag)
              html->sxml)
        (only (wak ssax parsing)
              ssax:xml->sxml)
        (wak sxml-tools sxpath)
        (wak sxml-tools sxml-tools))

;; Get HTML files for scraping ---------------------------------------------

;; CSUG and TSPL did not have sitemap.xml files
;; (at least not by appending sitemap.xml at end of URL)
;; used this site to generate sitemaps: https://www.xml-sitemaps.com/

;; fragile, position-based approach
;; vulnerable to change in shape of sitemap.xml
(define (sitemap->urlset path)
  (sxml:content
   (list-ref
    (ssax:xml->sxml (open-input-file path) '())
    2)))

(define (url-list->url url-list)
  (cadr (list-ref url-list 1)))

(define (extract-pagename url)
  (car (reverse (irregex-split #\/ url))))

;; shelling out with `system` calls; requires curl; almost certainly a better way
(define (download-pages base-url output-folder pagenames)
  (for-each
   (lambda (page)
     (system
      (string-append "curl " base-url page " --output " output-folder "/" page)))
   pagenames))

(define urlset-csug (sitemap->urlset "sitemap-csug.xml"))
(define url-csug (map url-list->url urlset-csug))
(define base-url-csug (car url-csug))
(define pagenames-csug (map extract-pagename (cdr url-csug)))
(download-pages base-url-csug "html-csug" pagenames-csug)

(define urlset-tspl (sitemap->urlset "sitemap-tspl.xml"))
(define url-tspl (map url-list->url urlset-tspl))
(define base-url-tspl (car url-tspl))
(define pagenames-tspl (map extract-pagename (cdr url-tspl)))
(download-pages base-url-tspl "html-tspl" pagenames-tspl)

;; Process data in CSUG Summary page -----------------------------------------

;; https://lists.gnu.org/archive/html/guile-user/2012-01/msg00049.html
(define matcher (sxpath '(// html body p table tr)))
;; first row is column headers; second row is horizontal rule
(define table-rows
  (cddr (matcher (html->sxml (open-input-file "html-csug/summary.html")))))

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

;; summary data is mostly processed
;; at the point of dealing with multiple forms, which might not be relevant with the new approach of scrping full docs
;; may only end up using this for keys and urls




;; (import (chez-stats))

;; ;; cdr to drop column names
;; (define data (list (cons 'csug (cdr (read-delim "R/CSUG.tsv" #\tab)))
;;                    (cons 'tspl (cdr (read-delim "R/TSPL.tsv" #\tab)))))

;; (with-output-to-file "chez-docs-data.scm"
;;   (lambda () (write `(define data ',data))))
