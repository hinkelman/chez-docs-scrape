;; curl will overwite files by default (which is preferred behavior in this case)

(import (chez-docs-scrape)
        (only (wak htmlprag) html->sxml))

(define csug-base "https://cisco.github.io/ChezScheme/csug10.0/")
(download-pages
 csug-base
 "html-csug"
 (crawl-index (string-append csug-base "index.html")))

(define tspl-base "https://scheme.com/tspl4/")
(download-pages
 tspl-base
 "html-tspl"
 (crawl-index (string-append tspl-base "index.html")))

;; first row is column headers; second row is horizontal rule
(define summary-rows
  (cddr (summary-matcher (html->sxml (open-input-file "html-csug/summary.html")))))

(define unique-categories (extract-unique-categories summary-rows))

(define summary (map extract-row-data summary-rows))

;; alias appears twice in CSUG as both a procedure and keyword for `import`
;; choosing to drop the keyword version (syntax:s22)
;; maybe append keyword info to end of rest of `alias` docs
(define summary-csug
  (filter (lambda (y) (not (and (string=? (car y) "alias")
                                (string=? (cadr y) "syntax:s22"))))
          (map cdr (filter (lambda (x) (string=? "csug" (car x))) summary))))

;; let occurs twice; changing one reference to "named let"
(define summary-tspl
  (map (lambda (y) (if (and (string=? (car y) "let")
                            (string=? (cadr y) "control:s20"))
                       (cons "named let" (cdr y))
                       y))
       (map cdr (filter (lambda (x) (string=? "tspl" (car x))) summary))))

(define summary-data (list (cons 'csug summary-csug)
                           (cons 'tspl summary-tspl)))

(let ([file "summary-data.scm"])
  (when (file-exists? file) (delete-file file))
  (with-output-to-file file
    (lambda () (write `(define summary-data ',summary-data)))))

(let ([file "chez-docs-data.scm"])
  (when (file-exists? file) (delete-file file))
  (let ([data (list (cons 'csug (process-html-dir "html-csug"))
                    (cons 'tspl (process-html-dir "html-tspl")))])
    (with-output-to-file file
      (lambda () (write `(define chez-docs-data ',data))))))
