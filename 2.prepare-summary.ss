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

(define (extract-category row)
  (let ([tds (sxml:content row)])
    (cadr (list-ref tds 1))))

(define (extract-unique-categories summary-rows)
  (let ([categories (map extract-category summary-rows)])
    (let loop ([lst categories]
               [out '()])
      (if (null? lst)
          out
          (if (member (car lst) out)
              (loop (cdr lst) out)
              (loop (cdr lst) (cons (car lst) out)))))))

;; unique-categories used in 3.prepare-docs.ss
(let ([file "unique-categories.scm"]
      [uc (extract-unique-categories summary-rows)])
  (when (file-exists? file) (delete-file file))
  (with-output-to-file file
    (lambda () (write `(define unique-categories ',uc)))))
    
(define (extract-row-data row)
  (let* ([tds (sxml:content row)]
         [row0 (list-ref tds 0)]
         [form (extract-form (list-ref row0 2))]
         [key (extract-key form)]
         [a-tag (assoc 'a (sxml:content (list-ref tds 2)))]
         [page (list-ref a-tag 2)]
         [source (if (irregex-search "t" page) "tspl" "csug")] 
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
          (map cdr (filter (lambda (x) (string=? "csug" (car x))) summary))))

;; let occurs twice; changing one reference to "named let"
;; won't work as expected for a user in `doc` procedure but will work with `find-proc` 
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

