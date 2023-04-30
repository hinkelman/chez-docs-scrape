(import (wak irregex)
        (only (wak htmlprag)
              html->sxml)
        (only (wak ssax parsing)
              ssax:xml->sxml)
        (wak sxml-tools sxpath)
        (wak sxml-tools sxml-tools))

(include "unique-categories.scm")

(define p-matcher (sxpath '(// html body p)))

(define (flatten x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (flatten (car x))
                      (flatten (cdr x))))))

;; procedure for getting list of files, e.g., (directory-list "html-csug")

;; (define (replace-newline str replacement)
;;   (irregex-replace/all "\n" str replacement))

;; (define (remove-newline str)
;;   (replace-newline str ""))

(define (remove-dotslash str)
  ;; removes "./" at beginning of string
  (let ([out (irregex-replace '(: bos "./") str)])
    ;; if "./" is not present returns #f
    (if out out str)))

;; there is also an extract-anchor procedure in 2.prepare-summary.ss that does something different
;; assuming one anchor for each prl block (see below)
(define (extract-anchor p-elem)
  (let* ([name-matcher (sxpath '(// a ^ name))]
         [name (cadar (name-matcher p-elem))])
    (cons name (remove-dotslash name))))

(define (remove-anchor anchor str)
  (irregex-replace/all anchor str ""))

(define (replace lst)
  ;; replace elements (symbols and strings) of flat lst with strings
  (map (lambda (x)
         ;; dropping newlines because those will be added elsewhere
         (cond [(member x '("<graphic>")) ""]
               [(member x '("formdef")) "\n\n"]
               [(member x '(nbsp)) " "]
               [(member x '("math/csug/0.gif")) "-->"]
               [(symbol? x) ""]
               [else x]))
       lst))

;; words/phrases used for splitting string (see add-splitters)
;; (define split-words (append unique-categories '("returns: " "libraries: ")))

;; (define (add-splitters lst)
;;   ;; lst should only contain strings
;;   ;; using qqq as string to add and subsequently split on)
;;   (map (lambda (x)
;;          (cond [(string=? x "formdef") ""]
;;                [(member x split-words)
;;                 (string-append "qqq" x)]
;;                [else (remove-dotslash x)]))
;;        lst))

(define (process-formdef p-elem)
  (let* ([anchor-pair (extract-anchor p-elem)]
         [str-lst (replace (flatten p-elem))]
         [str (apply string-append (cdr str-lst))])
    (list (cdr anchor-pair) (remove-anchor (car anchor-pair) str))))

;; currently have 3 groups: formdefs, retained other p-elem, and rejected p-elem
;; process-p-elem handles group 2
(define (process-p-elem p-elem)
  (let ([str-lst (replace (flatten p-elem))])
    (list (apply string-append str-lst))))

(define (check-formdef p-elem)
    (member "formdef" (flatten p-elem)))

;; tt tags indicate blocks of example code which are excluded from chez-docs
;; prl blocks also include tt tags so need to check formdef first
;; (define (check-tt p-elem)
;;   (member 'tt (flatten p-elem)))

(define (check-headers p-elem)
  (let* ([flat (flatten p-elem)]
         [hs '(h1 h2 h3 h4 h5 h6)]
         [mask (map (lambda (x) (member x flat)) hs)])
    (> (length (filter (lambda (x) x) mask)) 0)))

(define (check-footer p-elem)
  (member "copyright" (flatten p-elem)))

(define (process-p-list p-list)
  (let loop ([lst p-list]
             [flag 0]
             [tmp '()]
             [final '()])
    (cond [(null? lst)
           (reverse (cons (apply append (reverse tmp)) final))]
           [(and (= flag 0) (check-formdef (car lst)))
            (loop (cdr lst) 1 (cons (process-formdef (car lst)) '()) final)]
           [(and (= flag 1) (check-formdef (car lst)))
            (loop (cdr lst) 1
                  (cons (process-formdef (car lst)) '())
                  (cons (apply append (reverse tmp)) final))]
           [(and (= flag 1) (or (check-headers (car lst)) (check-footer (car lst))))
            (loop (cdr lst) 0 '() (cons (apply append (reverse tmp)) final))]
           [(= flag 1)
            (loop (cdr lst) flag (cons (process-p-elem (car lst)) tmp) final)]
           [else
            (loop (cdr lst) flag tmp final)])))
                        
(define (display-str-list str-list)
  (for-each (lambda (x) (display x)) str-list))             
                   
(define p-list
  (p-matcher (html->sxml (open-input-file "html-csug/test-numeric.html"))))

(define test (process-p-list p-list))
;; for some reason this test ends up with an empty list at the end
;; not trying to fix it now (or ever?) b/c has no effect on lookup process
(display-str-list (cdr (assoc "numeric:s16" test)))

