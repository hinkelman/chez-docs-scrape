;; this code is not very fast, but will be run infrequently

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

;; reading the HTML files produces lots of newlines that aren't displayed on the web
;; replacing all newlines that are part of a string; assumed to be at the end
;; single newline strings, "\n", are replaced in replace procedure below
(define (replace-newlines x)
  (if (and (string? x) (not (string=? x "\n")))
      (irregex-replace/all "\n" x " ")
      x))

(define (remove-dotslash str)
  ;; removes "./" at beginning of string
  (let ([out (irregex-replace '(: bos "./") str)])
    ;; if "./" is not present returns #f
    (if out out str)))

(define (gif? obj)
  ;; check if object is a string that contains ".gif"
  (and (string? obj) (irregex-search "\\.gif" obj)))

(define (check-sym lst ref sub sym)
  ;; check if earlier list elements are sym
  (let ([ref-sub (- ref sub)])
    (if (negative? ref-sub)
        #f
        (let ([x (list-ref lst ref-sub)])
          (and (symbol? x) (symbol=? x sym))))))

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
  (map (lambda (y i)
         ;; using member to avoid explicit type checks, e.g.,
         ;; (and (string? x) (string=? ...))
         (let ([x (replace-newlines y)]) ;; doesn't replace standalone \n
           (cond
            ;; hack to try to replace some of the newlines with a space
            ;; see https://github.com/hinkelman/chez-docs-scrape/issues/8
            [(and (member x '("\n")) (or (check-sym lst i 2 'tt)
                                         (check-sym lst i 3 'tt)))
             " "]
            [(member x '("\n")) ""]
            ;; representing ghostRightarrow with 2 spaces
            [(member x '("gifs/ghostRightarrow.gif")) "  "]
            [(member x '(nbsp)) " "]
            [(member x '(br dt)) "\n"]
            [(member x '(dd)) "\n    "]
            [(member x '(lt)) "<"]
            [(member x '(le)) "<="]
            [(member x '(gt)) ">"]
            [(member x '(ge)) ">="]
            [(member x '(sup)) "^"]
            [(member x '(li)) "\n* "]
            [(member x '(eacute)) (string (integer->char 233))]
            [(member x '(middot)) (string (integer->char 183))]
            [(member x '(szlig)) (string (integer->char 223))]
            ;; hex codes, e.g., &#x130 -> 304, are translated into integers by html->sxml
            [(integer? x) (string (integer->char x))]            
            ;; fragile, manual approach to handling all of these gifs
            ;; not all of these gifs will be picked up as part of chez-docs
            [(member x '("math/csug/0.gif" "math/tspl/0.gif")) "=>"]
            [(member x '("math/csug/2.gif" "math/tspl/8.gif")) "-->"]
            [(member x '("math/csug/4.gif" "math/tspl/9.gif")) "->"]
            [(member x '("math/csug/5.gif")) "min(max(g+1, min-tg), max-tg)"]
            [(member x '("math/tspl/27.gif")) "1/2 x (1 2 3) = (1/2 1 3/2)"]
            [(member x '("math/csug/3.gif" "math/tspl/25.gif"))
             (string (integer->char 955))] ;; lambda
            [(member x '("math/tspl/3.gif"))
             ;; vertical ellipsis; doesn't work on Windows
             (string (integer->char 8942))] 
            [(member x '("math/tspl/13.gif"))
             ;; infinity
             (string (integer->char 8734))]
            [(member x '("math/tspl/20.gif"))
             ;; final sigma
             (string (integer->char 962))]
            [(member x '("math/tspl/21.gif"))
             ;; big sigma
             (string (integer->char 931))]
            [(member x '("math/tspl/22.gif"))
             ;; small sigma
             (string (integer->char 963))]
            [(member x '("math/tspl/11.gif"))
             ;; negative infinity
             (string-append "-" (string (integer->char 8734)))] 
            [(member x '("math/tspl/12.gif"))
             ;; positive infinity
             (string-append "+" (string (integer->char 8734)))] 
            [(member x '("math/tspl/14.gif"))
             ;; negative pi
             (string-append "-" (string (integer->char 960)))]
            [(member x '("math/tspl/15.gif"))
             ;; positive pi
             (string-append "+" (string (integer->char 960)))]
            [(gif? x) "[image not available]"]
            ;; remove any strings preceded by class, align, name, href, alt, src
            ;; important that this cleanup step happens after all replacements above
            [(and (string? x)
                  (member #t (map (lambda (sym) (check-sym lst i 1 sym))
                                  '(class align name href alt src))))
             ""]
            [(or (number? x) (symbol? x)) ""]
            [else x])))
       lst (enumerate lst)))

(define (process-formdef p-elem)
  (let* ([anchor-pair (extract-anchor p-elem)]
         [str-lst (replace (flatten p-elem))]
         [str1 (apply string-append (cdr str-lst))]
         [str2 (if (string=? str1 "") str1 (string-append str1 "\n\n"))])
    (list (cdr anchor-pair) (remove-anchor (car anchor-pair) str2))))

;; process-p-elem handles p-elems that aren't formdefs
(define (process-p-elem p-elem)
  (let* ([p-flat (flatten p-elem)]
         [str1 (if (member 'table p-flat)
                   ;; for simplicity, choosing not to display tables
                   "[table not shown]"
                   (apply string-append (replace p-flat)))]
         [str2 (if (string=? str1 "") str1 (string-append str1 "\n\n"))])
    (list str2)))

(define (check-formdef p-elem)
  (member "formdef" (flatten p-elem)))

(define (check-headers p-elem)
  (let* ([flat (flatten p-elem)]
         [hs '(h1 h2 h3 h4 h5 h6)]
         [mask (map (lambda (x) (member x flat)) hs)])
    (> (length (filter (lambda (x) x) mask)) 0)))

(define (check-footer p-elem)
  (member "Copyright " (flatten p-elem)))

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

(define (get-p-list dir file)
  (p-matcher
   (html->sxml
    (open-input-file (string-append dir (string (directory-separator)) file)))))

(define (process-html-file dir file)
  (process-p-list (get-p-list dir file)))

(define (process-html-dir dir)
  ;; some files contain only text and no documentation used in chez-docs
  ;; filtering out those empty lists
  (filter (lambda (x) (not (null? x)))
          (apply append (map (lambda (file) (process-html-file dir file))
                             (directory-list dir)))))

(let ([file "chez-docs-data.scm"])
  (when (file-exists? file) (delete-file file))
  (let ([data (list (cons 'csug (process-html-dir "html-csug"))
                    (cons 'tspl (process-html-dir "html-tspl")))])
    (with-output-to-file file
      (lambda () (write `(define chez-docs-data ',data))))))



