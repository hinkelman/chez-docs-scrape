(import (wak irregex)
        (only (wak htmlprag)
              html->sxml)
        (only (wak ssax parsing)
              ssax:xml->sxml)
        (wak sxml-tools sxpath)
        (wak sxml-tools sxml-tools))

(define (flatten x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (flatten (car x))
                      (flatten (cdr x))))))

(define (remove-newline str)
  (irregex-replace/all "\n" str ""))

(define (remove-dotslash str)
  ;; removes "./" at beginning of string
  (let ([out (irregex-replace '(: bos "./") str)])
    ;; if "./" is not present returns #f
    (if out out str)))

(define p-matcher (sxpath '(// html body p)))
(define p-data
  (p-matcher (html->sxml (open-input-file "html-csug/test-numeric.html"))))

(define (replace lst)
  ;; replace elements of lst with strings
  ;; ignoring all tags and just pulling out strings and 'nbsp
  (map (lambda (x)
         ;; dropping newlines because those will be added elsewhere
         (cond [(string? x) (if (string=? x "\n") "" x)]
               [(and (symbol? x) (symbol=? x 'nbsp)) " "]
               [else ""]))
       lst))

(define (add-splitters lst)
  ;; using qqq as string to add and subsequently split on)
  (map (lambda (x)
         (cond [(string=? x "formdef") ""]
               [(member x '("procedure" "returns: " "libraries: "))
                (string-append "qqq" x)]
               [else (remove-dotslash x)]))
       lst))

;; prl below refers to p-tag blocks that contain procedure/returns/libraries info
;; prl elements are nested lists of strings and symbols
(define (process-prl prl)
  (let* ([str-lst (replace (flatten prl))]
         [str (remove-newline (apply string-append (add-splitters str-lst)))])
    (irregex-split "qqq" str)))
;; process-prl currently works correctly for the 3 examples that i've tried

;; TODO
;; (1) process text descriptions that follow prl
;; (2) classify all p-tag elems as either prl, prl-desc, or reject
;; (3) combine the anchor/key (currently car of prl) with prl and prl-desc
;;     for searching with assoc and subsequent display
;; (4) apply the above steps to one CSUG and one TSPL file
         
;; need to have reasonable rule to know that these can be discarded
(list-ref p-data 15) ;; maybe that there wasn't a formdef in the last few list elements
(list-ref p-data 16) ;; h3 tag could be the excluder
(list-ref p-data 17) ;; probably need formdef counter again
;; maybe keep a formdef proximity counter in loop

(define ex-simple (list-ref p-data 18))
(process-prl ex-simple)
(for-each (lambda (x) (display x) (newline)) (cdr (process-prl ex-simple)))

;; can we discard if first tag is tt --> yes
(list-ref p-data 20)

(define ex-multi-ret (list-ref p-data 29))
(for-each (lambda (x) (display x) (newline)) (cdr (process-prl ex-multi-ret)))
(define ex-multi (list-ref p-data 31))
(apply string-append (replace (flatten ex-multi)))
;; (extract-name ex-multi)
(count-string (flatten ex-multi) "returns: ")

(define ex-multi-desc1 (list-ref p-data 32))
(define ex-multi-desc2 (list-ref p-data 33))


