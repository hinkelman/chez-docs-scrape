(library (chez-docs-scrape)
  (export
   ;; index and download
   download-pages
   crawl-index
   strip-dotslash
   ;; summary page
   summary-matcher
   extract-row-data
   expand-url
   extract-form
   extract-key
   extract-unique-categories
   ;; documentation pages
   process-html-dir
   process-html-file
   get-p-list
   read-and-clean
   process-p-list
   process-p-elem
   extract-p-anchor
   check-formdef
   check-headers
   check-footer
   flatten
   normalize-children
   render-sxml
   render-children
   render-entity
   render-gif
   attr-value
   normalize-children
   strip-embedded-newlines
   collapse-br-clusters
   drop-leading-noise
   drop-trailing-noise
   normalize-inter-sibling-whitespace
   br-node?
   empty-string?
   drop-while)

  (import (chezscheme)
          (wak irregex)
          (wak sxml-tools sxml-tools)
          (wak sxml-tools sxpath)
          (only (wak htmlprag)
                html->sxml))

  ;; Index TSPL and CSUG pages and download pages -------------------------------

  ;; shelling out with system calls; requires curl
  (define (download-pages base-url output-folder pagenames)
    (unless (file-exists? output-folder) (mkdir output-folder))
    (for-each
     (lambda (page)
       (sleep (make-time 'time-duration 0 1))
       (system
        (string-append "curl -sS --fail " base-url page " --output " output-folder "/" page)))
     pagenames))

  (define (crawl-index index-url)
    ;; download index, parse, return list of unique page filenames
    (let* ([tmp "index.html"])
      (system (string-append "curl -sSL " index-url " --output " tmp))
      (let* ([sxml  (html->sxml (open-input-file tmp))]
             ;; href-matcher returns of list of lists
             ;; each sublist is of the form (href "./foreign.html#./foreign:h1")
             [raw   (map cadr (href-matcher sxml))]
             [hrefs (map (lambda (h) (strip-dotslash (strip-fragment h))) raw)]
             [pages (filter local-html? hrefs)])
        (delete-file tmp)
        (remove-duplicates pages))))
  
  (define href-matcher (sxpath '(// a ^ href)))

  (define (strip-dotslash str)
    ;; removes "./" at beginning of string
    (let ([out (irregex-replace '(: bos "./") str)])
      ;; if "./" is not present returns #f
      (if out out str)))

  (define (strip-fragment href)
    (car (irregex-split #\# href)))

  ;; used to remove hrefs like "canned/copyright.html" or
  ;; "http://www.apache.org/licences/LICENSE-2.0"
  (define (local-html? href)
    (and (not (irregex-search "^(http|mailto|#)" href))
         (not (irregex-search "/" href))
         (irregex-search "\\.html$" href)))

  ;; https://stackoverflow.com/questions/8382296/scheme-remove-duplicated-numbers-from-list
  (define (remove-duplicates ls)
    (cond [(null? ls)
           '()]
          [(member (car ls) (cdr ls))
           (remove-duplicates (cdr ls))]
          [else
           (cons (car ls) (remove-duplicates (cdr ls)))]))

  ;; Process data from CSUG Summary page -----------------------------------------

  ;; https://lists.gnu.org/archive/html/guile-user/2012-01/msg00049.html
  (define summary-matcher (sxpath '(// html body p table tr)))

  (define (extract-row-data row)
    (let* ([tds (sxml:content row)]
           [row0 (list-ref tds 0)]
           [form (extract-form (list-ref row0 2))]
           [key (extract-key form)]
           [a-tag (assoc 'a (sxml:content (list-ref tds 2)))]
           [page (list-ref a-tag 2)]
           [url-raw (cadadr (list-ref a-tag 1))]
           [url (expand-url url-raw)]
           [source (if (irregex-search "/tspl4/" url-raw) "tspl" "csug")]
           [anchor (extract-anchor url)])
      (list source key anchor url)))

  (define (extract-form tt)
    (apply
     string-append
     (map
      (lambda (x)
        (cond [(string? x) x]
              [(equal? x '(& nbsp)) " "]
              [else (cadr x)]))
      (sxml:content tt))))

  ;; key is how procedure/parameter/syntax is looked up
  (define (extract-key form)
    (let* ([key0 (car (irregex-split " " form))]
           [key1 (irregex-replace/all '(or #\( #\)) key0 "")])
      (if key1 key1 key0)))

  (define (expand-url url)
    (let* ([expand-text "https://cisco.github.io/ChezScheme/csug10.0"]
           ;; bos = beginning of string; colon creates sre 
           [url-expand (irregex-replace '(: bos #\.) url expand-text)])
      (if url-expand url-expand url)))

  (define (extract-anchor url)
    (car (reverse (irregex-split #\/ url))))

  (define (extract-unique-categories summary-rows)
    (let ([categories (map extract-category summary-rows)])
      (let loop ([lst categories]
                 [out '()])
        (if (null? lst)
            out
            (if (member (car lst) out)
                (loop (cdr lst) out)
                (loop (cdr lst) (cons (car lst) out)))))))

  (define (extract-category row)
    (let ([tds (sxml:content row)])
      (cadr (list-ref tds 1))))

  ;; Extract and format documentation for use in chez-docs -------------------------

  (define (process-html-dir dir)
    ;; some files contain only text and no documentation used in chez-docs
    ;; filtering out those empty lists
    (filter (lambda (x) (not (null? x)))
            (apply append (map (lambda (file) (process-html-file dir file))
                               (sort string<? (directory-list dir))))))

  (define (process-html-file dir file)
    (process-p-list (get-p-list dir file)))

  (define (get-p-list dir file)
    (p-matcher
     (html->sxml
      (read-and-clean
       (string-append dir (string (directory-separator)) file)))))

  (define p-matcher (sxpath '(// html body p)))

  ;; a couple of files have stray <p> tags that need to be removed
  ;; because html->sxml doesn't handle them well.
  ;; additionally, some dl/dt/dd blocks in the source HTML omit the closing
  ;; </dt> before <dd>, causing htmlprag to nest <dd> inside <dt> rather than
  ;; treating them as siblings. the two-pass fix below inserts the missing
  ;; </dt>, then cleans up any </dt></dt> double-close that results from the
  ;; well-formed cases that already had an explicit </dt>.
  (define (read-and-clean path)
    (let* ([raw     (call-with-input-file path get-string-all)]
           [step1   (irregex-replace/all "<p>[ \t\r\n]*(?=<(p|li|ul)>)" raw "")]
           [step2   (irregex-replace/all "<dd>" step1 "</dt><dd>")]
           [step3   (irregex-replace/all "</dt></dt>" step2 "</dt>")])
      (open-string-input-port step3)))
  
  ;; process-p-list goes through all elements in a p-list and
  ;; throws away elements that won't be served up as part of chez-docs
  ;; creates sublists where the first element is the p-anchor (e.g., objects:s0) that can be looked up with assoc
  ;; combines all the elements that are associated with a formdef into one sublist for display
  (define (process-p-list p-list)
    (let loop ([lst p-list]
               [flag 0]      ;; indicates if content in p-elem is part of a formdef
               [tmp '()]     ;; accumulate components of a formdef into a tmp list
               [final '()]) 
      (cond [(null? lst)
             (reverse final)]
            [(and (= flag 0) (check-formdef (car lst)))
             ;; start of new formdef
             ;; start new tmp list of lists
             (loop (cdr lst) 1 (list (process-p-elem (car lst) #t)) final)]
            [(and (= flag 1) (check-formdef (car lst)))
             ;; last p-elem was a formdef (= flag 1)
             ;; and current p-elem is a formdef
             ;; start a new tmp list of lists
             ;; cons the previous tmp list to final
             ;; and keep flag as one to indicate still in a formdef (but a new one)
             (loop (cdr lst)
                   1
                   (list (process-p-elem (car lst) #t))
                   (cons (apply append (reverse tmp)) final))]
            [(and (= flag 1) (or (check-headers (car lst)) (check-footer (car lst))))
             ;; if last p-elem was part of a formdef and then encounter header or footer
             ;; add tmp to final and set flag to 0 and set tmp to empty list
             (loop (cdr lst) 0 '() (cons (apply append (reverse tmp)) final))]
            [(= flag 1)
             ;; continue to collect p-elems that are part of a formdef in tmp
             (loop (cdr lst) flag (cons (process-p-elem (car lst) #f) tmp) final)]
            [else
             ;; skip over any elements that aren't being retained for display
             (loop (cdr lst) flag tmp final)])))

  (define (check-formdef p-elem)
    (member "formdef" (flatten p-elem)))

  (define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

  (define (check-headers p-elem)
    (let* ([flat (flatten p-elem)]
           [hs '(h1 h2 h3 h4 h5 h6)]
           [mask (map (lambda (x) (member x flat)) hs)])
      (> (length (filter (lambda (x) x) mask)) 0)))

  (define (check-footer p-elem)
    (member "Copyright " (flatten p-elem)))

  (define (process-p-elem p-elem formdef?)
    (let* ([str1 (render-sxml p-elem)]
           [str2 (if (string=? str1 "") str1 (string-append str1 "\n\n"))])
      (if formdef?
          (list (extract-p-anchor p-elem) str2)
          (list str2))))

  (define (extract-p-anchor p-elem)
    (let* ([name-matcher (sxpath '(// a ^ name))]
           [name (cadar (name-matcher p-elem))])
      (strip-dotslash name)))

  (define (render-sxml node)
    (cond
     [(symbol? node) (render-entity node)]
     ;; hex codes, e.g., &#x130 -> 304, are translated into integers by html->sxml
     [(number? node) (string (integer->char node))]
     [(pair? node)
      (let ([tag (car node)])
        (cond
         [(symbol=? tag '&)
          (render-entity (cadr node))]
         [(symbol=? tag '^) ""]  ;; attribute subtree — skip
         [(symbol=? tag 'table) "[table not shown]"]
         [(symbol=? tag 'br) "\n"]
         [(symbol=? tag 'ul)   ;; unordered list
          (string-append "\n" (render-children (normalize-children (sxml:content node))) "\n")]
         [(symbol=? tag 'li)   ;; list element
          (string-append "\n* " (render-children (normalize-children (sxml:content node))))]
         [(symbol=? tag 'dt)   ;; definition term
          (string-append "\n" (render-children (sxml:content node)))]
         [(symbol=? tag 'dd)   ;; definition detail
          (string-append "\n    " (render-children (normalize-children (sxml:content node))))] 
         [(symbol=? tag 'sup)
          (string-append "^" (render-children (sxml:content node)))]
         [(symbol=? tag 'img)
          ;; htmlprag form: (img (^ (src "...") (alt "...") ...))
          (let ([src (attr-value node 'src)])
            (if src (render-gif src) ""))]
         [else
          ;; generic passthrough: p, span, a, b, i, ...
          (render-children (normalize-children (sxml:content node)))]))]
     ;; anything that reaches here should be a bare string
     [else node]))

  (define (attr-value node attr-name)
    (let ([rest (cdr node)])
      (and (pair? rest) (pair? (car rest)) (symbol=? (caar rest) '^)
           (let ([hit (assoc attr-name (cdar rest))])
             (and hit (cadr hit))))))
  
  (define (render-entity sym)
    (let ([hit (assoc sym entity-table)])
      ;; bare 'br, 'li, 'ul etc. shouldn't reach here — tags are
      ;; handled by render-node. If one does, emit nothing.
      (if hit (cdr hit) "")))
  
  (define entity-table
    ;; bare symbols that htmlprag leaves unexpanded (HTML entities)
    '((nbsp    . " ")
      (lt      . "<")
      (le      . "<=")
      (gt      . ">")
      (ge      . ">=")
      (eacute  . "\xE9;")
      (middot  . "\xB7;")
      (szlig   . "\xDF;")))

  (define (render-gif src)
    (let ([hit (assoc src gif-table)])
      (if hit (cdr hit) "[image not available]")))

  (define gif-table
    '(("math/csug/0.gif"           . "=>")
      ("math/tspl/0.gif"           . "=>")
      ("math/csug/2.gif"           . "-->")
      ("math/tspl/8.gif"           . "-->")
      ("math/csug/4.gif"           . "->")
      ("math/tspl/9.gif"           . "->")
      ("math/csug/5.gif"           . "min(max(g+1, min-tg), max-tg)")
      ("math/tspl/27.gif"          . "1/2 x (1 2 3) = (1/2 1 3/2)")
      ("math/csug/3.gif"           . "\x3BB;")   ; lambda
      ("math/tspl/25.gif"          . "\x3BB;")   ; lambda
      ("math/tspl/3.gif"           . "\x22EE;")  ; vertical ellipsis
      ("math/tspl/13.gif"          . "\x221E;")  ; infinity
      ("math/tspl/20.gif"          . "\x3C2;")   ; final sigma
      ("math/tspl/21.gif"          . "\x3A3;")   ; big sigma
      ("math/tspl/22.gif"          . "\x3C3;")   ; small sigma
      ("math/tspl/11.gif"          . "-\x221E;") ; negative infinity
      ("math/tspl/12.gif"          . "+\x221E;") ; positive infinity
      ("math/tspl/14.gif"          . "-\x3C0;")  ; negative pi
      ("math/tspl/15.gif"          . "+\x3C0;")  ; positive pi
      ("gifs/ghostRightarrow.gif"  . "  ")))

  (define (render-children kids)
    (apply string-append
           (map (lambda (k) (render-sxml k)) kids)))

  ;; normalize-children: clean up whitespace/newline noise in a children list
  ;; so that render-children can be a simple walk with no lookahead or state

  (define (normalize-children children)
    (let* ([step1 (strip-embedded-newlines children)]
           [step2 (collapse-br-clusters step1)]
           [step3 (drop-leading-noise step2)]
           [step4 (drop-trailing-noise step3)])
      (normalize-inter-sibling-whitespace step4)))

  ;; strip trailing \n from every string node.
  ;; "some text\n" -> "some text"
  ;; "\n" -> ""
  ;; Leaves non-string nodes untouched.
  (define (strip-embedded-newlines children)
    (let loop ([lst children] [acc '()])
      (cond
       [(null? lst)
        (reverse acc)]
       [(and (string? (car lst))
             (string-ends-with-newline? (car lst)))
        (let ([stripped (strip-trailing-newline (car lst))])
          (if (null? (cdr lst))
              ;; trailing position — just drop the \n
              (loop (cdr lst) (cons stripped acc))
              ;; mid-content — replace \n with "" sentinel for step 5
              (loop (cdr lst) (cons "" (cons stripped acc)))))]
       [else
        (loop (cdr lst) (cons (car lst) acc))])))

  (define (string-ends-with-newline? str)
    (and (> (string-length str) 0)
         (char=? (string-ref str (- (string-length str) 1)) #\newline)))

  (define (strip-trailing-newline str)
    (substring str 0 (- (string-length str) 1)))

  ;; A cluster is: (empty-str?) (br) (empty-str?)*
  ;; where empty-str? is a string that was "\n" before step 1 (now "").
  ;; The whole cluster becomes a single (br).
  ;; Two consecutive (br) nodes (possibly separated by empty strings)
  ;; become ((br) (br)) — a blank line marker.
  (define (collapse-br-clusters children)
    (let loop ([lst children] [acc '()])
      (cond
       [(null? lst)
        (reverse acc)]
       ;; empty string just before a br: drop it (was trailing \n on prev string)
       [(and (string? (car lst))
             (string=? (car lst) "")
             (pair? (cdr lst))
             (br-node? (cadr lst)))
        (loop (cdr lst) acc)]
       ;; br node: emit it, then skip any following empty strings
       [(br-node? (car lst))
        (let ([rest (drop-while empty-string? (cdr lst))])
          (loop rest (cons '(br) acc)))]
       [else
        (loop (cdr lst) (cons (car lst) acc))])))

  (define (br-node? x)
    (and (pair? x) (symbol=? (car x) 'br)))

  (define (empty-string? x)
    (and (string? x) (string=? x "")))

  (define (drop-while pred lst)
    (cond [(null? lst) '()]
          [(pred (car lst)) (drop-while pred (cdr lst))]
          [else lst]))

  ;; drop leading noise (empty strings and empty-rendering anchor  nodes at the start).
  ;; Bare (a (^ (name "..."))) anchors render to "" but are pair nodes,
  ;; so drop-while empty-string? stopped at them,
  ;; leaving the "" sentinels that follow in a non-leading position where
  ;; step 5 converts them to spaces. Treating such anchors as leading noise
  ;; fixes entries like assq and putprop that have two (a ...) anchors
  ;; before the (span (^ (class "formdef")) ...) node.
  (define (drop-leading-noise children)
    (drop-while leading-noise? children))

  (define (leading-noise? node)
    (or (empty-string? node)
        (empty-anchor? node)))

  ;; An empty anchor is (a (^ ...)) with no content beyond the attribute
  ;; subtree — it renders to nothing but is a pair node.
  (define (empty-anchor? node)
    (and (pair? node)
         (symbol=? (car node) 'a)
         (let ([kids (sxml:content node)])
           (or (null? kids)
               (and (= (length kids) 1)
                    (pair? (car kids))
                    (symbol=? (caar kids) '^))))))

  ;; drop trailing noise (empty strings and bare (br) at the end).
  ;; The trailing "\n" "\n" pattern and any dangling br.
  (define (drop-trailing-noise children)
    (let loop ([lst (reverse children)])
      (cond
       [(null? lst) '()]
       [(empty-string? (car lst)) (loop (cdr lst))]
       [(br-node? (car lst)) (loop (cdr lst))]
       [else (reverse lst)])))

  ;; replace empty strings that sit between two real siblings with " ",
  ;; and trim any leading whitespace from the following string node.
  ;; "word\n" (tt ...) becomes "word" "" (tt ...) after step 1,
  ;; and that "" should be a space separator, not nothing.
  ;; "word\n" "    continuation" becomes "word" "" "    continuation" after step 1,
  ;; and the leading spaces on the continuation string are HTML line-indentation
  ;; artifacts that should be dropped — the " " replacement already provides
  ;; the needed word separation.
  ;; Only applies when the empty string is flanked by content on both sides.
  (define (normalize-inter-sibling-whitespace children)
    (let loop ([lst children] [acc '()])
      (cond
       [(null? lst)
        (reverse acc)]
       [(and (empty-string? (car lst))
             (pair? acc)
             (pair? (cdr lst))
             (not (br-node? (cadr lst))))
        (let ([next (cadr lst)]
              [rest (cddr lst)])
          (if (string? next)
              (loop (cons (string-trim-leading next) rest) (cons " " acc))
              (loop (cdr lst) (cons " " acc))))]
       [else
        (loop (cdr lst) (cons (car lst) acc))])))

  (define (string-trim-leading str)
    (let loop ([i 0])
      (cond
       [(= i (string-length str)) ""]
       [(or (char=? (string-ref str i) #\space)
            (char=? (string-ref str i) #\tab))
        (loop (+ i 1))]
       [else (substring str i (string-length str))])))
  )

