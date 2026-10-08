#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2020 Travis Hinkelman
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (chez-docs-scrape))

;; ---------------------------------------------------------------------------
;; flatten
;; ---------------------------------------------------------------------------

(test-begin "flatten")

(test-equal "nested list"
  '(a b c d)
  (flatten '(a (b (c) d))))
(test-equal "already flat"
  '(a b c)
  (flatten '(a b c)))
(test-equal "empty list"
  '()
  (flatten '()))
(test-equal "deeply nested"
  '(1 2 3 4 5)
  (flatten '(1 (2 (3 (4 (5)))))))
(test-equal "mixed strings and symbols"
  '("a" b "c")
  (flatten '("a" (b ("c")))))

(test-end "flatten")

;; ---------------------------------------------------------------------------
;; strip-dotslash
;; ---------------------------------------------------------------------------

(test-begin "strip-dotslash")

(test-equal "strips leading dot-slash"
  "objects:s0"
  (strip-dotslash "./objects:s0"))
(test-equal "no dot-slash unchanged"
  "objects:s0"
  (strip-dotslash "objects:s0"))
(test-equal "only dot-slash"
  ""
  (strip-dotslash "./"))
(test-equal "double dot-slash only strips one"
  "./foo"
  (strip-dotslash "././foo"))

(test-end "strip-dotslash")

;; ---------------------------------------------------------------------------
;; check-formdef / check-headers / check-footer
;; ---------------------------------------------------------------------------

(test-begin "predicates")

(test-assert "formdef present"
  (check-formdef '(p (span (^ (class "formdef")) (b "procedure")))))
(test-assert "formdef absent"
  (not (check-formdef '(p "just some text"))))
(test-assert "formdef in nested tt"
  (check-formdef '(p (span (^ (class "formdef")) (tt "(eq?")))))

(test-assert "check-headers h1"
  (check-headers '(p (h1 "Title"))))
(test-assert "check-headers h3"
  (check-headers '(p (h3 "Section"))))
(test-assert "check-headers h6"
  (check-headers '(p (h6 "Deep"))))
(test-assert "check-headers none"
  (not (check-headers '(p "plain text"))))
(test-assert "check-headers in nested content"
  (check-headers '(p (div (h2 "nested")))))

(test-assert "check-footer present"
  (check-footer '(p "Copyright " "2024")))
(test-assert "check-footer absent"
  (not (check-footer '(p "body text"))))

(test-end "predicates")

;; ---------------------------------------------------------------------------
;; br-node? / empty-string? / drop-while
;; ---------------------------------------------------------------------------

(test-begin "helpers")

(test-assert "br-node? positive"
  (br-node? '(br)))
(test-assert "br-node? negative — string"
  (not (br-node? "br")))
(test-assert "br-node? negative — other tag"
  (not (br-node? '(p "text"))))

(test-assert "empty-string? positive"
  (empty-string? ""))
(test-assert "empty-string? negative — non-empty"
  (not (empty-string? " ")))
(test-assert "empty-string? negative — symbol"
  (not (empty-string? 'x)))

(test-equal "drop-while drops matching prefix"
  '(3 4 5)
  (drop-while (lambda (x) (< x 3)) '(1 2 3 4 5)))
(test-equal "drop-while nothing matches"
  '(1 2 3)
  (drop-while (lambda (x) (> x 10)) '(1 2 3)))
(test-equal "drop-while all match"
  '()
  (drop-while number? '(1 2 3)))
(test-equal "drop-while empty list"
  '()
  (drop-while number? '()))

(test-end "helpers")

;; ---------------------------------------------------------------------------
;; strip-embedded-newlines
;; ---------------------------------------------------------------------------

(test-begin "strip-embedded-newlines")

(test-equal "trailing newline stripped, sentinel inserted"
  '("some text" "" "x")
  (strip-embedded-newlines '("some text\n" "x")))
(test-equal "trailing newline in last position — no sentinel"
  '("some text")
  (strip-embedded-newlines '("some text\n")))
(test-equal "standalone newline mid-list becomes empty string with sentinel"
  '("" "" "x")
  (strip-embedded-newlines '("\n" "x")))
(test-equal "non-string nodes pass through unchanged"
  '((br) (& nbsp) "text" "" "x")
  (strip-embedded-newlines '((br) (& nbsp) "text\n" "x")))
(test-equal "string without newline unchanged"
  '("hello")
  (strip-embedded-newlines '("hello")))
(test-equal "empty list"
  '()
  (strip-embedded-newlines '()))

(test-end "strip-embedded-newlines")

;; ---------------------------------------------------------------------------
;; collapse-br-clusters
;; ---------------------------------------------------------------------------

(test-begin "collapse-br-clusters")

(test-equal "empty string before br is dropped"
  '((br))
  (collapse-br-clusters '("" (br))))
(test-equal "empty string after br is dropped"
  '((br))
  (collapse-br-clusters '((br) "")))
(test-equal "empty strings on both sides of br dropped"
  '((br))
  (collapse-br-clusters '("" (br) "")))
(test-equal "non-empty string before br is kept"
  '("text" (br))
  (collapse-br-clusters '("text" (br))))
(test-equal "double br preserved as two br nodes"
  '((br) (br))
  (collapse-br-clusters '((br) "" (br))))
(test-equal "non-br content unchanged"
  '("a" "b" "c")
  (collapse-br-clusters '("a" "b" "c")))

(test-end "collapse-br-clusters")

;; ---------------------------------------------------------------------------
;; drop-leading-noise / drop-trailing-noise
;; ---------------------------------------------------------------------------

(test-begin "drop-leading-trailing-noise")

(test-equal "drop-leading-noise removes leading empty strings"
  '("text")
  (drop-leading-noise '("" "" "text")))
(test-equal "drop-leading-noise nothing to drop"
  '("text" "")
  (drop-leading-noise '("text" "")))
(test-equal "drop-leading-noise empty list"
  '()
  (drop-leading-noise '()))

(test-equal "drop-trailing-noise removes trailing empty strings"
  '("text")
  (drop-trailing-noise '("text" "" "")))
(test-equal "drop-trailing-noise removes trailing br"
  '("text")
  (drop-trailing-noise '("text" (br))))
(test-equal "drop-trailing-noise removes mixed trailing noise"
  '("text")
  (drop-trailing-noise '("text" "" (br) "")))
(test-equal "drop-trailing-noise nothing to drop"
  '("" "text")
  (drop-trailing-noise '("" "text")))

(test-end "drop-leading-trailing-noise")

;; ---------------------------------------------------------------------------
;; normalize-inter-sibling-whitespace
;; ---------------------------------------------------------------------------

(test-begin "normalize-inter-sibling-whitespace")

(test-equal "empty string between two nodes becomes space"
  '("word" " " "next")
  (normalize-inter-sibling-whitespace '("word" "" "next")))
(test-equal "no empty strings — unchanged"
  '("a" "b" "c")
  (normalize-inter-sibling-whitespace '("a" "b" "c")))

(test-end "normalize-inter-sibling-whitespace")

;; ---------------------------------------------------------------------------
;; render-entity / render-gif
;; ---------------------------------------------------------------------------

(test-begin "render-entity")

(test-equal "nbsp" " "    (render-entity 'nbsp))
(test-equal "lt"   "<"    (render-entity 'lt))
(test-equal "gt"   ">"    (render-entity 'gt))
(test-equal "le"   "<="   (render-entity 'le))
(test-equal "ge"   ">="   (render-entity 'ge))
(test-equal "eacute" "\xE9;" (render-entity 'eacute))
(test-equal "middot" "\xB7;" (render-entity 'middot))
(test-equal "szlig"  "\xDF;" (render-entity 'szlig))
(test-equal "unknown entity returns empty string"
  ""
  (render-entity 'bogus))

(test-end "render-entity")

(test-begin "render-gif")

(test-equal "csug 0.gif"          "=>"  (render-gif "math/csug/0.gif"))
(test-equal "tspl 0.gif"          "=>"  (render-gif "math/tspl/0.gif"))
(test-equal "csug 2.gif"          "-->" (render-gif "math/csug/2.gif"))
(test-equal "csug 4.gif"          "->"  (render-gif "math/csug/4.gif"))
(test-equal "csug 3.gif is lambda" "\x3BB;" (render-gif "math/csug/3.gif"))
(test-equal "ghost arrow gif"     "  "  (render-gif "gifs/ghostRightarrow.gif"))
(test-equal "unknown gif returns placeholder"
  "[image not available]"
  (render-gif "math/csug/9999.gif"))

(test-end "render-gif")

;; ---------------------------------------------------------------------------
;; sxml->doc
;; ---------------------------------------------------------------------------

(test-begin "sxml->doc")

(test-equal "bare string passes through"
  '("hello")
  (sxml->doc "hello"))
(test-equal "integer becomes char"
  (list (string (integer->char 304)))
  (sxml->doc 304))
(test-equal "unknown bare symbol drops out"
  '()
  (sxml->doc 'bogus))
(test-equal "(& nbsp) becomes space"
  '(" ")
  (sxml->doc '(& nbsp)))
(test-equal "(& le) becomes math"
  '((math "<="))
  (sxml->doc '(& le)))
(test-equal "^ node drops out"
  '()
  (sxml->doc '(^ (class "formdef"))))
(test-equal "br"
  '((br))
  (sxml->doc '(br)))
(test-equal "table keeps rows and cells"
  '((table (row (cell (code "ptr")) (cell "any object"))))
  (sxml->doc '(table (tr (td (tt "ptr")) (td "any object")))))
(test-equal "sup"
  '((sup "6"))
  (sxml->doc '(sup "6")))
(test-equal "img with known gif becomes math"
  '((math "=>"))
  (sxml->doc '(img (^ (src "math/csug/0.gif") (alt "<graphic>")))))
(test-equal "img with unknown gif"
  '((math "[image not available]"))
  (sxml->doc '(img (^ (src "math/csug/9999.gif") (alt "<graphic>")))))
(test-equal "ul with li items"
  '((ul (li "item1") (li "item2")))
  (sxml->doc '(ul (li "item1") (li "item2"))))
(test-equal "dt"
  '((dt "term"))
  (sxml->doc '(dt "term")))
(test-equal "dd"
  '((dd "detail"))
  (sxml->doc '(dd "detail")))
(test-equal "b becomes bold"
  '((bold "returns: "))
  (sxml->doc '(b "returns: ")))
(test-equal "i becomes var"
  '((var "italic"))
  (sxml->doc '(i "italic")))
(test-equal "empty tt drops out"
  '()
  (sxml->doc '(tt)))
(test-equal "tt with nbsp and metavariable"
  '((code "(quote " (var "obj") ")"))
  (sxml->doc '(tt "(quote" (& nbsp) (i "obj") ")")))
(test-equal "link keeps href"
  '((link "./io.html#g1" "7"))
  (sxml->doc '(a (^ (href "./io.html#g1")) "7")))
(test-equal "empty named anchor drops out"
  '()
  (sxml->doc '(a (^ (name "./objects:s0")))))
(test-equal "span is spliced into parent"
  '((bold "procedure") ": " (code "(foo x)"))
  (sxml->doc '(span (^ (class "formdef")) (b "procedure") ": " (tt "(foo x)"))))

;; normalization via passthrough
(test-equal "trailing newlines in p dropped"
  '("some text")
  (sxml->doc '(p "some text" "\n" "\n")))
(test-equal "mid-prose embedded newline becomes space"
  '("foo bar")
  (sxml->doc '(p "foo\n" "bar")))
(test-equal "br with adjacent newline noise becomes single br"
  '((bold "returns: ") (code "#t") (br) (bold "libraries: ") (code "(chezscheme)"))
  (sxml->doc '(p (b "returns: ") (tt "#t") "\n" (br) "\n"
                 (b "libraries: ") (tt "(chezscheme)") "\n" "\n")))
(test-equal "double br kept as blank line"
  '("line1" (br) (br) "line2")
  (sxml->doc '(p "line1" (br) "\n" (br) "\n" "line2")))
(test-equal "multiline tt indentation preserved"
  '((code "(define f" (br) "  (lambda (x) x))"))
  (sxml->doc '(p (tt "(define" (& nbsp) "f" (br) "\n" "\n"
                      (& nbsp) (& nbsp) "(lambda" (& nbsp) "(x)" (& nbsp) "x))"))))

(test-end "sxml->doc")

;; ---------------------------------------------------------------------------
;; extract-p-anchor
;; ---------------------------------------------------------------------------

(test-begin "extract-p-anchor")

(test-equal "extracts and strips dot-slash"
  "objects:s0"
  (extract-p-anchor '(p (a (^ (name "./objects:s0")))
                        (span (^ (class "formdef")) "x"))))
(test-equal "no dot-slash — unchanged"
  "objects:s0"
  (extract-p-anchor '(p (a (^ (name "objects:s0")))
                        (span (^ (class "formdef")) "x"))))

(test-end "extract-p-anchor")

;; ---------------------------------------------------------------------------
;; group-formdefs
;; ---------------------------------------------------------------------------

(test-begin "group-formdefs")

(define formdef-p
  '(p (a (^ (name "./test:s0")))
      (span (^ (class "formdef"))
            (b "procedure")
            ": "
            (tt "(foo x)"))
      "\n" (br) "\n"
      (b "returns: ") (tt "x") "\n" "\n"))

(define formdef-p2
  '(p (a (^ (name "./test:s1")))
      (span (^ (class "formdef"))
            (b "procedure")
            ": "
            (tt "(bar y)"))
      "\n" (br) "\n"
      (b "returns: ") (tt "y") "\n" "\n"))

(define prose-p
  '(p "This is some prose.\n" "\n"))

(define footer-p
  '(p "Copyright " "2024"))

(test-assert "prose-only p-list produces empty result"
  (null? (group-formdefs (list prose-p))))

(test-equal "single formdef before footer retained"
  (list (list "test:s0" formdef-p))
  (group-formdefs (list formdef-p footer-p)))

(test-equal "two consecutive formdefs both retained"
  (list (list "test:s0" formdef-p) (list "test:s1" formdef-p2))
  (group-formdefs (list formdef-p formdef-p2 footer-p)))

(test-equal "prose following a formdef is grouped with it"
  (list (list "test:s0" formdef-p prose-p))
  (group-formdefs (list formdef-p prose-p footer-p)))

(test-equal "prose before any formdef is dropped"
  (list (list "test:s0" formdef-p))
  (group-formdefs (list prose-p formdef-p footer-p)))

(test-end "group-formdefs")

;; ---------------------------------------------------------------------------
;; process-html-file
;; ---------------------------------------------------------------------------

(test-begin "process-html-file")

(let ([entries (process-html-file "html-tspl" "objects.html")])
  (test-equal "quote entry header"
    '((bold "syntax") ": " (code "(quote " (var "obj") ")") (br)
      (bold "syntax") ": " (code "'" (var "obj")) (br)
      (bold "returns: ") (code (var "obj")) (br)
      (bold "libraries: ") (code "(rnrs base)") ", " (code "(rnrs)"))
    (cadr (assoc "objects:s2" entries)))
  (test-assert "every entry has an anchor and at least one paragraph"
    (for-all (lambda (e) (and (string? (car e)) (pair? (cdr e)))) entries))
  (test-assert "no empty paragraphs"
    (for-all (lambda (e) (for-all pair? (cdr e))) entries)))

(test-end "process-html-file")

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
