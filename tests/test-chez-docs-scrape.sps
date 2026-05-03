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
;; render-sxml
;; ---------------------------------------------------------------------------

(test-begin "render-sxml")

(test-equal "bare string passes through"
  "hello"
  (render-sxml "hello"))
(test-equal "integer renders as char"
  (string (integer->char 304))
  (render-sxml 304))
(test-equal "unknown bare symbol returns empty string"
  ""
  (render-sxml 'bogus))
(test-equal "(& nbsp) renders as space"
  " "
  (render-sxml '(& nbsp)))
(test-equal "(& le) renders as <="
  "<="
  (render-sxml '(& le)))
(test-equal "^ node returns empty string"
  ""
  (render-sxml '(^ (class "formdef"))))
(test-equal "br renders as newline"
  "\n"
  (render-sxml '(br)))
(test-equal "table renders as placeholder"
  "[table not shown]"
  (render-sxml '(table (tr (td "a")))))
(test-equal "sup renders with caret"
  "^6"
  (render-sxml '(sup "6")))
(test-equal "img with known gif"
  "=>"
  (render-sxml '(img (^ (src "math/csug/0.gif") (alt "<graphic>")))))
(test-equal "img with unknown gif"
  "[image not available]"
  (render-sxml '(img (^ (src "math/csug/9999.gif") (alt "<graphic>")))))
(test-equal "ul with li items"
  "\n\n* item1\n* item2\n"
  (render-sxml '(ul (li "item1") (li "item2"))))
(test-equal "dt renders with no newline prefix"
  "\nterm"
  (render-sxml '(dt "term")))
(test-equal "dd renders with indent"
  "\n    detail"
  (render-sxml '(dd "detail")))
(test-equal "b tag passes through"
  "bold"
  (render-sxml '(b "bold")))
(test-equal "i tag passes through"
  "italic"
  (render-sxml '(i "italic")))
(test-equal "tt with nbsp"
  "(eq? x)"
  (render-sxml '(tt "(eq?" (& nbsp) "x)")))
(test-equal "a anchor tag produces no text"
  ""
  (render-sxml '(a (^ (name "./objects:s0")))))
(test-equal "span passes through content"
  "procedure: (foo x)"
  (render-sxml '(span (^ (class "formdef")) (b "procedure") ": " (tt "(foo x)"))))

;; normalization via passthrough
(test-equal "trailing newlines in p dropped"
  "some text"
  (render-sxml '(p "some text" "\n" "\n")))
(test-equal "mid-prose embedded newline becomes space"
  "foo bar"
  (render-sxml '(p "foo\n" "bar")))
(test-equal "br with adjacent newline noise renders as single newline"
  "returns: #t\nlibraries: (chezscheme)"
  (render-sxml '(p (b "returns: ") (tt "#t") "\n" (br) "\n"
                   (b "libraries: ") (tt "(chezscheme)") "\n" "\n")))
(test-equal "double br renders as blank line"
  "line1\n\nline2"
  (render-sxml '(p "line1" (br) "\n" (br) "\n" "line2")))
(test-equal "multiline tt indentation preserved"
  "(define f\n  (lambda (x) x))"
  (render-sxml '(p (tt "(define" (& nbsp) "f" (br) "\n" "\n"
                        (& nbsp) (& nbsp) "(lambda" (& nbsp) "(x)" (& nbsp) "x))"))))

(test-end "render-sxml")

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
;; process-p-list
;; ---------------------------------------------------------------------------

(test-begin "process-p-list")

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
  (null? (process-p-list (list prose-p))))

(let ([result (process-p-list (list formdef-p footer-p))])
  (test-assert "single formdef before footer retained"
    (= (length result) 1))
  (test-assert "entry anchor is a string"
    (string? (caar result))))

(let ([result (process-p-list (list formdef-p formdef-p2 footer-p))])
  (test-assert "two consecutive formdefs both retained"
    (= (length result) 2)))

(let ([result (process-p-list (list formdef-p prose-p footer-p))])
  (test-assert "formdef followed by prose and footer: one entry retained"
    (= (length result) 1)))

(test-end "process-p-list")

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
