;; I'm planning to re-organize chez-stats
;; so this import line will change
(import (chez-stats chez-stats))

;; cdr to drop column names
(define csug (cdr (read-tsv "R/CSUG.tsv")))
(define tspl (cdr (read-tsv "R/TSPL.tsv")))

;; thought it would be nice to use symbols instead of strings as keys
;; but special characters in the some of the keys, e.g., #2%variable,
;; create problems with that conversion
(define data (list (cons 'csug csug)
                   (cons 'tspl tspl)))

(with-output-to-file "chez-docs-data.scm" (lambda () (write data)))


