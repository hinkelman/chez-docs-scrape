;; I'm planning to re-organize chez-stats
;; so this import line will change
;; need to change first chez-docs blog post when I change this
(import (chez-stats chez-stats))

;; cdr to drop column names
(define data (list (cons 'csug (cdr (read-tsv "R/CSUG.tsv")))
                   (cons 'tspl (cdr (read-tsv "R/TSPL.tsv")))))

(with-output-to-file "chez-docs-data.scm" (lambda () (write data)))


