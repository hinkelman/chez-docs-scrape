(import (chez-stats))

;; cdr to drop column names
(define data (list (cons 'csug (cdr (read-delim "R/CSUG.tsv" #\tab)))
                   (cons 'tspl (cdr (read-delim "R/TSPL.tsv" #\tab)))))

(with-output-to-file "chez-docs-data.scm"
  (lambda () (write `(define data ',data))))


