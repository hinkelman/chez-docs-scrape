;; For now, I'm made chez-stats available globally on my system
;; when chez-stats is published to akku, I will install it at project level
(import (chez-stats))

;; cdr to drop column names
(define data (list (cons 'csug (cdr (read-delim "R/CSUG.tsv" #\tab)))
                   (cons 'tspl (cdr (read-delim "R/TSPL.tsv" #\tab)))))

(with-output-to-file "chez-docs-data.scm" (lambda () (write data)))


