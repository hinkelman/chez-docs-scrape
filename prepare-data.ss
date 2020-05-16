(import (chez-stats))

;; cdr to drop column names
(define data (list (cons 'csug (cdr (read-delim "R/CSUG.tsv" #\tab)))
                   (cons 'tspl (cdr (read-delim "R/TSPL.tsv" #\tab)))))

(with-output-to-file "chez-docs-data.scm" (lambda () (write data)))
;; after writing to file need to add
;; (define data '
;; at the begining of the file and one more closing parethesis at the end
;; it is better to open the file in a plain text editor
;; because emacs will bog down on this big file trying to match parentheses and whatever else it does
