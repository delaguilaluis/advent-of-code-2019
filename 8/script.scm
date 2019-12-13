(import (chicken file posix))
;; Open the file "read only"
(define infn (file-open "./input.txt" open/rdonly))

(define columnCount 25)
(define rowCount 6)

;; read some text from the file
(define readz
  (lambda ()
    (car (file-read infn columnCount))))

(define printRows
  (lambda (rowCount)
    (cond
      (zero? rowCount) (print done)
      (else (printRows (sub1 rowCount))))))

(define (main args)
  (printRows rowCount)
  (file-close infn))
