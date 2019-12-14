(import (chicken file posix))

;; Open the file "read only"
(define infn (file-open "./input.txt" open/rdonly))
(define columnCount 25)
(define lastRow 5)
; (define lastLayer )

;; read some text from the file
(define getRow
  (lambda (columnCount)
    (define result (file-read infn columnCount))
    (define readChars (car (cdr result)))
    (print readChars)
    (define text (car result))))

(define printRows
  (lambda (currentRow)
    (print (getRow columnCount))
    (cond
      ((zero? currentRow) #t)
      (else (printRows (sub1 currentRow))))))

(define (main args)
  (printRows lastRow)
  (file-close infn))
