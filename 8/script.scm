(import (chicken file posix))

;; Open the file "read only"
(define infn (file-open "./input.txt" open/rdonly))

(define columnCount 25)
(define rowCount 6)

;; read some text from the file
(define readz
  (lambda (charCount)
    (define result (file-read infn charCount))
    (define readCharsCount (car (cdr result)))
    (cond
      ;; return list of read chars only if completely read
      ((= readCharsCount charCount) (car result))
      (else (quote ())))))

(define printLayers
  (lambda (size)
    (define layer (readz size))
    (cond
      ((null? layer) #t)
      (else
        (print layer)
        (printLayers size)))))

(define (main args)
  (define layerSize (* rowCount columnCount))
  (printLayers layerSize)
  (file-close infn))
