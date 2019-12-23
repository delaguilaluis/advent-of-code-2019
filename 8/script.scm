(import (chicken file posix))

;; Open the file "read only"
(define infn (file-open "./input.txt" open/rdonly))

(define columnCount 25)
(define rowCount 6)

; get a number representation from a char
(define getNumber
  (lambda (c)
    (- (char->integer c) 48)))

(define charList->numberList
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (getNumber (car lat)) (charList->numberList (cdr lat)))))))

(define getLayer
  (lambda (charCount)
    (define result (file-read infn charCount))
    (define readCharsCount (car (cdr result)))
    (cond
      ;; return list of read chars only if completely read
      ((= readCharsCount charCount)
        (charList->numberList (string->list (car result))))
      (else (quote ())))))

; (define printLayers
;   (lambda (size)
;     (define layer (getLayer size))
;     (cond
;       ((null? layer) #t)
;       (else
;         (print layer)
;         (printLayers size)))))

; get the max value out of a list of numbers
(define max
  (lambda (lat maxVal)
    (cond 
      ((null? lat) maxVal)
      ((> (car lat) maxVal) (max (cdr lat) (car lat)))
      (else (max (cdr lat) maxVal)))))

; count the number of ocurrences of `num`
(define countOccurrences
  (lambda (lat num ocurrences)
    (cond
      ((null? lat) ocurrences)
      ((= num (car lat)) (countOccurrences (cdr lat) num (add1 ocurrences)))
      (else (countOccurrences (cdr lat) num ocurrences)))))

(define zerosOnLayers
  (lambda (layerSize)
    (cond
      (null? (getLayer layerSize) (quote ()))
      (else (cons (countOccurrences (getLayer layerSize) 0 0) (zerosOnLayers layerSize))))))

(define (main args)
  (define layerSize (* rowCount columnCount))
  (print (zerosOnLayers layerSize))
  (file-close infn))
