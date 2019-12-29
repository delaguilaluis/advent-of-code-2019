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

(define getLayers
  (lambda (layerSize)
    (define layer (getLayer layerSize))
    (cond
      ((null? layer) (quote ()))
      (else (cons layer (getLayers layerSize))))))

; count the number of ocurrences of `num`
(define countOccurrences
  (lambda (lat num ocurrences)
    (cond
      ((null? lat) ocurrences)
      ((= num (car lat)) (countOccurrences (cdr lat) num (add1 ocurrences)))
      (else (countOccurrences (cdr lat) num ocurrences)))))

(define getZerosOnLayers
  (lambda (layers)
    (cond
      ((null? layers) (quote ()))
      (else (cons
        (countOccurrences (car layers) 0 0)
        (getZerosOnLayers (cdr layers)))))))

(define getLayerWithOcurrences
  (lambda (occurrences layers)
    (define currentLayer (car layers))
    (define ocurrencesOfCurrent (countOccurrences currentLayer 0 0))
    (cond
      ((= ocurrencesOfCurrent occurrences) currentLayer)
      (else (getLayerWithOcurrences occurrences (cdr layers))))))

; get the min value out of a list of numbers
(define meen
  (lambda (lat minVal)
    (cond
      ((null? lat) minVal)
      ((< (car lat) minVal) (meen (cdr lat) (car lat)))
      (else (meen (cdr lat) minVal)))))

(define joinLayers
  (lambda (frontLayer backLayer)
    (cond
      ((null? frontLayer) (quote ()))
      ; if front layer pixel is transparent (2); use back layer pixel instead
      ((= 2 (car frontLayer)) (cons (car backLayer) (joinLayers (cdr frontLayer) (cdr backLayer))))
      (else (cons (car frontLayer) (joinLayers (cdr frontLayer) (cdr backLayer)))))))

(define reduce
  (lambda (layers accumulator)
    (cond
      ((null? layers) accumulator)
      (else (reduce (cdr layers) (joinLayers accumulator (car layers)))))))

(define (main args)
  (define layerSize (* rowCount columnCount))
  (define layers (getLayers layerSize))
  (file-close infn)

  (define zerosOnLayers (getZerosOnLayers layers))

  ; start assuming the first value of the list is the minimum
  (define minimum (meen zerosOnLayers (car zerosOnLayers)))
  (define layerOfInterest (getLayerWithOcurrences minimum layers))
  (define result (*
    (countOccurrences layerOfInterest 1 0)
    (countOccurrences layerOfInterest 2 0)))
  (print result)

  (define merged (reduce layers (car layers)))
  (print merged))
