(import (chicken file posix))

(define (file->list infn)
  (define result (file-read infn 1))
  (define read-char (car result))
  (define read-chars-count (car (cdr result)))

  (cond
    ((= read-chars-count 0) (quote ()))
    (else (cons read-char (file->list infn)))))

(define (split-b list current result)
  (cond
    ((null? list) result)
    (else (cond
      ;; Breaklines are #f when converted to number
      ((eq? (string->number (car list)) #f)
        ;; Restore current string and cons its number representation to result
        (split-b (cdr list) "" (cons (string->number current) result)))
      (else
        ;; Add digit to the current string
        (split-b (cdr list) (string-append current (car list)) result))))))

(define (split list)
  (split-b list "" (quote ())))

(define (floored-division x y)
  (define quotient (/ x y))
  (define rmdr (remainder x y))

  (cond
    ((= rmdr 0)
      quotient)
    (else
      (- quotient (/ rmdr y)))))

(define (compute-fuel mass)
  (- (floored-division mass 3) 2))

(define (compute-all list)
  (cond
    ((null? list) 0)
    (else (+ (compute-fuel (car list)) (compute-all (cdr list))))))

(define (main args)
  (define list (file->list (file-open "./input.txt" open/rdonly)))
  (define numberList (split list))
  (print (compute-all numberList)))
