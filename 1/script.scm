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
    ((= rmdr 0) quotient)
    (else (- quotient (/ rmdr y)))))

(define (forgiving-substraction x y)
  (cond
    ((> y x) 0)
    (else (- x y))))

(define (compute-fuel-b mass fuel)
  (define fuel-for-mass (forgiving-substraction (floored-division mass 3) 2))
  (cond
    ((<= mass 0) fuel)
    (else (compute-fuel-b fuel-for-mass (+ fuel fuel-for-mass)))))

(define (compute-fuel mass)
  (compute-fuel-b mass 0))

(define (compute-all list)
  (cond
    ((null? list) 0)
    (else (+ (compute-fuel (car list)) (compute-all (cdr list))))))

(define (main args)
  (define list (file->list (file-open "./input.txt" open/rdonly)))
  (define numberList (split list))
  (print (compute-all numberList)))
