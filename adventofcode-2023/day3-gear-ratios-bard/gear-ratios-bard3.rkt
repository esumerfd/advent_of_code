#lang racket

(define (read-schematic input)
  (map #'(lambda (line)
         (map #'string->number
              (string-split line " ")))
      (string-split input "\n")))

(define (is-part-number elem schema row col)
  (cond ((and (< row 0) (< col 0)) #f)
        ((and (< row 0) (= col 0)) #f)
        ((and (>= row (length schema)) (< col 0)) #f)
        ((and (>= row (length schema)) (= col 0)) #f)
        ((or (#=(char (string-ref schema row (+ col 1))) #\+)
             (#=(char (string-ref schema row (- col 1))) #\+)
             (#=(char (string-ref schema (+ row 1) col)) #\+)
             (#=(char (string-ref schema (- row 1) col)) #\+)
             (#=(char (string-ref schema row col)) #\*))
              (not (string-char-p? elem "#\\.")))
        (#t #f)))

(define (sum-part-numbers schema)
  (let ((sum 0))
    (do ((row 0 (length schema)))
      ((do ((col 0 (length (car schema)))
            ((+ col 1)))
        (when (is-part-number (car schema) schema row col)
          (setq sum (+ sum (car schema))))))
    sum))
