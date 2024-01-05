#lang racket

(define (sum-part-numbers schematic)
  (let ((rows (string-split schematic "\n")))
    (fold (lambda (sum row)
             (+ sum (mapcar #'string->number
                             (remove-if #'\char-whitespace? 
                                      (flatten row)))))
           0 rows)))

(define (main input-file)
  (display (sum-part-numbers (file-read input-file))))

(main "puzzle-input")
