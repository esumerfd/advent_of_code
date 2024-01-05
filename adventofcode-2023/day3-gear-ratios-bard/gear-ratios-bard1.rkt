#lang racket

(define (adjacent-symbols y x schematic)
  (cond ((or (< y 0) (< x 0) (>= y (length schematic)) (>= x (length (car schematic)))) '())
        ((char= (cadr (car schematic) x) "*") '(x (+ y 1)))
        ((char= (cadr (car schematic) x) "#") '(x (- y 1)))
        ((char= (cadr (car schematic) x) "+") '(x (+ y -1)))
        ((char= (cadr (car schematic) x) "$") '(x (- y -1)))
        ((string= (cdr (cadr (car schematic) x)) ".") '())
        (else (cons x (append (adjacent-symbols (+ y 1) x schematic)
                             (adjacent-symbols (- y 1) x schematic)
                             (adjacent-symbols y (+ x 1) schematic)
                             (adjacent-symbols y (- x 1) schematic)))))

(define (sum-part-numbers schematic)
  (let loop ((y 0) (x 0) (sum 0))
    (if (null? schematic)
      sum
      (let ((row (car schematic))
            (neighbors (adjacent-symbols y x schematic)))
        (loop (car (cdr schematic))
              (+ x 1)
              (cond ((null? neighbors) sum)
                    ((number? (car neighbors))
                     (+ sum (car neighbors))
                     (loop (cdr neighbors) x sum))
                    (t sum)))))))

