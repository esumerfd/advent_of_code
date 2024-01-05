#lang racket

(define (is-symbol char)
  "Checks if a character is a symbol in the engine schematic."
  (member char '("*" "#" "+" "." "$")))

(define (is-part-number char row col schematic)
  "Checks if a character is a part number based on its proximity to a symbol."
  (if (not (char-numeric? char))
      #f
      (let loop ((i -1))
        (if (< i 2)
            #f
            (let ((new-row (+ row i))
                  (new-col (+ col i)))
              (when (and (< 0 new-row) (< new-col (string-length (list-ref schematic new-row))))
                  (if (is-symbol (string-ref (list-ref schematic new-row) new-col))
                      #t
                      (loop (- i 1)))
                                      ))))))

(define (calculate-part-sum schematic)
  "Calculates the sum of all part numbers in the engine schematic."
  (let ((part-sum 0))
    (for-each (lambda (row)
                (for-each (lambda (char)
                            (when (is-part-number char (car row) (cdr row) schematic)
                              (set! part-sum (+ part-sum (char->integer char)))))
                          row))
              schematic)
    part-sum))

; Define the engine schematic as a list of lists.
(define schematic
  '("467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."))

; Calculate and print the sum of all part numbers.
(display "The sum of all part numbers is: ")
(display (calculate-part-sum (schematic "puzzle-input")))
(newline)

