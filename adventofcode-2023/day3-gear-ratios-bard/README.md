--- Day 3: Gear Ratios ---

You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.

It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

"Aaah!"

You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.

The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

Here is an example engine schematic:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

# Run

* racket gear-ratios.rkt


# Bard

## Answer 1:

I can definitely help you solve this problem in Scheme! Here's an approach using two functions:

adjacent-symbols: This function takes a pair of coordinates in the schematic and checks if any valid symbol (excluding '.') is adjacent to them in any direction (including diagonals).
sum-part-numbers: This function takes the schematic as a list of strings and recursively iterates through it, checking for adjacent symbols and accumulating the sum of adjacent numbers.

Here's the code:

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


This code defines two functions:

adjacent-symbols takes the y and x coordinates of a number in the schematic and checks if there's a valid symbol adjacent to it (up, down, left, right, diagonals). It returns a list of neighboring coordinates if a symbol is found, or an empty list otherwise.
sum-part-numbers takes the schematic as a list of strings and recursively iterates through each character, checking if it's a number and adjacent to a symbol. If it is, its value is added to the sum. The function returns the final sum of all part numbers in the schematic.
To use this code, first read your actual schematic into a list of strings, one string per line. Then, call the sum-part-numbers function with the list of strings and it will return the sum of all part numbers in the schematic.

This is just one way to solve the problem, and there are other approaches you could take. Feel free to explore different solutions and let me know if you have any further questions!

Result is compile error:

    gear-ratios-bard1.rkt:3:0: read-syntax: expected a `)` to close `(`
      possible cause: indentation suggests a missing `)` before line 15
      context...:

## Answer 2

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

Result is missing function:

    gear-ratios-bard2.rkt:5:5: fold: unbound identifier
      in: fold
      location...:

## Answer 3

Here's the Scheme code to solve for the sum of all part numbers in the engine schematic:

    (define (sum-part-numbers schematic)
      "
      Sum all part numbers in the given engine schematic string.

      Args:
        schematic: A string representing the engine schematic.

      Returns:
        An integer, the sum of all part numbers.
      "
      (let ((rows (string-split schematic "\n")))
        (fold (lambda (sum row)
                 (+ sum (mapcar #'string->number
                                 (remove-if #'\char-whitespace? 
                                          (flatten row)))))
               0 rows)))

    (define (main input-file)
      (display (sum-part-numbers (file-read input-file))))

    (main "input.txt") ; Replace "input.txt" with your actual schematic file


Explanation:

sum-part-numbers function:

* Takes the schematic string as input.
* Splits the schematic into rows using string-split.
* Folds the rows, starting with 0 as the accumulator, using fold.
* Inside the fold, uses mapcar to convert each character in the row to a number (excluding whitespaces) using string->number.
* Filters out non-part numbers by checking if they're adjacent to a symbol using a custom predicate.
* Sums the part numbers in the current row and adds them to the accumulator.
* Finally, returns the accumulated sum after processing all rows.

main function:

* Takes the input file path as an argument.
* Reads the file contents using file-read.
* Calls the sum-part-numbers function on the file contents.
* Displays the sum of all part numbers.

Usage:

Replace "input.txt" with the actual path to your engine schematic file.
Run the code in your Scheme interpreter.

The output will be the sum of all part numbers in the schematic.

Note:

This code assumes the engine schematic format is similar to the provided example.
You might need to adjust the symbol detection logic or whitespace handling depending on your specific schematic format.
I hope this helps you solve the Day 3: Gear Ratios puzzle in Scheme!

Result is compile error:

    gear-ratios-bard3.rkt:14:14: read-syntax: bad syntax `#=`

# Answer 4

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

Result is caused by my syntax of the input. Program expected list of lists and got a string:
application: not a procedure;

     expected a procedure that can be applied to arguments
      given: '("467..114.." "...*......" "..35..633." "......#..." "617*......" ".....+.58." "..592....." "......755." "...$.*...." ".664.598..")

# Answer 5

Switched to javascript so I can understand the errors better.

the conversation continued for a while, me telling Bard what the errors were and it correcting the code.

    const schematic = `467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..`;

    let sum = 0;

    for (let i = 0; i < schematic.length; i++) {
      for (let j = 0; j < schematic[i].length; j++) {
        const char = schematic[i][j];

        // Check for digits adjacent to symbols, including diagonals, with boundary checks
        if (char.match(/\d/)) {
          if (
            (i > 0 && isSymbol(schematic[i - 1][j])) ||
            (i < schematic.length - 1 && isSymbol(schematic[i + 1][j])) ||
            (j > 0 && isSymbol(schematic[i][j - 1])) ||
            (j < schematic[i].length - 1 && isSymbol(schematic[i][j + 1]))
          ) {
            sum += parseInt(char);
          }
        }
      }
    }

    function isSymbol(char) {
      return char === "*" || char === "#" || char === "+" || char === "." || char === "$";
    }

    console.log("Sum of part numbers:", sum);


Bard missing boundary checks, and then only checking for a single digit resulted in those improvements.

    const schematic = `467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..`;

    let sum = 0;
    let accumulatedDigits = "";

    for (let i = 0; i < schematic.length; i++) {
      for (let j = 0; j < schematic[i].length; j++) {
        const char = schematic[i][j];

        if (char.match(/\d/)) {
          accumulatedDigits += char;
        } else if (isSymbol(char) || j === schematic[i].length - 1) {
          if (accumulatedDigits.length > 0) {
            sum += parseInt(accumulatedDigits);
            accumulatedDigits = "";
          }
        }
      }
    }

    function isSymbol(char) {
      return char === "*" || char === "#" || char === "+" || char === "." || char === "$";
    }


    if (accumulatedDigits.length > 0) {
      sum += parseInt(accumulatedDigits);
    }

    console.log("Sum of part numbers:", sum);

But still not producing the right answer. When I told Bard about that the code was improved.

    const schematic = `467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..`;

    let sum = 0;
    let accumulatedDigits = "";

    for (let i = 0; i < schematic.length; i++) {
      for (let j = 0; j < schematic[i].length; j++) {
        const char = schematic[i][j];

        if (char.match(/\d/)) {
          accumulatedDigits += char;
        } else {
          if (accumulatedDigits.length > 0 && (
            // Check for symbols around or after the accumulated digits
            isSymbol(char) ||
            (j > 0 && isSymbol(schematic[i][j - 1])) ||
            (i > 0 && isSymbol(schematic[i - 1][j])) ||
            (i > 0 && j > 0 && isSymbol(schematic[i - 1][j - 1])) ||
            (i < schematic.length - 1 && isSymbol(schematic[i + 1][j])) ||
            (j < schematic[i].length - 1 && isSymbol(schematic[i][j + 1])) ||
            (i < schematic.length - 1 && j < schematic[i].length - 1 && isSymbol(schematic[i + 1][j + 1])) ||
            (i > 0 && j < schematic[i].length - 1 && isSymbol(schematic[i - 1][j + 1])) ||
            (i < schematic.length - 1 && j > 0 && isSymbol(schematic[i + 1][j - 1]))
          )) {
            sum += parseInt(accumulatedDigits);
            accumulatedDigits = "";
          }
        }
      }

      // Handle symbols at the end of a row
      if (accumulatedDigits.length > 0 && (
        isSymbol(schematic[i][schematic[i].length - 1]) ||
        (i > 0 && isSymbol(schematic[i - 1][schematic[i].length - 1])) ||
        (i < schematic.length - 1 && isSymbol(schematic[i + 1][schematic[i].length - 1]))
      )) {
        sum += parseInt(accumulatedDigits);
        accumulatedDigits = "";
      }
    }

    if (accumulatedDigits.length > 0) {
      sum += parseInt(accumulatedDigits);
    }

    console.log("Sum of part numbers:", sum);

Bard has started sending parts of the code back. For example, it stopped printing the issymbol function presumably noting it hadnt changed.

when I told it that is was just using the i and j indexes and not the positions around the accumulated digits, it aggreed, but was not able to fix the code. Ending up asking me for the right answer.


