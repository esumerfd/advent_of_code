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

function isSymbol(char) {
  return char === "*" || char === "#" || char === "+" || char === "." || char === "$";
}

let sum = 0;
let accumulatedDigits = "";

for (let i = 0; i < schematic.length; i++) {
  for (let j = 0; j < schematic[i].length; j++) {
    const char = schematic[i][j];

    if (char.match(/\d/)) {
      accumulatedDigits += char;
    } else {
      if (accumulatedDigits.length > 0) {
        // Check for symbols surrounding the accumulated digits
        if (
          isSymbol(schematic[i][j]) || // Check current position
          (j > 0 && isSymbol(schematic[i][j - 1])) || // Check left
          (i > 0 && isSymbol(schematic[i - 1][j])) || // Check above
          (i > 0 && j > 0 && isSymbol(schematic[i - 1][j - 1])) || // Check top-left diagonal
          (i < schematic.length - 1 && isSymbol(schematic[i + 1][j])) || // Check below
          (j < schematic[i].length - 1 && isSymbol(schematic[i][j + 1])) || // Check right
          (i < schematic.length - 1 && j < schematic[i].length - 1 && isSymbol(schematic[i + 1][j + 1])) || // Check bottom-right diagonal
          (i > 0 && j < schematic[i].length - 1 && isSymbol(schematic[i - 1][j + 1])) || // Check top-right diagonal
          (i < schematic.length - 1 && j > 0 && isSymbol(schematic[i + 1][j - 1])) // Check bottom-left diagonal
        ) {
          sum += parseInt(accumulatedDigits);
          accumulatedDigits = "";
        }
      }
    }
  }

  // Handle symbols at the end of a row, considering accumulated digits
  if (accumulatedDigits.length > 0) {
    if (
      isSymbol(schematic[i][schematic[i].length - 1]) ||
      (i > 0 && isSymbol(schematic[i - 1][schematic[i].length - 1])) ||
      (i < schematic.length - 1 && isSymbol(schematic[i + 1][schematic[i].length - 1]))
    ) {
      sum += parseInt(accumulatedDigits);
      accumulatedDigits = "";
    }
  }
}

// Handle any remaining digits at the end
if (accumulatedDigits.length > 0) {
  sum += parseInt(accumulatedDigits);
}

console.log("Sum of part numbers:", sum);

