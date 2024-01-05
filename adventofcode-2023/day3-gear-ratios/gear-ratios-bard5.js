const fs = require('node:fs');

function calibrationLines(data, lineCallback) {
  let lines = data.split('\n');
  for(let i = 0;i < lines.length;i++){
    if (lines[i].length > 0) lineCallback(lines[i])
  }
}

function load(filename) {
  var data = []

  puzzle = fs.readFileSync(filename, 'utf8');
  calibrationLines(puzzle, (line) => {
    data.push(line.split(""))
  })

  return data;
}

const schematic = load("puzzle_input.test");

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

