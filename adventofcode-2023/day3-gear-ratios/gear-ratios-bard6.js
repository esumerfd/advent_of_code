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
