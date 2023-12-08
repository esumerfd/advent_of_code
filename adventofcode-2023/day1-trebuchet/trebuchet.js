// Node solution
const fs = require('node:fs');

function calculateFile(puzzleFile) {
  let data = load(puzzleFile)
  return calculate(data)
}

function calculate(data) {
  let sum = 0
  calibrationLines(data, (line) => {
    let decodedLine = firstLastDigit(line)
    sum += decodedLine
  })
  return sum
}

function load(filename) {
  return fs.readFileSync(filename, 'utf8');
}

function calibrationLines(data, lineCallback) {
  let lines = data.split('\n');
  for(let i = 0;i < lines.length;i++){
    if (lines[i].length > 0) lineCallback(lines[i])
  }
}

function firstLastDigit(calibrationLine) {
  if (!calibrationLine) return 0
  var digits = calibrationLine.replace(/[^\d]+/g, "")
  if (digits) digits = digits[0] + digits.slice(-1)
  return Number(digits)
}

module.exports = { calculateFile, calculate, calibrationLines, firstLastDigit  }
