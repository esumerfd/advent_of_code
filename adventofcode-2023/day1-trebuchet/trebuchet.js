// Node solution
const fs = require('node:fs');

function calculate(puzzleFile) {
  let data = load(puzzleFile)
  let sum = 0
  return sum
}

function load(filename) {
  return fs.readFileSync(filename, 'utf8');
}

function firstLastDigit(calibrationLine) {
  if (!calibrationLine) return ""
  var digits = calibrationLine.replace(/[^\d]+/g, "")
  if (digits) digits = digits[0] + digits.slice(-1)
  return digits
}

module.exports = {  calculate, firstLastDigit  }
