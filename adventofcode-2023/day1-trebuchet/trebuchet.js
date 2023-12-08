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
  let digits = wordsToDigits(calibrationLine)
  digits = digits.replace(/[^\d]+/g, "")
  if (digits) digits = digits[0] + digits.slice(-1)
  return Number(digits)
}

var words = {
  "one":    "1",
  "two":    "2",
  "three":  "3",
  "four":   "4",
  "five":   "5",
  "six":    "6",
  "seven":  "7",
  "eight":  "8",
  "nine":   "9",
}

function wordsToDigits(calibrationLine) {
  return calibrationLine
}

function reverseString(text) {
  let letters = text.split("")
  let reversed = letters.reverse()
  return reversed.join("")
}

module.exports = { calculateFile, calculate, calibrationLines, firstLastDigit, wordsToDigits }
