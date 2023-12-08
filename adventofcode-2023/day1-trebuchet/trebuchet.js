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
  let firstDigit = findFirstDigit(calibrationLine)
  let lastDigit = findFirstDigit(reverse(calibrationLine))
  return Number(`${firstDigit}${lastDigit}`)
}

var words = {
  "1":    "1",
  "2":    "2",
  "3":    "3",
  "4":    "4",
  "5":    "5",
  "6":    "6",
  "7":    "7",
  "8":    "8",
  "9":    "9",
  "one":  "1",
  "two":  "2",
  "three":"3",
  "four": "4",
  "five": "5",
  "six":  "6",
  "seven":"7",
  "eight":"8",
  "nine": "9",
}

function findFirstDigit(calibrationLine) {
  let newDigit = ""

  let letters = calibrationLine.split("")

  wordselect:
  for (let word in words) {
    for (let index = 0; index < letters.length; index++) {
      let partLetters = letters.slice(index).join("")
      if (partLetters.startsWith(word)) {
        newDigit = words[word]
        break wordselect
      }
    }
  }

  return newDigit
}

function reverseString(text) {
  let letters = text.split("")
  let reversed = letters.reverse()
  return reversed.join("")
}

module.exports = { calculateFile, calculate, calibrationLines, firstLastDigit, findFirstDigit }
