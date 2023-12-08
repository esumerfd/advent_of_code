var trebuchet = require("./trebuchet")

describe("trebuchet", function() {
  
  describe("final answer", function() {
    test("adds up numbers", function() {
      let sum = trebuchet.calculateFile('puzzle_input')

      // Part One  expect(sum).toBe(54304)
      // expect(sum).toBe(000)
    })

    test.each`
      expected | calibrationLine
      ${12}       | ${"12"}
      ${99}       | ${"aaaaa987cccccc789ddddd"}
      ${11+22+33} | ${"a1a1a\nb2b2b\nc3c3c\n"}
    `("expecting $expected from calibrationLine $calibrationLine", ({expected, calibrationLine}) => {

      let sum = trebuchet.calculate(calibrationLine)

      expect(sum).toBe(expected)
    })
  })

  describe("line numbers", function() {
    test("are first and last", function() {
      expect(trebuchet.firstLastDigit("1abc2")).toBe(12)
      expect(trebuchet.firstLastDigit("pqr3stu8vwx")).toBe(38)
      expect(trebuchet.firstLastDigit("a1b2c3d4e5f")).toBe(15)
      expect(trebuchet.firstLastDigit("treb7uchet")).toBe(77)
      expect(trebuchet.firstLastDigit("789")).toBe(79)
    })

    test("nothing if bad input", function() {
      expect(trebuchet.firstLastDigit("no digits")).toBe(0)
      expect(trebuchet.firstLastDigit("")).toBe(0)
      expect(trebuchet.firstLastDigit(null)).toBe(0)
      expect(trebuchet.firstLastDigit(undefined)).toBe(0)
    })
  })

  describe("read data", function() {
    test("one line at a time", function() {
      let count = 0
      let data = "line1\nline2\nline3\n"

      trebuchet.calibrationLines(data, function (line) { count++ })

      expect(count).toBe(3)
    })
  })

  describe("Part two", function() {
    test("include words", function() {
      expect(trebuchet.firstLastDigit("two1nine")).toBe(29)
      expect(trebuchet.firstLastDigit("eightwothree")).toBe(83)
      expect(trebuchet.firstLastDigit("abcone2threexyz")).toBe(13)
      expect(trebuchet.firstLastDigit("xtwone3four")).toBe(24)
      expect(trebuchet.firstLastDigit("4nineeightseven2")).toBe(42)
      expect(trebuchet.firstLastDigit("zoneight234")).toBe(14)
      expect(trebuchet.firstLastDigit("7pqrstsixteen")).toBe(76)
    })

    test("simple work replacement", function() {
      expect(trebuchet.findFirstDigit("one", "1"))
      expect(trebuchet.findFirstDigit("two", "2"))
      expect(trebuchet.findFirstDigit("onetwo", "12"))
    })

    test.only("overlapped words", function() {
      expect(trebuchet.findFirstDigit("oneightthree")).toBe("1")
      expect(trebuchet.findFirstDigit("eightwofour")).toBe("8")
    })
  })
})
