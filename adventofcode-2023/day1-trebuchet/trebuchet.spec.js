var trebuchet = require("./trebuchet")

describe("trebuchet", function() {
  
  describe("final answer", function() {
    test("adds up numbers", function() {
      let sum = trebuchet.calculate('puzzle_input')

      expect(sum).toBe(0)
    })
  })

  describe("line numbers", function() {
    test("are first and last", function() {
      expect(trebuchet.firstLastDigit("1abc2")).toBe("12")
      expect(trebuchet.firstLastDigit("pqr3stu8vwx")).toBe("38")
      expect(trebuchet.firstLastDigit("a1b2c3d4e5f")).toBe("15")
      expect(trebuchet.firstLastDigit("treb7uchet")).toBe("77")
      expect(trebuchet.firstLastDigit("789")).toBe("79")
    })

    test("nothing if bad input", function() {
      expect(trebuchet.firstLastDigit("no digits")).toBe("")
      expect(trebuchet.firstLastDigit("")).toBe("")
      expect(trebuchet.firstLastDigit(null)).toBe("")
      expect(trebuchet.firstLastDigit(undefined)).toBe("")
    })
  })
})
