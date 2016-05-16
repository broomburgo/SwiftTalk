struct Number: CustomStringConvertible {
  let value: Int
  init(_ value: Int) {
    self.value = value
  }

  func times(factor: Number) -> Number {
    return Number(value*factor.value)
  }

  var description: String {
    return "\(value)"
  }
}

let two = Number(2)
let three = Number(3)

let six = two.times(three)

let timesExponential = Number.times

let sixAgain = timesExponential(two)(three)

struct Person {
  let name: String
  init(name: String) {
    self.name = name
  }

  func getName() -> String {
    return name
  }
}

// (A -> () -> B) -> (A -> B)

func ex<A,B>(origin: A -> () -> B) -> A -> B {
  return { origin($0)() }
}

/*
 (P => (false => Q)) => (P => Q)
 (false => Q) => Q
 */

let getNameExponential = ex(Person.getName)

let persons = [
  Person(name: "John"),
  Person(name: "Lucy"),
  Person(name: "Annie")
]

let names = persons.map(getNameExponential)
let john = names[0]
