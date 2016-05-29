/*:
 ## Exponentials
 
 [Reference](https://en.wikipedia.org/wiki/Exponential_object)

 We'd like to *extract* a method from a class and create a pure function out of it.
 
 The first paramter of the resulting function will be an instance of that class.
 */

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

/*:
 This doesn't work properly in Swift for methods that have a Void input, because we're going to produce a function of type:
 
 `A -> () -> B`
 
 But what we really want is a function of type:
 
 `A -> B`
 
 So to make this work we need a higher-order function of type:
 
 `(A -> () -> B) -> (A -> B)`

 We're sure this can be implemented, because this means (thanks, [Curry-Howard isomorphism](https://en.wikipedia.org/wiki/Curry–Howard_correspondence)):
 
 `(P => (false => Q)) => (P => Q)`

 but

`(false => Q) => Q`

 thus, we get a tautology.
 */

func exp<A,B>(origin: A -> () -> B) -> A -> B {
  return { origin($0)() }
}

let getNameExponential = exp(Person.getName)

/*:
 It's easier and more readable to define the `exp` function as a postfix operator `^`.
 */

postfix operator ^ {}
postfix func ^ <A,B> (origin: A -> () -> B) -> A -> B {
  return { origin($0)() }
}

let persons = [
  Person(name: "John"),
  Person(name: "Lucy"),
  Person(name: "Annie")
]

let names = persons.map(Person.getName^)
let john = names[0]

print("DONE")
