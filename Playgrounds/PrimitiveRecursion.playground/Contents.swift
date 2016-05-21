/*:
 # Primitive Recursion
 
 The source is the first part of [this lesson](http://www.cs.cmu.edu/~cdm/pdf/PrimRec-6up.pdf).
 
 We're going to define *primitive recursion* as a way to create recursive functions from non-recursive ones, by the means of basic functions and tools.

 First of all, let's just consider the `N` type as the type for natural numbers:
 */
typealias N = UInt

class Arg_n {
  let values: [N]

  init(_ values: N ...) {
    self.values = values
    guard values.count == numberOfArguments else { fatalError("\(self): initialized with wrong number of arguments") }
  }

  var numberOfArguments: Int {
    return values.count
  }

  func at(index: Int) -> N {
    guard index < numberOfArguments else { fatalError("\(self): selecting 'at' with overflown index \(index)") }
    return values[index]
  }
}

class Arg_0: Arg_n {
  init() {
    super.init()
  }
  override var numberOfArguments: Int { return 0 }
}

class Arg_1: Arg_n {
  init(_ value1: N) {
    super.init(value1)
  }

  override var numberOfArguments: Int { return 1 }
}

class Arg_2: Arg_n {
  init(_ value1: N, _ value2: N) {
    super.init(value1, value2)
  }

  override var numberOfArguments: Int { return 2 }
}

class Arg_3: Arg_n {
  init(_ value1: N, _ value2: N, _ value3: N) {
    super.init(value1, value2, value3)
  }

  override var numberOfArguments: Int { return 3 }
}

/*:
 We want to define **primitive recursive functions**, that is, functions of type:
 */
typealias Primitive = Arg_n -> Arg_1

/*:
 To define this class of functions, we are going to use **induction**, that is, we define a number of basic functions, we build more complex cases with composition and a *limited type of recursion*.
*/
/*: ------ */
/*:
 ## Basic types
 
 There are 3 basic functions to consider:

 A **Constant zero** is a Recursor that will always return zero.
 */
let constantZero: Primitive = { _ in Arg_1(0) }

/*:
 A **Projection** selects a single value from the argument set.

 Notice that we are going to use the *ordinal* number for selecting the argument to project, like in mathematics: this means, for example, that to express the projection of the second argument we are going to write `p(2)`.
 */
func p(ordinal: Int) -> Primitive {
  return { arg in Arg_1(arg.at(ordinal-1)) }
}

/*:
 A **Successor** function will return the successive natural number.
 */
let succ: Arg_1 -> Arg_1 = { arg in Arg_1(arg.at(0) + 1) }

/*: ------ */
/*:
 ## Basic tools

 **Composition** for two functions can be defined like this:
 */
infix operator • { associativity left precedence 190 }
func • <A, B, C> (lhs: B -> C, rhs: A -> B) -> A -> C {
  return { a in lhs(rhs(a)) }
}

/*:
 **Primitive recursion** can be defined like this:
 */
func primitiveRecursor(h: Arg_3 -> Arg_1, _ g: Arg_1 -> Arg_1) -> Arg_2 -> Arg_1 {
  return { arg in
    switch (arg.at(0), arg.at(1)) {
    case let (0, y):
      return g(Arg_1(y))
    case let (xPlusOne, y):
      let x = xPlusOne - 1
      return h(Arg_3(x, primitiveRecursor(h, g)(Arg_2(x, y)).at(0), y))
    }
  }
}

func primitiveRecursor(h: Arg_2 -> Arg_1, _ g: Arg_1 -> Arg_1) -> Arg_2 -> Arg_1 {
  return { arg in
    switch (arg.at(0), arg.at(1)) {
    case let (0, y):
      return g(Arg_1(y))
    case let (xPlusOne, y):
      let x = xPlusOne - 1
      return h(Arg_2(primitiveRecursor(h, g)(Arg_2(x, y)).at(0), y))
    }
  }
}

func primitiveRecursor(h: Arg_1 -> Arg_1, _ g: Arg_1 -> Arg_1) -> Arg_2 -> Arg_1 {
  return { arg in
    switch (arg.at(0), arg.at(1)) {
    case let (0, y):
      return g(Arg_1(y))
    case let (xPlusOne, y):
      let x = xPlusOne - 1
      return h(Arg_1(primitiveRecursor(h, g)(Arg_2(x, y)).at(0)))
    }
  }
}

func primitiveRecursor(h: Arg_2 -> Arg_1, _ g: Arg_0 -> Arg_1) -> Arg_1 -> Arg_1 {
  return { arg in
    switch (arg.at(0)) {
    case 0:
      return g(Arg_0()) /// Arg_0 has to be initialized with 0 arguments, and basically means "Void"
    case let xPlusOne:
      let x = xPlusOne - 1
      return h(Arg_2(x, primitiveRecursor(h, g)(Arg_1(x)).at(0)))
    }
  }
}

/*:
 We need to define various primitive recursors, one for each type we need.

 A function is called *primitive recursive* if it can be constructed from the basic functions by applying composition and primitive recursion, so **basic functions + basic tools**.
 
 As can be easily seen, the primitive recursor will consider two cases for the `f` function:
 
 - if the first argument of the function is 0, it will call the `g` function with the remaining values (possibly one);
 - if the first argument is greater than zero, it will call the `h` function, with the predecessor of the first argument, along with the very function with that predecessor (thus, the primitive recursor again), and with all the other arguments;
 
 Thus, the primitive recursor is a recursive higher-order function, that can generate a recursive function `f` from non-recursive ones.
 */
/*: ------ */
/*:
 ## Example: Addition, Multiplication, Factorial
 */
let add: Arg_2 -> Arg_1 = primitiveRecursor(succ, { x in x })

let mult: Arg_2 -> Arg_1 = primitiveRecursor(add, constantZero)

/*:
 To define the `fact` function we actually need another compositional tool, a *factorizer*, that is, a higher-order function that builds a *product type* (our `Arg_2`) made of two elements, both individially generated by two single functions.
 
 [Source](https://bartoszmilewski.com/2015/01/07/products-and-coproducts/)
 */
let factorizer: (Arg_n -> Arg_1) -> (Arg_n -> Arg_1) -> (Arg_n -> Arg_2) =
  { f1 in { f2 in { args in Arg_2(f1(args).at(0), f2(args).at(0)) } } }

let fact: Arg_1 -> Arg_1 = primitiveRecursor(
  mult • factorizer(succ • p(1))(p(2)),
  succ • constantZero)

/*: ------ */
/*:
  ## Example: Predecessor, Subtraction
 
 The `pred` function is simple: becuase the primitiveRecursor will call first function **h** on the previous natural number, and then call itself again, we just need a **h** that selects the first of those operations.
 */
let pred: Arg_1 -> Arg_1 = primitiveRecursor(p(1), constantZero)

/*:
  The `sub` function is quite tricky: it's going to take the predecessor of the same subtraction with the precedessor of the value on the right, until the value on the right is 0 (at which point the subtraction will return the value on the left); this way, the value on the left is going to be reduced (with `pred`) a number of times equal to the value on the right.
 */
let sub: Arg_2 -> Arg_1 = primitiveRecursor(pred • p(2) as Arg_3 -> Arg_1, p(1)) • factorizer(p(2))(p(1))

/*: ------ */
/*:
 ## Tests
 */

assert(add(Arg_2(4, 6)).at(0) == 10)
assert(mult(Arg_2(7, 8)).at(0) == 56)
assert(fact(Arg_1(5)).at(0) == 120)
assert(pred(Arg_1(9)).at(0) == 8)
assert(pred(Arg_1(0)).at(0) == 0)
assert(sub(Arg_2(15, 9)).at(0) == 6)
assert(sub(Arg_2(4, 7)).at(0) == 0)

/*: ------ */
print("DONE")
