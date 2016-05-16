/*:
 ## The Road to Monad
 
 [Reference](https://www.youtube.com/watch?v=ZhuHCtR3xq8)

 We're going to talk about **pure functions**: some value goes in, some other value goes out, no side effects.
 
 Swift notation for variables and functions is pretty mathematical: a `let` definition is an assertion about the type (an possibly the value) of a variable.
 */

let x: Int
let f1: Int -> Int

/*:
 We want to assert the type of completely generic functions, but we can't do that in Swift (as of 2.2), so we are going to use Int as a generic type.
 */

struct A {
  let value: Int
  init(_ value: Int) {
    self.value = value
  }
}

let f: A -> A
let g: A -> A

/*:
 First of all, to actually use them we need to define them, so for the sake of simplicity we are going write the as simple functions.
 */

f = { A($0.value*2) }
g = { A($0.value*$0.value) }

/*:
 How can we compose them into a function, say, `h`?
 */

infix operator • { associativity left precedence 190 }
func • <A, B, C> (lhs: B -> C, rhs: A -> B) -> A -> C {
  return { lhs(rhs($0)) }
}

let h: A -> A

h = g • f

/*:
 This is the *essence* of a monoid: we can combine arbitrary things, as long as the types match. This is **the way** to *build complexity*: we start with *small* things, then we combine them to build more complex stuff. The way to control complexity is **compositionality**.
 
 The way to build complexity is to insure that the types match, and to create a composition operation for those types.

 Before formalizing the *function monoid*, let's make a simpler example.
 
 First of all: a *Monoid* is a collection of things, then a *rule* to combine those things, then a *metarule* that should be obeyed by that rule, then a *unit* instance that is *neutral* in respect to the rule.
 
 Let's make a **clock hour monoid**.
 */

typealias Hour = Int
let oneH: Hour = 1
let twoH: Hour = 2
let threeH: Hour = 3
let fourH: Hour = 4
let fiveH: Hour = 5
let sixH: Hour = 6
let sevenH: Hour = 7
let eightH: Hour = 8
let nineH: Hour = 9
let tenH: Hour = 10
let elevenH: Hour = 11
let twelveH: Hour = 12

infix operator <> { associativity left precedence 95 }
func <> (lhs: Hour, rhs: Hour) -> Hour {
  return (lhs + rhs) % 12
}

assert((oneH <> threeH) == fourH)
assert((tenH <> sevenH) == fiveH)

/*:
 Here's the metarule:
 `<>` has to be associative: `(a <> b) <> c = a <> (b <> c)`
 
 Here's the unit:
 `twelveH`
 */

assert((threeH <> twelveH) == threeH)
assert((eightH <> twelveH) == eightH)

/*:
 Ok, so functions, actually **endofunctions**, that is, functions in which the input and the output have the same type, form a monoid in respect to the composition operator, with the unit being the identity function.
 */

func <> (lhs: A -> A, rhs: A -> A) -> A -> A {
  return lhs • rhs
}

let id: A -> A = { $0 }

let fAgain1 = f <> id
let fAgain2 = id <> f

assert(fAgain1(A(3)).value == 6)
assert(fAgain2(A(3)).value == 6)

/*:
 We now have all the ingredients to introduce **Monads**.
 
 Let's consider a function that *takes an A* and return some data that uses the A in some way.
 */

class M<T> {
  let value: T
  init(_ value: T) {
    self.value = value
  }
}

/*:
 We want to redefine the functions f, g, h with a different return type.
 */

let fm: A -> M<A>
let gm: A -> M<A>
let hm: A -> M<A>

/*:
 Now we need to explain compositionality for those functions.
 
 Let's say we have a function that takes and A and returns what we get by applying fm to that A
 */

let fl: A -> M<A> = { a in fm(a) }

infix operator >>- { associativity left precedence 100 }
func >>- <T,U> (lhs: M<T>, rhs: T -> M<U>) -> M<U> {
  return rhs(lhs.value)
}

fm = { M(f($0)) }
gm = { M(g($0)) }

hm = { a in fm(a) >>- { b in gm(b) } }
/// it's easier like this: hm = { fm($0) >>- gm }

assert(h(A(3)).value == hm(A(3)).value.value)

/*:
 To restore compositionality, we need the `>>-` operator (**bind**), which is not as simmetric as `<>`, and we need to actually put a *lamba* on the outer left of the expression.
 
 `>>-` works like `<>`, so has to be associative and has to have a unit instance.
 */

let im1: A -> M<A> = { a in (fm(a) >>- { b in gm(b) }) >>- { c in hm(c) } }
let im2: A -> M<A> = { a in fm(a) >>- ({ a in gm(a) >>- { c in hm(c) } }) }

assert(im1(A(2)).value.value == im2(A(2)).value.value)

let unit: A -> M<A> = { M($0) }

let m1 = M(A(3))
let m1u = m1 >>- unit

assert(m1.value.value == m1u.value.value)

/*:
 Last but not least, we want to consider functions like `A -> M<B>`, and compose them.
 */

struct B {
  let value: Int
  init(_ value: Int) {
    self.value = value
  }
}

struct C {
  let value: Int
  init(_ value: Int) {
    self.value = value
  }
}

/*:
 We can't actually compose arbitrary functions if types aren't all the same: we only have monoids for functions like A -> A.
 
 Notice that we are not saying that f <> g = g <> f! We are just saying that you **can** compose those functions in both ways: the result will probably be different, but it can be done!
 */

let ff: A -> B = { a in B(a.value * 2) }
let gg: B -> C = { b in C(b.value * b.value) }

/*:
 ff <> gg is ok, but gg <> ff is not

 But we can also compose the correspondent monads!
 */

let ffm: A -> M<B>
let ggm: B -> M<C>
let hhm: A -> M<C>

ffm = { a in M(ff(a)) }
ggm = { b in M(gg(b)) }

hhm = { a in ffm(a) >>- { b in ggm(b) } }
/// it's easier like this: hmm = { gmm($0) >>- fmm }

/*:
 **MIND BLOWN**
 */

print("DONE")
 





























