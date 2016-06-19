/*:
 # All About Monads
 
 A more or less complete translation into Swift of the famous "All About Monads" Haskell tutorial, available [here](https://wiki.haskell.org/All_About_Monads). The translation will actually be  **opinionated**, because I'll make personal observations on what's going on that could be not in line with the original tutorial. Also, I'm going to use different examples.
 
 Monads in programming are a way of structure computations with values and sequences of transformations of those value; thus, monads are tools for constructing programs in a compositional way.
 
 We can think of monads as strategies to combine computations.
 
 For example, the Swift `Optional` type has methods, called `map` and `flatMap` that represent strategy with which we can combine and transform optionals.
 
 There's a [HUGE](https://wiki.haskell.org/Monad_tutorials_timeline) amount of *monad tutorials* on the web; why? Probably because talking about monads is like talking about different kinds of cakes: you can prepare them, slice them up and eat them, but that's about it; every cake is different from the others, and has specific attributes. It's the same with monads: their basic abstract definition doesn't tell us much about them, and it's even worse if we start talking about the actual category-theoretic definition of a monad.
 
 So why study monads? Because they provide **modularity**, **flexibility** and **isolation** in a safe, composable way to functional programs. One could observe that object-oriented programming tries to achieve the same thing through classes, but they're very different beasts, because:
 
 - monads have to follow specific mathematical laws, that make them more reliable and *provably correct* in their use;
 - there are specific, strict ways to *create* a monad;
 - there actually exists already a bunch of existing monads that cover a lot of cases, while when we create a class it's like we always start all over again;
 
 What is a monad in practice? It is a *particular* kind of **type constructor**: Haskell programmers are familiar with the concept, but in Swift we can think of it as the definition of a generic stuct o enum.
    
     For the purpose of the tutorial we are going to re-create the `Optional` type and call it `Maybe`, like it is in Haskell.
 
 Let's define the `Maybe` type, that works like `Optional`:
 */

enum Maybe<A> {
  case Nothing
  case Just(A)
}

/*:
 Why is `Maybe` a *type constructor*? Because `Maybe` is not a type in itself: `Maybe<Int>` is a type, `Maybe<String>` is a type. The *word* `Maybe` will let us construct a type putting `<` and `>` to the right of the word, and an existing type between them. `A`, in `Maybe<A>` is just a placeholder for an existing type. Notice that in Haskell we write `data Maybe a`: it's exactly the same of `enum Maybe<A>`, the syntax and conventions are different but the meat is the same. 
 
 If `Maybe` is a type constructor, `Nothing` and `Just` are *data* (or value) constructors. We can't push the analogy between Swift and Haskell too far, because in Haskell we can be generic (polimorphic) in both the *contained* type and the *container*, while in Swift we can only be generic in the contained type: e.g. in Haskell we can write `data m a` for a generic type constructor parametric on a generic type, while in Swift if we write `struct M<A>`, the `M` is defined, and not generic.

 This means that we won't be able to actually generically define monads in Swift like it's done in Haskell, save for doing some acrobatic gestures with protocols (which are not the same as Haskell's typeclasses) like it's done in [Swiftz](https://github.com/typelift/Swiftz). For every monad we define, we'll create a different type, and give that type the attributes of a monad.
 
 Ok, let's go.
 */
 /*: ------ */
/*:
 ## The Maybe monad
 
 For a type constructor to be a monad we need two functions associated with it:
 
 - a (generic) function `return` that builds values of the type, where the polimorfic "contained" type is the same as the generic type of the function, so for `Maybe`:
 */
extension Maybe {
  static func return_(value: A) -> Maybe<A> {
    return .Just(value)
  }
}
/*:
 - a (twice generic) operator `>>-`, usually called `bind` that builds a new instance of `Maybe` from and existing instance and a function that creates the new one from the value *contained* in the first one.
 */
infix operator >>- { associativity left precedence 140 }
func >>- <A,B> (lhs: Maybe<A>, rhs: A -> Maybe<B>) -> Maybe<B> {
  switch lhs {
  case .Nothing:
    return .Nothing
  case let .Just(value):
    return rhs(value)
  }
}
/*: 
 Every monad, to be a monad, needs these two functions, and they must obey certain laws, which we'll see further ahead.
 
 Let's make an example. Say we are going to make a family tree, and we want to be able to write functions for retieving the living parents, grandparents and so on of particular people. Because parents can be alive or dead, every `Person` has a `Maybe<Person>` as mother and a `Maybe<Person>` as father.
 */
class Person1 {
  let mother: Maybe<Person1>
  let father: Maybe<Person1>
  init(mother: Maybe<Person1>, father: Maybe<Person1>) {
    self.mother = mother
    self.father = father
  }
}

func getMother1(person: Person1) -> Maybe<Person1> {
  return person.mother
}

func getFather1(person: Person1) -> Maybe<Person1> {
  return person.father
}
/*:
 If we want to get the paternal grandfather of someone's mother we could write the following method:
 */
extension Person1 {
  func mothersPaternalGrandfather1() -> Maybe<Person1> {
    switch getMother1(self) {
    case .Nothing:
      return .Nothing
    case let .Just(theMother):
      switch getFather1(theMother) {
      case .Nothing:
        return .Nothing
      case let .Just(theMothersFather):
        return getFather1(theMothersFather)
      }
    }
  }
}
/*:
 For an arbitrary relationship this gets quite boring, until you realize that to go over each parental step we do the same thing: we check if the parent is there, then we check the parent's required parent and so on.
 
 It would be better if we had a way to combine the `getMother` and `getFather` by taking into account the fact that both doesn't return a `Person` but a `Maybe<Person>`, so *method chaining* won't work.
 
 If we look a the type of the functions, they're both of type `Person -> Maybe<Person>`, which looks like the function a the right hand side of the `bind` operator, i.e., `A -> Maybe<B>`, where `A` and `B` are both `Person`. We can see that, in `mothersPaternalGrandfather1`, at each `switch` we do three things:
 
 1. we check if the value is `Nothing`, and if it is we just return `Nothing`;
 2. if there's a value, we apply the `A -> Maybe<B>` function;
 3. we then get another value, which has to go through the first two steps again;
 
 We can see that the implementation of `bind` precisely correspond to the first to steps, so by just applying `bind` to a `Maybe<Person>`, passing the right function on the right hand side (i.e., `getMother` or `getFather`), we get another `Maybe<Person>` to work with.
 
 The result is the following:
 */
extension Person1 {
  func mothersPaternalGrandfather2() -> Maybe<Person1> {
    return Maybe<Person1>.return_(self)
      >>- getMother1
      >>- getFather1
      >>- getFather1
  }
}
/*:
 This way we can compose those functions arbitrarily.
 
 We can make the thing a little more "Swifty" by adding the functions `getMother` and `getFather` as methods to `Person`, but we need a function to extract the methods as [exponential objects](/Playgrounds/Exponentials.playground/Contents.swift): will define this function as a postfix operator `^`.
 
 We'll also add an initializer to `Maybe` that will act as `return`.
 */
extension Person1 {
  func getMother() -> Maybe<Person1> {
    return mother
  }

  func getFather() -> Maybe<Person1> {
    return father
  }
}

postfix operator ^ {}
postfix func ^ <A,B> (origin: A -> () -> B) -> A -> B {
  return { origin($0)() }
}

extension Maybe {
  init(_ value: A) {
    self = .Just(value)
  }
}

extension Person1 {
  func mothersPaternalGrandfather() -> Maybe<Person1> {
    return Maybe(self)
      >>- Person1.getMother^
      >>- Person1.getFather^
      >>- Person1.getFather^
  }
}
/*:
 Finally, we'll just redefine everything we just did for the included type `Optional`, to make it work as a monad. We'll also redefine `Person` to use optionals (as expected).
 */
func >>- <A,B> (lhs: A?, rhs: A -> B?) -> B? {
  switch lhs {
  case .None:
    return .None
  case let .Some(value):
    return rhs(value)
  }
}

class Person {
  let mother: Person?
  let father: Person?
  init(mother: Person?, father: Person?) {
    self.mother = mother
    self.father = father
  }

  func getMother() -> Person? {
    return mother
  }

  func getFather() -> Person? {
    return father
  }

  func mothersPaternalGrandfather() -> Person? {
    return Optional(self)
      >>- Person.getMother^
      >>- Person.getFather^
      >>- Person.getFather^
  }
}
/*:
 As we can see, the `>>-` combinator captures a general strategy for combining computations *that may fail to return a value*.
 */
/*: ------ */
/*:
 ## The recipe for a monad

 `List` is also a monad, and to be more *swifty* up-front we're going to directly refer to the `Array` type.
 
 Why is `List` a monad? What kind of *computation strategy* does it represent? There's plenty of uses, the most obvious being accumulating computations on sets of the same kind of data (like a list of integers); but another, more interesting case, can be the one of *ambiguous* computations, that is, computations that can return any number of values of a certain kind (also zero). The case with one or zero values is similar of the one of the `Maybe` monad, but more values are accepted.

The Swift's standard library already defines a method on `Array` that corresponds to the `bind` operation: it's called `flatMap`, so we can trivially define the `>>-` operator on `Array`.
 */
func >>- <A,B>(lhs: [A], rhs: A -> [B]) -> [B] {
	return lhs.flatMap(rhs)
}
/*:
 So `Optional` and `Array` share a mechanism for composing computations, based on the following generic pattern:

```func >>- <A,B>(lhs: Monad<A>, rhs: A -> Monad<B>) -> Monad<B>```

 Unfortunately, we cannot express this concept generically in Swift, becase `Monad` in the function above has to be the *same kind of type* for both `Monad<A>` and `Monad<B>`, and for example if we used this function on a `Optional<A>` the type system should enforce the creation of a `Optional<B>` and not a generic `Monad<B>`. It's not possible to do this in Swift because the lack of higher-kinded types (e.g. *typeclasses* in Haskell).

This means that we cannot express generic computations on monads in Swift without an hacky approach. It's not a big deal really: we can still take advantage of the monadic approach for various cases, without the need for a completely generic interface. The idea is to *equip* with `>>-` various kinds of types, and also create specific types with a specific meaning for the operation. But there's a catch: to have some actual meaning, `>>-` and `return` must follow some *laws* that will give it the mathematical power it needs.

#### The monad laws

The laws basically state two things:

- `return` must be an *identity* operation in respect to `>>-`, both from left and from right (first two laws);
- `>>-` must be left-associative (third law);

To state this, we need to prove for each monad we build that (we'll use a fake `Monad` type to express the operations in Swift):
 */
class Monad<A: Equatable>: Equatable {
	let value: A
	init(_ value: A) {
		self.value = value
	}
}
func >>- <A,B> (lhs: Monad<A>, rhs: A -> Monad<B>) -> Monad<B> {
	return rhs(lhs.value)
}
func == <A>(lhs: Monad<A>, rhs: Monad<A>) -> Bool {
	return lhs.value == rhs.value
}

extension Monad {
	static func firstLaw(f f: A -> Monad<A>) -> A -> Bool {
		return { x in (Monad(x) >>- f) == f(x) }
	}

	static func secondLaw() -> A -> Bool {
		return { x in Monad(x) >>- Monad.init == Monad(x) }
	}

	static func thirdLaw(f f: A -> Monad<A>, g: A -> Monad<A>) -> A -> Bool {
		return { x in Monad(x) >>- f >>- g == Monad(x) >>- { a in f(a) >>- g } }
	}

}
/*:
 To check the laws validity for `Monad<A>`, we'll simply apply the laws to an array of random integers, and check if it stands for every integer: because we're "passing in" the `f` and `g`functions to the laws, the proof can be considered sufficient for the present case.
*/
/// 'randomIntegers' is an helper function, defined in "Utilities.swift"
func checkLaw(law: Int -> Bool) {
	assert(randomIntegers(range: (-100...100), count: 100).map(law).reduce(true) { $0 && $1 });
}

checkLaw(Monad<Int>.firstLaw(f: { Monad($0*$0) }))
checkLaw(Monad<Int>.secondLaw())
checkLaw(Monad<Int>.thirdLaw(f: { Monad($0*$0) }, g: { Monad($0*2) }))


















print("TO BE CONTINUED")

