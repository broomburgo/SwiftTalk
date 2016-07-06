/*:
# All About Monads - part 2

A more or less complete translation into Swift of the famous "All About Monads" Haskell tutorial, available [here](https://wiki.haskell.org/All_About_Monads). The translation will actually be  **opinionated**, because I'll make personal observations on what's going on that could be not in line with the original tutorial. Also, I'm going to use different examples.

Due to the complexity of the topic, this wil be split in 2 different playgrounds. This is the second one.

------

## Combining monads

In the context of software development, monads are used for expressing composable computations. The `bind` method's implementation for each monad represents the way a particular monad is composed with other instances of the same monad. But we often need to combine multiple computation strategies: for example, the operation of `Operation<A>` could in fact return an instance of another monad, like `Either<A>`: in this case we would end up with a `Operation<Either<A>>`, and the `bind` method of this instance will work with functions like `Either<A> -> Operation<B>`, which is not what we probably want.

For example, in the aforementioned case, we would like a *bind-like* method that could work with functions like `A -> Operation<Either<B>>`. How do we express this kind of composition in a generic way? First of all, monads are **not** arbitrarily composable, and we need strategies for each combination of monads. Is there a generic *form* for these strategies?

Let's try and implement a `bindEither` method for this case: to do that in Swift, we need to specify that the `A` type for `Operation<A>` is an *either type*, so we need a protocol for that:
*/
protocol EitherType {
	associatedtype RightType
	func get() throws -> RightType
}

extension Either: EitherType {
	typealias RightType = A
}
/*:
Now we can implement a `bindEither` method for `Operation<A>` where `A: EitherType`:
*/
extension Operation where A: EitherType {
	func bindEither <B> (transform: A.RightType -> Operation<Either<B>>) -> Operation<Either<B>> {
		return Operation<Either<B>> {
			do {
				return try transform(self.runOperation().get()).runOperation()
			}
			catch let error {
				return .Left(error)
			}
		}
	}
}
/*:
Producing a custom method specific for the `Operation<A>` is not particularly elegant, it doesn't clearly express the combination strategy, and is **not composable**: seems ok for a single nested monad, but multiple nesting would require specific implementations for each level of nesting.

Also, in some cases we might need to combine computations that are interdependent: for example, the combination strategy for an optional array of optionals could be that the first optional will result in `None` if the contained array is empty, or if it contains only `None` instances.

Now, there's various possible approaches to the problem.

If everything is wrapped into an operation, we need a way to produce functions of the wrapped value, which can be passed to the `bind` method, from functions that start from the inner value
*/
extension Either {
	static func liftOperation(base: A -> Operation<Either<A>>) -> Either<A> -> Operation<Either<A>> {
		return { eitherA in
			do {
				return try base(eitherA.get())
			}
			catch let error {
				return Operation.unit(.Left(error))
			}
		}
	}
}
/*:
Notice how the method is not really specific to `Operation<A>`: the only thing that's required is a `unit` static method.

We can define a generic protocol for `unit`
*/
protocol UnitType {
	associatedtype Wrapped
	static func unit(value: Wrapped) -> Self
}

extension Operation: UnitType {
	typealias Wrapped = A
}

extension Either {
	static func lift<Wrapper: UnitType where Wrapper.Wrapped == Either>(base: A -> Wrapper) -> Either<A> -> Wrapper {
		return { eitherA in
			do {
				return try base(eitherA.get())
			}
			catch let error {
				return Wrapper.unit(.Left(error))
			}
		}
	}
}
/*:
Now we can generically lift every function that produces a wrapped `Either<A>` from a `A` into a function that actually starts from `Either<A>`, so that the wrapped can bind the functions (if bind is available).

We can extend every monad we created with this strategy. We can even define a generic procotol for "liftable" types:
*/
protocol LiftType {
	associatedtype Wrapped
	static func lift<Wrapper: UnitType where Wrapper.Wrapped == Self>(base: Wrapped -> Wrapper) -> Self -> Wrapper
}

extension Either: LiftType {
	typealias Wrapped = A
}





















print("done")
