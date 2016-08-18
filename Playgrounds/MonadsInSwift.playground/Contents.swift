/*:
# Monads in Swift

This is going to be a proposal to express frequently used monads and monadic concepts using Swift features. There'n no unique answer to the question "how to implement monads in Swift" and some implementations try to basically translate Haskell code in Swift: this may not be the best solution, and considering the push for *protocol-oriented programming* in Swift, it's probably a good idea to use protocols to represent monads, and try and extract the most generic concepts into separate protocols.

This approach will lead us to implement *monad transformers* - which are basically essential tools to leverage monads in real-world cases - in a specific way that's actually based on protocols: basically, while in Haskell monad transformers are new monad instances, in Swift we will add methods to monads when certain constraints are satisfied.

I'm going to redefine some basic monads in terms of protocols, without much explanation about their properties: please refer to [All About Monads](/AllAboutMonads.playground) to understand the why and the how. Also, I'm going to go full Swift and avoid using Haskell names and concepts: we are going to use Swift conventions for everything, for example instead of defining a `unit` static function to create an instance of a monad by passing the wrapped value, we are just going to use a regular initializer. In this playground I'm going to just consider `Optional`, `Either`, `Writer` and `Reader` as monads, and only the `Writer<Either<Wrapped>,Log>` and `Reader<Optional<Wrapped>,Environment>` transformers: it will suffice to make a point and provide the basic concepts.

The code in this playground, other that have a teaching value, will be written so that it can be actually used in real world scenarios, and I'm goind to provide protocol extensions with implemented functions adequate to be used in production code. Thus, other than protocols, actual structs and enum, implementing the required methods, are going to be provided.

Let's go.

## Monad definitions with protocols
*/
/// Basic algebra

protocol WrapperType {
	associatedtype WrappedType
	init(_ value: WrappedType)
}

protocol Semigroup {
	/// AXIOM: is associative
	/// a.compose(b.compose(c)) = (a.compose(b)).compose(c)
	func compose(other: Self) -> Self
}

extension SequenceType where Generator.Element: Semigroup {
	func sconcat(initial: Generator.Element) -> Generator.Element {
		return reduce(initial) { $0.compose($1) }
	}
}

protocol EmptyType {
	static var empty: Self { get }
}

protocol Monoid: Semigroup, EmptyType {
	/// AXIOM: Self.empty <> a == a <> Self.empty == a
}
/*:
### Optional
*/
/// Basic definition
protocol OptionalType: WrapperType {
	init()
	func runOptional<A>(@noescape ifSome ifSome: WrappedType -> A, @noescape ifNone: () -> A) -> A
}

/// Data
extension Optional: OptionalType {
	typealias WrappedType = Wrapped

	func runOptional<A>(@noescape ifSome ifSome: WrappedType -> A, @noescape ifNone: () -> A) -> A {
		if let this = self {
			return ifSome(this)
		} else {
			return ifNone()
		}
	}
}

/// Monad
extension OptionalType {
	func flatMap <
		OtherType,
		OtherOptionalType: OptionalType
		where
		OtherOptionalType.WrappedType == OtherType
		>
		(@noescape transform: WrappedType -> OtherOptionalType) -> Optional<OtherType> {
		return runOptional(
			ifSome: { (wrapped) -> Optional<OtherType> in
				transform(wrapped).runOptional(
					ifSome: { (other) -> Optional<OtherType> in
						Optional(other)
					},
					ifNone: { () -> Optional<OtherType> in
						Optional()
				})
			},
			ifNone: { () -> Optional<OtherType> in
				Optional()
		})
	}
}
/*:
Notice how `flatMap`, even if it's generically defined on `OptionalType` and not `Optional`, will actually return a `Optional`. The reason for this is rather complex, and I'll try to explain it.

A protocol with associated types and/or `Self` reference cannot be used as an *existential* type, i.e., we cannot write some like:

``` swift
let value: OptionalType
```

The compiler will tell us that `OptionalType` can only be used as a genric contraint: this means that the compiler cannot create a `OptionalType` *box*: we can only use `OptionalType` as a constraint for a generic parameter, so that the compiler can actually infer a *proper* type (`class`, `enum` or `struct`) for that parameter, depending on context.

This means that, if `flatMap` returned `OptionalType` we couldn't assign the result to a constant, because we would incur in compilation error like *type cannot be inferred from context*: in some cases we could get away with it, if it's certain from the context that the return type of the expression is a concrete type. This by the way is probably the reason why `flatMap` on `SequenceType` **returns an array**: that way we can actually use the result.

An alternative would be to *type-erase* the `OptionalType` by creating a `AnyOptional` wrapped class, with the [usual type-erasure machinery](https://realm.io/news/type-erased-wrappers-in-swift/): this means that we could return `OptionalType` from `flatMap`, but during use we would we forced to always wrap the expression in a `AnyOptional` instance. This may be fine, but I really think that, considering the current state of Swift 3.0, it's simply better and more convenient to just return a `Optional`, or in general a concrete type.

The **real** reason why I'm using protocols here is so that I can define monad transformers as constrained extensions on types (like `extension WriterType where Wrapped: EitherType`), because, as of Swift 3.0, we cannot contrain an associated type to be a concrete type (it would make it non-generic). In a future when this will be possibile, we will be able to completely ditch protocols for monads in Swift, and only use concrete types.
*/
/*:
### Either
*/
/// Basic definition
protocol EitherType: WrapperType {
	init(_ right: WrappedType)
	init(_ left: ErrorType)
	func runEither<A>(@noescape ifRight ifRight: WrappedType -> A, @noescape ifLeft: ErrorType -> A) -> A
}

/// Data
enum Either<Wrapped>: EitherType {
	typealias WrappedType = Wrapped

	case Right(WrappedType)
	case Left(ErrorType)

	init(_ value: WrappedType) {
		self = .Right(value)
	}

	init(_ error: ErrorType) {
		self = .Left(error)
	}

	func runEither<A>(@noescape ifRight ifRight: WrappedType -> A, @noescape ifLeft: ErrorType -> A) -> A {
		switch self {
		case let .Right(value):
			return ifRight(value)
		case let .Left(error):
			return ifLeft(error)
		}
	}
}

/// Monad
extension EitherType {
	func flatMap <
		OtherType,
		OtherEitherType: EitherType
		where
		OtherEitherType.WrappedType == OtherType
		> (@noescape transform: WrappedType -> OtherEitherType) -> Either<OtherType> {
		return runEither(
			ifRight: { (right) -> Either<OtherType> in
				return transform(right).runEither(ifRight: Either.init, ifLeft: Either.init)
			},
			ifLeft: { (left) -> Either<OtherType> in
				return Either(left)
		})
	}
}
/*:
### Writer
*/
/// Basic defintion
protocol WriterType: WrapperType {
	associatedtype LogType: Monoid
	init(_ value: WrappedType, _ info: LogType)
	var runWriter: (WrappedType,LogType) { get }
}

extension WrapperType where Self: WriterType {
	init(_ value: WrappedType) {
		self.init(value, LogType.empty)
	}
}

/// Data
struct Writer<Wrapped,Log: Monoid>: WriterType {
	typealias WrappedType = Wrapped
	typealias LogType = Log

	let value: Wrapped
	let info: Log
	init(_ value: WrappedType, _ info: LogType) {
		self.value = value
		self.info = info
	}

	var runWriter: (WrappedType, LogType) {
		return (value, info)
	}
}

/// Monad
extension WriterType {
	func flatMap <
		OtherType,
		OtherWriterType: WriterType
		where
		OtherWriterType.WrappedType == OtherType,
		OtherWriterType.LogType == LogType
		>
		(@noescape transform: WrappedType -> OtherWriterType) -> Writer<OtherType,LogType> {
		let (value,info) = runWriter
		let (otherValue,otherInfo) = transform(value).runWriter
		return Writer(otherValue,info.compose(otherInfo))
	}
}
/*:
### Reader
*/
/// Basic defintion
protocol ReaderType: WrapperType {
	associatedtype EnvironmentType
	init(_ function: EnvironmentType -> WrappedType)
	func runReader(environment: EnvironmentType) -> WrappedType
}

extension WrapperType where Self: ReaderType {
	init(_ value: WrappedType) {
		self.init { _ in value }
	}
}

/// Data
struct Reader<Wrapped,Environment>: ReaderType {
	typealias WrappedType = Wrapped
	typealias EnvironmentType = Environment

	let function: EnvironmentType -> WrappedType
	init(_ function: EnvironmentType -> WrappedType) {
		self.function = function
	}

	func runReader(environment: EnvironmentType) -> WrappedType {
		return function(environment)
	}
}

/// Monad
extension ReaderType {
	func flatMap <
		OtherType,
		OtherReaderType: ReaderType
		where
		OtherReaderType.WrappedType == OtherType,
		OtherReaderType.EnvironmentType == EnvironmentType
		>
		(transform: WrappedType -> OtherReaderType) -> Reader<OtherType,EnvironmentType> {
		return Reader { environment in
			transform(self.runReader(environment)).runReader(environment)
		}
	}
}
/*:
## Monad transformers

Monads are pretty useful to represent specific compuations in a precise and reusable way, but in real world case we would probably end up mixing more monads in series of functions. For example, we could use a `Writer` in a certain situation, but we also need to wrap a `Either` into the `Writer`, because the wrapped value could be an error.

The result type would be something like `Writer<Either<Wrapped>,Log>`: now, if we just `flatMap` such `Writer` we would end up manipulating a `Either`, but we probably want to pass to `flatMap` a function of type `Wrapped -> Writer<Either<Wrapped>,Log>`, which means that we would love to directly operate with the wrapped value, instead of unwrapping and rewrapping `Either` each time.

The solution is *monad transformers*, which in Haskell are types themselves, with specific semantics: for example, `EitherT` is a monad transformer (and a monad itself) that can represent **any** monad wrapping a `Either`: the point is that, to work out the wrapping-unwrapping machinery, we only need two ingredients:

- a monad acting as *wrapper*, that is, something that can be initialized with a value and can be called `flatMap` on;
- a `Either` acting as the *wrapped* value, something that we can manipulate at low level;

That's why in Haskell we can define a monad transformer representing *any* monad wrapping an `Either`, and call it `EitherT`: because about the wrapper we only need to know that is a monad, but we need to know the *specifics* of `Either`.

Unfortunately, in Swift we cannot generically define something to be a monad, or in other terms, a *flatMappable*, because the language doesn't feature *higher-kinded types*: in other terms, in Swift we cannot express something like "this type is a monad, wrapping a value of subtype A, and I can flatMap it into a value of the same type but with subtype B". Thus, we actually need to implement the transformer methods for **any** possible wrapper of `Either`.
*/
extension WriterType where WrappedType: EitherType {
	func flatMapT <OtherType> (transform: WrappedType.WrappedType -> Writer<Either<OtherType>,LogType>) -> Writer<Either<OtherType>,LogType> {
		return flatMap { (either) -> Writer<Either<OtherType>,LogType> in
			either.runEither(
				ifRight: { (value) -> Writer<Either<OtherType>,LogType> in
					return transform(value)
				},
				ifLeft: { (error) -> Writer<Either<OtherType>,LogType> in
					return Writer(Either(error))
			})
		}
	}
}
/*:
We just called `flatMap` on `self` (that is, `WriterType`), so we only needed a *flatMappable* as the wrapper, but we called `runEither` on the wrapped type, so we needed the specifics of the either to make this work. Now, to do the same thing with every other *MonadType* wrapper (inluding `EitherType`) we just need to copy and paste the `flatMapT` code and change `WriterType` with *MonadType*.

Let's see now what happens when a `Reader` wraps an `Optional`.
*/
extension ReaderType where WrappedType: OptionalType {
	func flatMapT <OtherType> (transform: WrappedType.WrappedType -> Reader<Optional<OtherType>,EnvironmentType>) -> Reader<Optional<OtherType>,EnvironmentType> {
		return flatMap { (wrapped) -> Reader<Optional<OtherType>,EnvironmentType> in
			Reader { environment in
				wrapped.flatMap { value in
					transform(value).runReader(environment)
				}
			}
		}
	}
}
/*:
Notice how things are different this time around: we start by calling `flatMap` on the wrapper, like in the `Writer<Either>` case, but we are actually just calling `flatMap` on `Optional` (so it could be any monad), and we are using the internals of `ReaderType` (that is, `runReader`). The way a *Reader transformer* works is exactly the opposite of an Either transformer*: we use it when the Reader wraps something else, while we use the *Either transformer* when an Either is wrapped by something else.

The reason for this is that a `Reader`, along with many other monads, yields its wrapped value by calling a function, which means that to extract the value we must create a new `Reader` and use the new context: at this point we don't need to extract the value from the wrapped value, we can just call `flatMap`, produce the new *wrapper*, then call *run* on it with the aforementioned context.

I think it's enought for this discussion: starting from this concepts, we can define any trasformer for any monad, and with that we can actually use monads as an everyday development tool. With just basic, idiomatic Swift we already use `Optional` constantly, but there's many of them that can be used: we actually already use a monad transformer, becuase `Array` is also a monad, and when we have an array of optionals, the standard library defines a special `flatMap` method (on `SequenceType`) that takes a function with the Optional's wrapped value as the input: to be a *real* `flatMapT` this function should return a `Array<Optional<OtherType>>`, but instead it just returns `Optional<OtherType>`: also, it filters the optionals from the sequence, so it's like it actually calls `filter` after flatMapping; and, to be precise, the method is not really going to `flatMap` the optionals: if the collection contains optionals, you need to unwrap them. From a semantic standpoint, it would be much better to have an actual transformer in the library, and a `filterSome` method to remove the optionals which value is nil: anything would be much clearer, with better semantics.

We can only hope that precision in denotational semantics is a considered goal for future Swift versions.
*/
