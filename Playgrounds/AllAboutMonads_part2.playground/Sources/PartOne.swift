public enum Maybe<A> {
	case Nothing
	case Just(A)
}

extension Maybe {
	public static func unit(value: A) -> Maybe<A> {
		return .Just(value)
	}
}

extension Maybe {
	public func bind <B> (transform: A -> Maybe<B>) -> Maybe<B> {
		switch self {
		case .Nothing:
			return .Nothing
		case let .Just(value):
			return transform(value)
		}
	}
}

infix operator >>- { associativity left precedence 140 }
public func >>- <A,B> (lhs: Maybe<A>, rhs: A -> Maybe<B>) -> Maybe<B> {
	return lhs.bind(rhs)
}

extension Optional {
	public static func unit (value: Wrapped) -> Optional {
		return .Some(value)
	}

	public func bind <B> (transform: Wrapped -> B?) -> B? {
		switch self {
		case .None:
			return .None
		case let .Some(value):
			return transform(value)
		}
	}
}

public func >>- <A,B> (lhs: A?, rhs: A -> B?) -> B? {
	return lhs.bind(rhs)
}

public func >>- <A,B>(lhs: [A], rhs: A -> [B]) -> [B] {
	return lhs.flatMap(rhs)
}

public struct Identity<A> {
	let value: A
	public init(_ value: A) {
		self.value = value
	}

	public func runIdentity() -> A {
		return value
	}

	public static func unit(x: A) -> Identity<A> {
		return Identity(x)
	}

	public func bind <B> (transform: A -> Identity<B>) -> Identity<B> {
		return transform(runIdentity())
	}
}

public func >>- <A,B> (lhs: Identity<A>, rhs: A -> Identity<B>) -> Identity<B> {
	return lhs.bind(rhs)
}

public struct Operation<A> {
	let computation: () -> A
	public init(_ value: () -> A) {
		self.computation = value
	}

	public func runOperation() -> A {
		return computation()
	}

	public static func unit(value: A) -> Operation<A> {
		return Operation { value }
	}

	public func bind <B> (transform: A -> Operation<B>) -> Operation<B> {
		return Operation<B> {
			return transform(self.runOperation()).runOperation()
		}
	}
}

public func >>- <A,B> (lhs: Operation<A>, rhs: A -> Operation<B>) -> Operation<B> {
	return lhs.bind(rhs)
}

extension Array {
	public static func unit (value: Generator.Element) -> [Generator.Element] {
		return [value]
	}

	public func bind <B> (transform: Generator.Element -> [B]) -> [B] {
		return flatMap(transform)
	}
}

public enum Either<A> {
	case Left(ErrorType)
	case Right(A)

	public static func unit (value: A) -> Either<A> {
		return .Right(value)
	}

	public func bind <B> (transform: A -> Either<B>) -> Either<B> {
		switch self {
		case let .Left(error):
			return Either<B>.Left(error)
		case let .Right(value):
			return transform(value)
		}
	}

	public func get() throws -> A {
		switch self {
		case let .Left(error):
			throw error
		case let .Right(value):
			return value
		}
	}

	public func getOrCatch (catchError: ErrorType -> A) -> A {
		switch self {
		case let .Left(error):
			return catchError(error)
		case let .Right(value):
			return value
		}
	}
}

public func >>- <A,B> (lhs: Either<A>, rhs: A -> Either<B>) -> Either<B> {
	return lhs.bind(rhs)
}

public struct Unit: Equatable {
	public let get: () = ()
	public init() {}
}

public func == (lhs: Unit, rhs: Unit) -> Bool {
	return true
}

public struct State<A,S> {
	let computation: S -> (A,S)
	public init(computation: S -> (A,S)) {
		self.computation = computation
	}

	public func runState(state: S) -> (A,S) {
		return computation(state)
	}

	public static func unit(value: A) -> State<A,S> {
		return State { state in (value,state) }
	}

	public func bind<B>(transform: A -> State<B,S>) -> State<B,S> {
		return State<B,S> { state in
			let (newValue,newState) = self.runState(state)
			let newStateMonad = transform(newValue)
			return newStateMonad.runState(newState)
		}
	}

	public static func get() -> State<S,S> {
		return State<S,S> { state in (state,state) }
	}

	public static func put(newState: S) -> State<Unit,S> {
		return State<Unit,S> { _ in (Unit(),newState) }
	}

	public static func getS(transform: S -> A) -> State<A,S> {
		return State<A,S> { state in (transform(state),state) }
	}

	public static func modify(transform: S -> S) -> State<Unit,S> {
		return State<Unit,S> { state in (Unit(),transform(state)) }
	}
}

public func >>- <A,B,S> (lhs: State<A,S>, rhs: A -> State<B,S>) -> State<B,S> {
	return lhs.bind(rhs)
}

public struct Reader<A,E> {
	let function: E -> A
	public init(_ function: E -> A) {
		self.function = function
	}

	public func runReader(environment: E) -> A {
		return function(environment)
	}

	public static func unit(value: A) -> Reader<A,E> {
		return Reader<A,E> { _ in value }
	}

	public func bind <B> (transform: A -> Reader<B,E>) -> Reader<B,E> {
		return Reader<B,E> { environment in
			transform(self.runReader(environment)).runReader(environment)
		}
	}

	public static func ask() -> Reader<E,E> {
		return Reader<E,E> { $0 }
	}

	public func local(transform: E -> E) -> Reader<A,E> {
		return Reader<A,E> { self.runReader(transform($0)) }
	}
}

public func >>- <A,B,E> (lhs: Reader<A,E>, rhs: A -> Reader<B,E>) -> Reader<B,E> {
	return lhs.bind(rhs)
}

public protocol Monoid {
	static var empty: Self { get }
	func operation(other: Self) -> Self
}

public struct Writer<A,L: Monoid> {
	let function: () -> (A,L)
	public init(_ function: () -> (A,L)) {
		self.function = function
	}

	public func runWriter() -> (A,L) {
		return function()
	}

	public static func unit(value: A) -> Writer<A,L> {
		return Writer { (value,L.empty) }
	}

	public func bind<B>(transform: A -> Writer<B,L>) -> Writer<B,L> {
		return Writer<B,L> {
			let (oldValue,oldLog) = self.runWriter()
			let (newValue,newLog) = transform(oldValue).runWriter()
			return (newValue,oldLog.operation(newLog))
		}
	}

	public func tell(newLog: L) -> Writer<A,L> {
		return Writer<A,L> {
			let (oldValue,oldLog) = self.runWriter()
			return (oldValue,oldLog.operation(newLog))
		}
	}

	public func listen() -> Writer<(A,L),L> {
		return Writer<(A,L),L> {
			let (oldValue,oldLog) = self.runWriter()
			return ((oldValue,oldLog),oldLog)
		}
	}

	public func censor(transform: L -> L) -> Writer<A,L> {
		return Writer<A,L> {
			let (oldValue,oldLog) = self.runWriter()
			return (oldValue,transform(oldLog))
		}
	}
}

public func >>- <A,B,L> (lhs: Writer<A,L>, rhs: A -> Writer<B,L>) -> Writer<B,L> {
	return lhs.bind(rhs)
}
