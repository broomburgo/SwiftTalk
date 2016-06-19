import Foundation

public func randomIntegers(range range: Range<Int>, count: UInt) -> [Int] {
	guard count > 0 else { return [] }
	return (0..<count).map { _ in
		let offset: Int
		if range.startIndex < 0 {
			offset = abs(range.startIndex)
		} else {
			offset = 0
		}
		let min = UInt32(range.startIndex + offset)
		let max = UInt32(range.endIndex   + offset)
		return Int(min + arc4random_uniform(max - min)) - offset
	}
}
