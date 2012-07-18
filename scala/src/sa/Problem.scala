package sa

abstract class Problem {
	def initialState:State
	def initialTemp:Double
	def probability(s1:State,s2:State,temp:Double):Double
	def cool(heat:Double):Double
	def cycles:Int
}