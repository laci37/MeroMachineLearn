package sa

abstract class State {
	def energy:Double
	def neighbor:State
}