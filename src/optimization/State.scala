package optimization

abstract class State {
	def cost():Double
	def neighbor():State
	//def acceptRise():Double
	
}