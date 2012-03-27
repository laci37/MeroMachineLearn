package optimization

abstract class State extends Serializable{
	def cost():Double
	def neighbor():State
	//def acceptRise():Double
	
}