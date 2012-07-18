package xor

import actors.Actor

object Input extends Actor {
  def act() = {
    loop {
      Evolver ! Console.readLine()
      receive {
        case _ => 
      }
    }
  }

}