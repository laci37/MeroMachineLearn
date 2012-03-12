package nga

abstract class Generation(val members: Traversable[Genome]) {
  def generate(): this.type
}