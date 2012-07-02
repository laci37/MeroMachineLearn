package nga
/**
 * represents a Genome, which can be decoded to an object of type T
 */
trait Decodable[+T] extends Genome {
  def decode():T
}