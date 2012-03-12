package nga

abstract class Generation(val members: Traversable[Genome]) extends Iterator[Generation]{
  override def next(): Generation
  override def hasNext()=true
}