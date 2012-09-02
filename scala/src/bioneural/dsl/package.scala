package bioneural

package object dsl {
  implicit def FN2Extension(n:FNNeuron)={
    new FNExtension(n)
  }
}