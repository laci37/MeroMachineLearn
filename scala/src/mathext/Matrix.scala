package mathext

class Matrix(val rows: Int, val cols: Int) {
  //init arrays
  protected val arr = new Array[Array[Double]](rows)
  for (i <- (0 to rows - 1)) arr(i) = new Array[Double](cols)

  override def clone() = {
    val res = new Matrix(rows, cols)
    for (i <- (0 to rows - 1)) {
      System.arraycopy(arr(i), 0, res.arr(i), 0, cols)
    }
    res
  }

  def apply(row: Int) = new Vector(arr(row))

  def apply(row: Int, col: Int) = arr(row)(col)

  def update(row: Int, col: Int, value: Double) = arr(row)(col) = value

  def +=(that: Matrix) = {
    if (that.cols != this.cols || that.rows != this.rows)
      throw new MatrixException("Dimensions should be equal")
    for (i <- (0 to rows - 1)) for (j <- (0 to cols - 1)) {
      this(i, j) += that(i, j)
    }
    this
  }

  def -=(that: Matrix) = {
    if (that.cols != this.cols || that.rows != this.rows)
      throw new MatrixException("Dimensions should be equal")
    for (i <- (0 to rows - 1)) for (j <- (0 to cols - 1)) {
      this(i, j) -= that(i, j)
    }
    this
  }
  
  def *=(that:Double)={
    for (i <- (0 to rows - 1)) for (j <- (0 to cols - 1)) {
      this(i, j) *= that
    }
    this
  }

  def ×(that: Matrix): Matrix = {
    if (that.rows != this.cols)
      throw new MatrixException("Dimensions should be equal")
    val res = new Matrix(this.rows, that.cols)
    for (i <- (0 to rows - 1)) for (j <- (0 to that.cols - 1)) for (k <- (0 to cols - 1)) {
      res(i,j)+=this(i,k)*that(k,j)
    }
    res
  }
  
  def +(that:Matrix)={
    this.clone+=that
  }
  
  def -(that:Matrix)={
    this.clone-=that
  }
  
  def *(that:Double)={
    this.clone *= that
  }
  
  
}

class MatrixException(val msg: String) extends Exception(msg)