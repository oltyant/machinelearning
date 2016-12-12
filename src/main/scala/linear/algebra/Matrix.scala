package linear.algebra

import Numeric.Implicits._

/**
  * Created by oltyant on 2016. 12. 10..
  */
package object matrix {
  type Vect[A] = Vector[A]
  type Matrix[A] = Vector[Vect[A]]

  def identity[A: Numeric](n: Int): Matrix[A] = Matrix(n, n) {
    (i, j) => if (i == j) implicitly[Numeric[A]].one else implicitly[Numeric[A]].zero
  }

  def product[A : Numeric](v1: Vect[A], v2: Vect[A]): A = (0 until v1.size).foldRight(implicitly[Numeric[A]].zero)((i, acc) => v1(i) * v2(i) + acc)

  def transpose[A : Numeric](m: Matrix[A]): Matrix[A] = {
    if (m.head.isEmpty) Matrix.empty
    else Matrix[A](m.colCount, m.rowCount) {
      (i, j) => m(j)(i)
    }
  }

  def product[A : Numeric](m1: Matrix[A], m2: Matrix[A]): Matrix[A] = {
    m1.map(row1 => transpose(m2).map(col2 => product(row1, col2)))
  }

  def det[A : Numeric](m: Matrix[A], n: Int): Option[A] = {
    val one  = implicitly[Numeric[A]].one
    val zero = implicitly[Numeric[A]].zero
    val power = (x: A, y: Int) => (0 until y).foldLeft(one)((acc, a) => acc * x * x)

    if (m.colCount != m.rowCount) None
    else if (n < 1) None
    else if (n == 1) Some(m(0)(0))
    else if (n == 2) Some(m(0)(0) * m(1)(1) - m(1)(0) * m(0)(1))
    else {
      var res = zero
      var j2 = 0
      var helperM = Matrix(n - 1, n - 1)((i, j) => zero)
      (0 until n).foreach {
        j1 => (1 until n).foreach {
          i => j2 = 0
            (0 until n).foreach {
              j => if (j != j1) {
                helperM = helperM.updated(i - 1, helperM(i - 1).updated(j2, m(i)(j)))
                j2 += 1
              }
            }
        }
          res += det(helperM, n - 1).map(power(-one, j1 + 2) * m(0)(j1) * _).getOrElse(zero)
      }
      Some(res)
    }
  }

  def cofact[A : Numeric](m: Matrix[A], n: Int): Option[Matrix[A]] = {
    val one  = implicitly[Numeric[A]].one
    val zero = implicitly[Numeric[A]].zero
    val power = (x: A, y: Int) => (0 until y).foldLeft(one)((acc, a) => acc * x * x)

    if (m.length != m.head.length) None
    else {
      var helperM = Matrix(n - 1, n - 1)((i, j) => zero)
      var res = Matrix(n, n)((i, j) => zero)
      (0 until n).foreach {
        j => (0 until n).foreach {
          i => {
            var i1 = 0
            (0 until n).foreach {
              ii => if (ii != i) {
                var j1 = 0
                (0 until n).foreach {
                  jj => if (jj != j) {
                    helperM = helperM.updated(i1, helperM(i1).updated(j1, m(ii)(jj)))
                    j1 += 1
                  }
                }
                i1 += 1
              }
            }
            val determinant = det(helperM, n - 1)
            res = res.updated(i, res(i).updated(j, power(-one, i + j + 2) * determinant.getOrElse(zero)))
          }
        }
      }
      Some(res)
    }
  }

  def adjugant[A : Numeric](m: Matrix[A], n: Int): Option[Matrix[A]] = cofact(m, n).map(cofm => transpose[A](cofm))

  implicit def m2RichMatrix[A : Numeric](m: Matrix[A]): RichMatrix[A] = RichMatrix(m)
  implicit def RichMatrix2m[A : Numeric](rm: RichMatrix[A]): Matrix[A] = rm.m

  case class RichMatrix[A : Numeric](m: Matrix[A]) {
    val rowCount = m.length
    val colCount = m.head.length
    val one  = implicitly[Numeric[A]].one
    val zero = implicitly[Numeric[A]].zero
    lazy val T = transpose(m)
    lazy val I = if (isSquare) Some(identity(rowCount)) else None
    lazy val isSquare = (rowCount==colCount)
    lazy val determinant = if (isSquare) det(this.m, rowCount) else None
    lazy val cofactor = if (isSquare) cofact(this.m, rowCount) else None
    lazy val adj = if (isSquare) adjugant(this.m, rowCount) else None
    lazy val inv: Option[Matrix[Double]] = if (isSquare) adj.map(a => Matrix.scalar(determinant.map(d => 1.0 / d.toDouble()).getOrElse(0.0), a.map(_.map(_.toDouble)))) else None

    def checkDims[B](that: RichMatrix[B]): Boolean = that.colCount == this.colCount && that.rowCount == this.rowCount

    def *(that: RichMatrix[A]): Option[Matrix[A]] = if (this.colCount != that.rowCount) None else Some(product(this.m, that.m))

    def +(that: RichMatrix[A]): Option[Matrix[A]] =
      if (checkDims(that)) Some {
        Matrix(this.rowCount, this.colCount)((i, j) => this.m(i)(j) + that.m(i)(j))
      } else None

    override def equals(obj: scala.Any): Boolean = obj match {
      case mm : RichMatrix[Double] if checkDims(mm) => Matrix(rowCount, colCount) {
        (i, j) => Math.abs(m(i)(j).toDouble() - mm(i)(j))
      }.forall(_.foldLeft(true)(_ && _ < 0.0000001))
      case mm : RichMatrix[Float] if checkDims(m) => Matrix(rowCount, colCount) {
        (i, j) => Math.abs(m(i)(j).toFloat() - mm(i)(j))
      }.forall(_.foldLeft(true)(_ && _ < 0.0000001f))
      case mm : RichMatrix[A] if checkDims(m) => Matrix(rowCount, colCount) {
        (i, j) => m(i)(j) - mm(i)(j)
      }.forall(_.foldLeft(true)(_ && _ == zero))
      case _ => false
    }

    def unary_- = Matrix.scalar(-one, this.m)
  }

  object Matrix {
    def apply[A : Numeric](rowCount: Int, colCount: Int)(f: (Int, Int) => A): Matrix[A] =
      (0 until rowCount).map(i => (0 until colCount).map(j => f(i, j)).toVector).toVector

    def apply[A : Numeric](rowCount: Int)(f: (Int) => Vect[A]): Matrix[A] =
      (0 until rowCount).map(i => f(i)).toVector

    def scalar[A : Numeric](a: A, m: Matrix[A]): Matrix[A] = Matrix.apply[A](m.rowCount, m.colCount)((i, j) => a * m(i)(j))

    def empty[A : Numeric]: Matrix[A] = Vector.empty[Vect[A]]
  }

}



