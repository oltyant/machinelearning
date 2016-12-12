package linear.algebra

import linear.algebra.matrix._
import org.scalatest.{MustMatchers, WordSpecLike}

import scala.util.Random

/**
  * Created by oltyant on 2016. 12. 10.
  */
class TestMatrix extends WordSpecLike with MustMatchers {

  "A non degenerate matrix " must {
    "be the same when multiplied with identity matrix" in {
      val m = Matrix(5, 5) {
        (_, _) => 1
      }
      val res: Option[Matrix[Int]] = m.I.flatMap(_ * m)
      assert {
        res match {
          case Some(a) => a == m
          case _ => false
        }
      }
    }
  }

  "An arbitrary matrix" must {
    "be properly changed multiplied by an arbitrary scalar (between 1 and 5)" in {
      val m = Matrix(4, 4) {
        (i, j) => (i * j % (i + j + 1))
      }
      val n = (Random.nextInt % 5) + 1
      val res = Matrix.scalar(n, m)
      val invertRes = res.map(r => r.map(c => (c / n)))
      assert(invertRes == m)
    }
    "be properly changed added with a matrix with same dimensions" in {
      val m1 = Matrix(4, 5) {
        (i, j) => i + j
      }
      val m2 = Matrix(4, 5) {
        (i, j) => i - j
      }
      val res = m2 + m1
      val expected = Matrix(4, 5) {
        (i, j) => 2 * i
      }
      assert(res.map(r => r == expected).getOrElse(false))
    }
  }
}
