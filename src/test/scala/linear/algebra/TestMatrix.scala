package linear.algebra.matrix

import org.scalatest.{MustMatchers, WordSpecLike}

import scala.util.Random

/**
  * Created by oltyant on 2016. 12. 10.
  */
class TestMatrix extends WordSpecLike with MustMatchers {

  "A non degenerative matrix " must {
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
      val res = m * n
      val invertRes = res.map(r => r.map(c => (c / n)))
      assert(invertRes == m)
    }
    "be properly changed added with a matrix with same dimensions" in {
      val m1 = Matrix(4, 5)(_ + _)
      val m2 = Matrix(4, 5)(_ - _)
      val res = m2 + m1
      val expected = Matrix(4, 5)((i, _) => 2 * i)
      assert(res.map(r => r == expected).getOrElse(false))
    }
    "be properly replaced the columns and rows (mirrored) by calling transpose on it" in {
      val m1 = Matrix(4,5)(_ + _)
      val expected = Matrix(5,4)(_ + _)
      assert(transpose[Int](m1) == expected)
    }
    "result a proper matrix if we multiple it with another matrix that has proper dimensions" in {
      /*
       * |0 1 2 3 4|   |0 1 2|
       * |1 2 3 4 5| * |1 2 3|    |30 40 50|
       *               |2 3 4|  = |40 55 70|
       *               |3 4 5|
       *               |4 5 6|
       */
      val m1 = Matrix(2,5)(_ + _)
      val m2 = Matrix(5,3)(_ + _)
      val expected = Vector[Vector[Int]] (
        Vector(30, 40, 50),
        Vector(40, 55, 70)
      )
      assert {
        (m1 * m2).forall(_ == expected)
      }
    }
    "result None if we multiple it with another matrix that has inappropriate dimensions" in {
      val m1 = Matrix(2,5) (_ + _)
      val m2 = Matrix(4,3) (_ + _)
      val expected: Option[Matrix[Int]] = None
      assert {
        (m1 * m2).forall(_ == expected)
      }
    }
    "have a determinant if it is a square matrix" in {
      val m1 = Matrix(2,2)(_ + _)
      val expected1 = -1
      //val m2 = Matrix(3,3)(_ + _)
      //val expected2 = 0
      assert {
          det[Int](m1, 2).forall(_ == expected1) //&&
          //det[Int](m2, 3).forall(_ == expected2)
      }
    }
    "have None as determinant if it is NOT a square matrix" in {
      val m1 = Matrix(2,3)(_ + _)
      val expected: Option[Int] = None
      assert(det[Int](m1, 2).forall(_ == expected))
    }
    "give back any of an extracted matrix by explicit row and column indices" in {
      val n = 6
      val m = 8
      val m1 = Matrix(n, m)(_ + _)
      val extractedOddCells = extract[Int](m1,
          (0 until n).toVector.filter(_ % 2 == 0),
          (0 until m).toVector.filter(_ % 2 == 0))
      val expected = Vector(
        Vector(0,2,4,6),
        Vector(2,4,6,8),
        Vector(4,6,8,10)
      )
      assert(expected == extractedOddCells)
    }
  }

  "An identity matrix" must {
    "be constructed based on a simple integer properly" in {
      val n = Random.nextInt(20)
      val expected = Matrix(n, n)((i, j) => if (i == j) 1 else 0)
      assert(expected == identity[Int](n))
    }
  }

  "Two vectors' (n x 1 dimensional matrix) product" must {
    "give back the proper number" in {
      val v1 = Vector(1,2,3,4,5)
      val v2 = Vector(5,4,3,2,1)
      val expected = 35
      assert(expected == product[Int](v1, v2))
    }
  }
}
