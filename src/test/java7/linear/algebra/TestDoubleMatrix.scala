package linear.algebra

import java.lang.Double

import com.google.common.collect.Lists
import org.scalatest.{MustMatchers, WordSpecLike}
import linear.algebra.{DoubleMatrix => JDoubleMatrix, Matrix => JMatrix}

import scala.util.Random

/**
  * Created by oltyan on 2017.01.01.
  */
class TestDoubleMatrix extends WordSpecLike with MustMatchers {
  "A non degenerative double matrix " must {
    "be the same when multiplied with identity matrix" in {
      val m = new JDoubleMatrix(Array(
        Array(1.0, 1.0, 1.0, 1.0, 1.0),
        Array(1.0, 1.0, 1.0, 1.0, 1.0),
        Array(1.0, 1.0, 1.0, 1.0, 1.0),
        Array(1.0, 1.0, 1.0, 1.0, 1.0),
        Array(1.0, 1.0, 1.0, 1.0, 1.0)))
      val eye5 = new JDoubleMatrix(Array(
        Array(1.0, 0.0, 0.0, 0.0, 0.0),
        Array(0.0, 1.0, 0.0, 0.0, 0.0),
        Array(0.0, 0.0, 1.0, 0.0, 0.0),
        Array(0.0, 0.0, 0.0, 1.0, 0.0),
        Array(0.0, 0.0, 0.0, 0.0, 1.0)))
      val res: JMatrix[Double] = m.product(eye5)
      assert(res == m)
    }
  }

  "An arbitrary matrix" must {
    "be properly changed multiplied by an arbitrary scalar (between 1 and 5)" in {
      val m = new JDoubleMatrix(Array(
        Array(0.0, 0.0, 0.0, 0.0),
        Array(0.0, 2.0, 3.0, 4.0),
        Array(0.0, 2.0, 4.0, 8.0),
        Array(0.0, 3.0, 6.0, 9.0)))
      val n = ((Random.nextInt % 5) + 1).toDouble
      val expected = new JDoubleMatrix(Array(
        Array(0.0 * n, 0.0 * n, 0.0 * n, 0.0 * n),
        Array(0.0 * n, 2.0 * n, 3.0 * n, 4.0 * n),
        Array(0.0 * n, 2.0 * n, 4.0 * n, 8.0 * n),
        Array(0.0 * n, 3.0 * n, 6.0 * n, 9.0 * n)))
      val res = m.product(n);
      assert(expected == m)
    }
    "be properly changed added with a matrix with same dimensions" in {
      val m1 = new JDoubleMatrix(Array(
        Array(1.0, 2.0, 3.0),
        Array(2.0, 4.0, 9.0)
      ))
      val m2 = new JDoubleMatrix(Array(
        Array(-1.0, -2.0, -3.0),
        Array(-2.0, -4.0, -9.0)
      ))
      val res = m2.add(m1)
      val expected = new JDoubleMatrix(Array(
        Array(0.0, 0.0, 0.0),
        Array(0.0, 0.0, 0.0)
      ))
      assert(res == expected)
    }
    "be properly replaced the columns and rows (mirrored) by calling transpose on it" in {
      val m1 = new JDoubleMatrix(Array(
        Array(1.0, 2.0, 3.0),
        Array(2.0, 4.0, 9.0)
      ))
      val expected = new JDoubleMatrix(Array(
        Array(1.0, 2.0),
        Array(2.0, 4.0),
        Array(3.0, 9.0)
      ))
      assert(m1.transpose() == expected)
    }
    "result a proper matrix if we multiple it with another matrix that has proper dimensions" in {
      /*
       * |0 1 2 3 4|   |0 1 2|
       * |1 2 3 4 5| * |1 2 3|    |30 40 50|
       *               |2 3 4|  = |40 55 70|
       *               |3 4 5|
       *               |4 5 6|
       */
      val m1 = new DoubleMatrix(Array(
        Array(0.0, 1.0, 2.0, 3.0, 4.0),
        Array(1.0, 2.0, 3.0, 4.0, 5.0)
      ))
      val m2 = new DoubleMatrix(Array(
        Array(0.0, 1.0, 2.0),
        Array(1.0, 2.0, 3.0),
        Array(2.0, 3.0, 4.0),
        Array(3.0, 4.0, 5.0),
        Array(4.0, 5.0, 6.0)
      ))
      val expected = new DoubleMatrix(Array(
        Array(30.0, 40.0, 50.0),
        Array(40.0, 55.0, 70.0)
      ))
      assert(m1.product(m2) == expected)
    }
    "result None if we multiple it with another matrix that has inappropriate dimensions" in {
      val m1 = new DoubleMatrix(Array(
        Array(0.0, 1.0, 2.0, 3.0, 4.0),
        Array(1.0, 2.0, 3.0, 4.0, 5.0)
      ))
      val m2 = new DoubleMatrix(Array(
        Array(0.0, 1.0, 2.0, 3.0, 4.0),
        Array(1.0, 2.0, 3.0, 4.0, 5.0)
      ))
      assertThrows[RuntimeException](m1.product(m2))
    }
    "give back any of an extracted matrix by explicit row and column indices" in {
      val m1 = new DoubleMatrix(Array(
        Array(0.0, 1.0, 2.0, 3.0, 4.0, 5.0),
        Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
        Array(2.0, 3.0, 4.0, 5.0, 6.0, 7.0),
        Array(3.0, 4.0, 5.0, 6.0, 7.0, 8.0),
        Array(4.0, 5.0, 6.0, 7.0, 8.0, 9.0),
        Array(5.0, 6.0, 7.0, 8.0, 9.0, 10.0),
        Array(6.0, 7.0, 8.0, 9.0, 10.0, 11.0),
        Array(7.0, 8.0, 9.0, 10.0, 11.0, 12.0)
      ))
      val extractedOddCells = m1.extract(
        Lists.newArrayList(0,2,4,6),
        Lists.newArrayList(0,2,4))
      val expected = new DoubleMatrix(Array(
        Array(0.0, 2.0, 4.0),
        Array(2.0, 4.0, 6.0),
        Array(4.0, 6.0, 8.0),
        Array(6.0, 8.0, 10.0)
      ))
      assert(expected == extractedOddCells)
    }
  }
}
