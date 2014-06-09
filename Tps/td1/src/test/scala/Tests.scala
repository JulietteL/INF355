import org.scalatest._

import fr.enst.plnc2014.td1.TD1._
import fr.enst.plnc2014.td1.ExtSeq._
import fr.enst.plnc2014.td1.Complex
import fr.enst.plnc2014.td1.Complex._
import fr.enst.plnc2014.td1.ExtCond._

class Tests extends FlatSpec with Matchers {

 "isOdd" should "characterize numbers correctly" in {
   isOdd(0) should be (false)
   isOdd(1) should be (true)
 }

 it should "work on negative numbers" in {
   isOdd(-1) should be (true)
   isOdd(-2) should be (false)
 }


  "isEven" should "characterize numbers correctly" in {
    isEven(0) should be (true)
    isEven(1) should be (false)
  }

  it should "work on negative numbers" in {
    isEven(-1) should be (false)
    isEven(-2) should be (true)
  }



  "any" should "work with an empty list" in {
    Nil.any((_: Int) => true) should be (false)
  }

  it should "identify an odd number" in {
    List(1, 2, 3).any(isOdd) should be (true)
  }

  it should "not identify even numbers" in {
    List(2, 4, 6).any(isOdd) should be (false)
  }

  "all" should "work with an empty list" in {
    Nil.all((_: Int) => false) should be (true)
  }

  it should "work with all even numbers" in {
    List(2, 4, 6).all(isEven) should be (true)
  }

  it should "fail with an odd number in the list" in {
    List(2, 4, 5, 6).all(isEven) should be (false)
  }

  "myWhile" should "not evaluate actions when false" in {
    var c = 0
    myWhile(false, c += 1)
    c should equal (0)
  }

  it should "not evaluate action several times" in {
    var c = 0
    myWhile({c += 1; false}, c += 10)
    c should equal (1)
  }

  it should "evaluate actions while true" in {
    var c = 0
    var x = 0
    myWhile(x < 4, { c += x; x+= 1 })
    c should equal (6)
  }

  "doWhile" should "work with an automatic conversion" in {
    var x = 0
    var c = 0
    (x < 4) doWhile {
      c += x
      x += 1
    }
    c should equal(6)
  }

 "a Complex" should "print properly" in {
   Complex(0, 0).toString should equal("0.0")
   Complex(0, 1.2).toString should equal("1.2i")
   Complex(1.2, 0).toString should equal("1.2")
   Complex(1.2, 3.4).toString should equal("1.2+3.4i")
   Complex(1.2, -3.4).toString should equal("1.2-3.4i")
   Complex(0, -1.2).toString should equal("-1.2i")
 }

 it should "have a proper reciprocal" in {
   Complex(1.2, 3.4).reciprocal.toString should equal("1.2-3.4i")
 }

 it should "handle complex addition" in {
   Complex(1, 2) + Complex(3, 4) should equal(Complex(4, 6))
 }

 it should "handle addition with an integer" in {
   Complex(1, 2) + 3 should equal(Complex(4, 2))
   3 + Complex(1, 2) should equal(Complex(4, 2))
 }

 it should "handle complex multiplication" in {
   Complex(1,2) * Complex(3,4) should equal (Complex(-5, 10))
 }

 it should "handle complex division" in {
   Complex(2,4) / Complex(1, -1) should equal (Complex(-1,3))
}

 it should "have a proper abs" in {
   Complex(3,4).abs should equal (5)
}
//
//  "solveQueens" should "find the unique solution for 1 queen" in {
//    var l = List[(Int, Int)]()
//    solveQueens(1, l :::= _)
//    l should equal (List((1, 1)))
//  }
//
//  it should "not find any solution for 2 and 3 queens" in {
//    var n = 0
//    solveQueens(2, _ => n += 1)
//    solveQueens(3, _ => n += 1)
//    n should equal (0)
//  }
//
//  it should "find all solutions for 8 queens" in {
//    var n = 0
//    solveQueens(8, _ => n += 1)
//    n should equal (92)
//  }

}
