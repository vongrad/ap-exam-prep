
package adpro.exam2016

import scala.annotation.tailrec

object Q1 {
  def checksumImp (in: String) :Int = {
    var result = 0
    for (c <- in.toList)
      result = (result + c.toInt) % 0xffff
    return result
  }

  // Write your answer for Task 2 here.
  def checksumFun (in :String) :Int = in.toList.foldRight(0) ((c, z) => (z + c.toInt) % 0xffff)

  def checksumImp2 (in: String): Int = {
    @tailrec
    def go(l: List[Char], acc: Int): Int = l match {
      case Nil => acc
      case h::t => go(t, (acc + h.toInt) % 0xffff) // Tail recursive
    }
    go(in.toList, 0)
  }
}


object Q2 {
  import fpinscala.monads.Functor
  import scala.language.higherKinds

  def onList[A] (f: A => A) :List[A] => List[A] = l => l.map(f) // Task 3.

  def onCollection[C[_],A] (f: A => A) (implicit functorC: Functor[C]) :C[A] => C[A] = c => functorC.map(c)(f) // Task 4.

}

object Q3 {

  import fpinscala.monoids.Monoid
  import scala.language.higherKinds

  def foldBack[A] (l :List[A]) (implicit M :Monoid[A]) :A = (l++l.reverse).fold(M.zero)(M.op) // Task 5.

}

object Q4 {

  type Computation[A] = A => Either[A,String]

  // Task 6.
  def run[A] (init: A) (progs: List[Computation[A]]): (A,List[String]) = progs.foldLeft((init, List.empty[String])) ((z, a) => {
    a(z._1) match {
      case Left(v) => (v, z._2)
      case Right(e) => (z._1, e::z._2)
    }
  })
}


object Q5 {

  sealed trait Tree[A]
  case class Branch[A] (l: () => Tree[A], a: A, r:() => Tree[A]) extends Tree[A]
  case class Leaf[A] (a: A) extends Tree[A]

  // Task 7.
  def multiply (t: Tree[Int]) :Int = t match {
    case Leaf(a) => a
    case Branch(l, a, r) => if (a == 0) 0 else {
      val lv = multiply(l())
      if (lv == 0) 0 else {
        val rv = multiply(r())
        lv * a * rv
      }
    }
  }

  // Task 8. (answer below in a comment)
  // I would write a series of tests, especially property tests where I would fill the nodes with exceptions that would get thrown if the node was evaluated
  // Then if I initialized the tree, I should not get any exception if the tree is correctly eager
  // I would also write severalscenario tests where I would make sure that the computation is correct

}

object Q6 {

  sealed trait Nat[+A]
  case object Zero extends Nat[Unit]
  case class Succ[A] (pred: A) extends Nat[A]

  val zero: Nat[Unit] = Zero            // Task 9.
  val one: Nat[Nat[Unit]] = Succ (zero)     // Task 9.
  val two: Nat[Nat[Nat[Unit]]] = Succ (one)      // Task 9.


  def plus2[A]  (x : Nat[A]) : Nat[Nat[Nat[A]]]  = Succ(Succ(x))        // Task 10.

}