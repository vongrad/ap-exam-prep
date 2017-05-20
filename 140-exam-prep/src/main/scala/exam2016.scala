
package adpro.exam2016

object Q1 {
   def checksumImp (in: String) :Int = {
    var result = 0
    for (c <- in.toList)
      result = (result + c.toInt) % 0xffff
    return result
  }

  def checksumFun (in :String) :Int = in.toList.foldLeft(0)((z, c) => (z + c.toInt) % 0xffff)

  // Write your answer for Task 2 here.
}


object Q2 {
  import fpinscala.monads.Functor
  import scala.language.higherKinds

  def onList[A] (f: A => A) :List[A] => List[A] = (l: List[A]) => l.map(f)

  def onCollection[C[_],A] (f: A => A) (implicit functorC: Functor[C]) :C[A] => C[A] = c => functorC.map(c)(f)

}

object Q3 {

  import fpinscala.monoids.Monoid
  import scala.language.higherKinds

  def foldBack[A] (l :List[A]) (implicit M :Monoid[A]) :A = (l ++ l.reverse).foldRight(M.zero)(M.op)

}

object Q4 {

  type Computation[A] = A => Either[A,String]

  def run[A] (init: A) (progs: List[Computation[A]]): (A,List[String]) =
    progs.foldLeft((init, List.empty[String]))((z, c) => c(z) match {
      case Left(a) => (a, z._2)
      case Right(err) => (z._1, err::z._2)
    })
    // TODO: reverse errors
}


object Q5 {

  sealed trait Tree[A]
  case class Branch[A] (l: () => Tree[A], a: A, r:() => Tree[A]) extends Tree[A]
  case class Leaf[A] (a: A) extends Tree[A]

  def multiply (t: Tree[Int]) :Int = t match {
    case Leaf(a) => a
    case Branch(l, a, r) => {
      if (a == 0) 0 else {
        val vl = multiply(l())
        if (vl == 0) 0 else {
          val vr = multiply(r())
          if (vr == 0) 0 else vl * a * vr
        }
      }
    }
  }

  // Task 8. (answer below in a comment)

}
   
object Q6 {
   
  sealed trait Nat[+A]
  case object Zero extends Nat[Unit]
  case class Succ[A] (pred: A) extends Nat[A]

  val zero : Nat[Unit] = Zero
  val one : Nat[Nat[Unit]] = Succ (zero)
  val two : Nat[Nat[Nat[Unit]]] = Succ (one)


  def plus2[A] (x : Nat[A]): Nat[Nat[Nat[A]]] = Succ(Succ(x))

}
