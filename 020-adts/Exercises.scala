import scala.annotation.tailrec
// Advanced Programming 2017,
// A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // Exercise 3

  def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
    case Nil => Cons(newHead, Nil)
    case Cons(_, t) => Cons(newHead, t)
  }

  // Exercise 4

  @annotation.tailrec
  def drop[A] (l: List[A], n: Int) : List[A] = (n, l) match {
    case (_, Nil) => Nil
    case (0, l) => l
    case (n, Cons(_, t)) => drop(t, n - 1)
  }

  // Exercise 5

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  // Exercise 6

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) | Nil => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // Exercise 7

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

  def length[A] (as: List[A]): Int = foldRight(as, 0)((_, s: Int) => s + 1)

  // Exercise 8

  @tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 9

  def sum (as : List[Int]) : Int = foldLeft(as, 0)(_ + _)
  def product (as :List[Int]) : Int = foldLeft(as, 1)(_ * _)
  def length1 (as :List[Int]) : Int = foldLeft(as, 0)((a, _) => a + 1)

  // Exercise 10

  def reverse[A] (as :List[A]) :List[A] = foldLeft(as, Nil:List[A])((acc, a) => Cons(a, acc))

  // Exercise 11

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = {
    foldLeft(reverse(as), z)((z, a) => f(a, z))
  }

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = {
    foldRight[A, B => B](as, z => z)((a, g) => b => g(f(b, a)))(z)
  }

  // Exercise 12

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]) :List[A] = foldRight(as, Nil:List[A])(append)

  // Exercise 13

  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = foldRight(as, Nil:List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  // Exercise 14

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  // Exercise 15

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = flatMap(l)(a => if (p(a)) Cons(a, Nil) else Nil)

  // Exercise 16

  def add (l: List[Int]) (r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add(t1)(t2))
  }

  // Exercise 17

  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case ((Cons(h1, t1), Cons(h2, t2))) => Cons(f(h1, h2), zipWith(f)(t1, t2))
  }

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(_, t), _) => if (contains(sup, sub)) true else hasSubsequence(t, sub)
  }

  def contains[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) contains(t1, t2) else false
  }

  // Exercise 19

  def pascal (n :Int) : List[Int] = {
    def _pascal(n: Int, prev: List[Int]): List[Int] = {
      if (n == 1) prev else _pascal(n - 1, _row(Cons(0, prev)))
    }

    def _row(prev: List[Int]): List[Int] = prev match {
      case Nil => Cons(1, Nil)
      case Cons(_, Nil) => Cons(1, Nil)
      case Cons(h1, Cons(h2, t)) => Cons(h1 + h2, _row(Cons(h2, t)))
    }
    _pascal(n, Cons(1, Nil))
  }

    // Clean implementation
//  def pascal (n :Int) : List[Int] = n match {
//    case 0 => Nil
//    case 1 => List(1)
//    case _ => {
//      val p = pascal(n-1)
//      append(Cons(1, zipWith[Int,Int,Int] (_ + _) (p, tail(p))), List(1))
//    }
//  }

}

object Tests extends App {

  val l1 = List(1,2,3,4,5)

  // Exercise 2
  assert(List.tail(l1) == List(2,3,4,5))

  // Exercise 3
  assert(List.setHead(l1, 5) == List(5,2,3,4,5))

  // Exercise 4
  assert(List.drop(l1, 3) == List(4,5))
  assert(List.drop(l1, 0) == List(1,2,3,4,5))
  assert(List.drop(l1, 5) == List())
  assert(List.drop(l1, 6) == List())

  // Exercise 5
  assert(List.dropWhile(l1, (a: Int) => a % 2 == 0) == List(1,2,3,4,5))
  assert(List.dropWhile(l1, (a: Int) => a % 2 == 1) == List(2,3,4,5))

  // Exercise 6
  assert(List.init(l1) == List(1,2,3,4))

  // Exercise 7
  assert(List.length(l1) == 5)

  // Exercise 8
  assert(List.foldLeft(l1, 0)(_ + _) == 15)
  assert(List.foldLeft(l1, 0)(_ + _) == List.foldRight(l1, 0)(_ + _))

  // Exercise 9
  assert(List.sum(l1) == 15)
  assert(List.product(l1) == 120)
  assert(List.length1(l1) == 5)

  // Exercise 10
  assert(List.reverse(l1) == List(5,4,3,2,1))

  // Exercise 12
  val l2 = List(6,7,8)
  assert(List.concat(List(l1, l2)) == List(1,2,3,4,5,6,7,8))

  // Exercise 13
  assert(List.filter[Int](l1)(a => a % 2 == 0) == List(2,4))
  assert(List.filter[Int](l1)(a => a % 2 == 1) == List(1,3,5))

  // Exercise 14
  assert(List.flatMap[Int, Int](l2)(a => List(a,a)) == List(6,6,7,7,8,8))

  // Exercise 15
  assert(List.filter1(l1)(a => a % 2 == 0) == List(2,4))

  // Exercise 16
  assert(List.add(l1)(l2) == List(7, 9, 11))

  // Exercise 17
  assert(List.zipWith[Int, Int, Int](_ + _)(l1, l2) == List(7, 9, 11))

  // Exercise 18
  assert(List.hasSubsequence(l1, List(1,2,3)))
  assert(!List.hasSubsequence(l1, List(1,3)))
  assert(List.hasSubsequence(l1, List()))
  assert(List.hasSubsequence(l1, List(3,4,5)))
  assert(List.hasSubsequence(l1, List(5)))
  assert(List.hasSubsequence(l1, List(1)))

  // Exercise 19
  assert(List.pascal(1) == List(1))
  assert(List.pascal(2) == List(1,1))
  assert(List.pascal(3) == List(1,2,1))
  assert(List.pascal(4) == List(1,3,3,1))
  assert(List.pascal(5) == List(1,4,6,4,1))

}
