// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscal.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

  // Exercise 2

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
  }

  // Exercise 3
  def take (n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (n == 0) empty else cons(h(), t().take(n - 1))
  }

  def drop (n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(_, t) => if (n == 0) this else t().drop(n - 1)
  }

  // Exercise 4
  def takeWhile (p :A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
  }

  // Exercise 5
  def forAll (p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
  }

  // Exercise 6
  def takeWhile2 (p: A => Boolean): Stream[A] = foldRight(empty[A])((a, z) => if(p(a)) cons(a, z) else empty)

  // Exercise 7
  def headOption2(): Option[A] = foldRight(None:Option[A])((a, _) => Some(a))

  // Exercise 8
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, z) => cons(f(a), z))

  def filter (f: A => Boolean): Stream[A] = foldRight(empty[A])((a, z) => if (f(a)) cons(a, z) else z)

  def append[B>:A] (b: => Stream[B]): Stream[B] = foldRight(b) ((a, z) => cons(a, z))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, z) => f(a).append(z))

  // Exercise 9
  def find (p :A => Boolean) :Option[A] = this.filter (p).headOption()
  // Because lists are not lazy and we would have to go through entire list and then check if it has the elements, it this case, we have to go just until we find one

  // Exercise 13
  def map2[B] (f: A => B): Stream[B] = unfold(this) (s => s.headOption().map(a => (f(a), s.tail)))

  def take2 (n: Int): Stream[A] = unfold((n, this)) (s => if (s._1 == 0) None else s._2.headOption().flatMap(a => Some((a, (s._1 - 1, s._2.tail)))))

  def takeWhile3 (p: A => Boolean): Stream[A] = unfold(this) (s => s.headOption().flatMap(a => if (p(a)) Some((a, s.tail)) else None))

  def zipWith[B, C] (f: (A, B) => C) (s2: Stream[B]): Stream[C] = unfold((this, s2)) (s => s._1.headOption().flatMap(a => s._2.headOption().map(b => (f(a, b), (s._1.tail, s._2.tail)))))
}




case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  // Exercise 1
  def to(n: Int): Stream[Int] = {
    def _to(c: Int): Stream[Int] = {
      if (c <= n) cons(c, _to(c + 1)) else empty
    }
    _to(0)
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def fib : Stream[Int] = {
    def _do(prev: Int, current: Int): Stream[Int] = {
      cons(prev, _do(current, current + prev))
    }
    _do(0, 1)
  }

  def fib2: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map(s => cons(s._1, unfold(s._2)(f))).getOrElse(empty)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq
}

object Test4 extends App {

  val s1 = cons(0, cons(1, cons(2, cons(3, cons(4, cons(5, empty))))))
  val s2 = cons(2, cons(3, cons(4, cons(5, empty))))
  val naturals = Stream.from(0)

  // Exercise 1
  assert(Stream.to(5).toList == s1.toList)
//  assert(Stream.from(2).toList == s2.toList)

  // Exercise 3
  assert(s1.take(3).toList == List(0,1,2))
  assert(s1.drop(3).toList == List(3,4,5))
  assert(Stream.from(2).take(4).toList == s2.toList)
  assert(Stream.from(2).drop(5).take(4).toList == List(7,8,9,10))

  // Exercise 4
  assert(naturals.takeWhile(_ < 5).toList == List(0,1,2,3,4))

  // Exercise 5
  assert(!naturals.forAll(_ < 0))
  assert(s1.forAll(_ < 6))

  // Exercise 6
  assert(naturals.takeWhile2(_ < 5).toList == List(0,1,2,3,4))

  // Exercise 7
  assert(naturals.headOption2().get == 0)
  assert(naturals.drop(100).headOption2().get == 100)
  assert(empty.headOption2().isEmpty)

  // Exercise 8
  assert(naturals.map (_*2).drop (30).take (5).toList == List(60,62,64,66,68))

  assert(naturals.drop(42).filter (_%2 ==0).take (5).toList == List(42,44,46,48,50))

  assert(naturals.take(3).append(naturals).take(5).toList == List(0,1,2,0,1))

  assert(naturals.flatMap (x => Stream.to(x)).take (10).toList == List(0,0,1,0,1,2,0,1,2,3))

  // Exercise 10
  assert(Stream.fib.take(8).toList == List(0,1,1,2,3,5,8,13))

  // Exercise 11
  assert(Stream.unfold(3)(s => Some((s, s + 1))).take(10).toList == List(3,4,5,6,7,8,9,10,11,12))

  // Exercise 12
  assert(fib2.take(8).toList == List(0,1,1,2,3,5,8,13))

  assert(Stream.from2(2).take(4).toList == s2.toList)
  assert(Stream.from2(2).drop(5).take(4).toList == List(7,8,9,10))

  // Exercise 13
  assert(naturals.zipWith[Int,Int] (_+_) (naturals).take(2000000000).take(10).toList == List(0,2,4,6,8,10,12,14,16,18))
  assert(naturals.map (_%2==0).zipWith[Boolean,Boolean] (_||_) (naturals.map (_%2==1)).take(3).toList == List(true,true,true))
}

// vim:tw=0:cc=80:nowrap
