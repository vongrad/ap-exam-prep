trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // Exercise 1 (CB 6.1)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, nRNG) = rng.nextInt
    (Math.abs(x), nRNG)
  }

  // Exercise 2 (CB 6.2)

  def double(rng: RNG): (Double, RNG) = {
    val (x, newRng) = nonNegativeInt(rng)
    val nextVal = (x.toDouble / Int.MaxValue) + 1
    (nextVal - Math.floor(nextVal), newRng)
  }

  // Exercise 3 (CB 6.3)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

   def doubleInt(rng: RNG): ((Double, Int), RNG) = {
     val (d, r1) = double(rng)
     val (i, r2) = nonNegativeInt(r1)
     ((d, i), r2)
   }

   def double3(rng: RNG): ((Double, Double, Double), RNG) = {
     val (d1, r1) = double(rng)
     val (d2, r2) = double(r1)
     val (d3, r3) = double(r2)
     ((d1,d2,d3), r3)
   }

  def boolean(rng: RNG): (Boolean, RNG) = rng.nextInt match { case (i,rng2) => (i%2 == 0, rng2) }

  // Exercise 4 (CB 6.4)

   def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
     if (count == 0) (Nil:List[Int], rng) else {
       val (i, r1) = rng.nextInt
       val (l, newRNG) = ints(count - 1) (r1)
       (i::l, newRNG)
     }
   }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)

  val _double: Rand[Double] = (rng: RNG) => map[Int, Double](int)(a => a.toDouble / Double.MaxValue)(rng)

  // Exercise 6 (CB 6.6)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng: RNG) => {
    val (x1, r1) = ra(rng)
    val (x2, r2) = rb(r1)
    (f(x1, x2), r2)
  }

  // this is given in the book

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldLeft((List.empty[A], rng))((z, a) => {
      val x = a(z._2)
      (x._1::z._1, x._2)
    })
  }

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Exercise 8 (6.8)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(rng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(a => rng => if (a < n) (a, rng) else (a % n, rng))

}

import RNG.Simple
import State._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s1) = this.run(s)
    (f(a), s1)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State({ s =>
    val (a, s1) = this.run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = this.run(s)
    f(a).run(s1)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 9 (6.10) continued

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = State(s => {
    sas.foldRight((List.empty[A], s))((a, z) => {
      val (a1, s1) = a.run(z._2)
      (a1::z._1, s1)
    })
  })

  //
  // This is given in the book:

   def modify[S](f: S => S): State[S, Unit] = for {
     s <- get // Gets the current state and assigns it to `s`.
     _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
   } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 10

  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = {
    val (a1, seed1) = s.run(seed)
    a1#::state2stream(s)(seed1)
  }

  // Exercise 11

  val random_integers = state2stream(random_int) _

}

object Test extends App {

  val rng = Simple(5)

  // Exercise 1
  println(RNG.nonNegativeInt(rng))

  // Exercise 2
  assert(RNG.double(rng)._1 < 1.0 && RNG.double(rng)._1 >= 0)

  // Exercise 4
  println(RNG.ints(10)(rng))

  // Exercise 5
  println(RNG._double(rng))

  // Exercise 6
  println(RNG.map2(rng => rng.nextInt, rng => rng.nextInt)((a, b) => a % b)(rng))

  // Exercise 7
  println(RNG._ints(10)(rng))

  // Exercise 8
  println(RNG.nonNegativeLessThan(5)(rng))

  // Exercise 10
  val state = State[RNG, Int](a => a.nextInt)
  println(state2stream(state)(rng).take(10).toList)

  // Exercise 11
  println(random_integers(rng).take(10).toList)


}


// vim:cc=80:foldmethod=indent:foldenable
