// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecWasowski extends FlatSpec with Checkers {

  import Stream._

  case class ForcedEvaluationException(msg: String) extends scala.Exception(msg)

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  def infiniteStream[A] (implicit arbA: Arbitrary[A]) :Stream[A] = arbA.arbitrary.sample match {
    case None => infiniteStream(arbA)
    case Some(a) => cons(a, infiniteStream(arbA))
  }

//  implicit def arbException = Arbitrary[ForcedEvaluationException] (throw ForcedEvaluationException("Not lazy"))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)

  def getStream[A] (implicit arbA: Arbitrary[A]): Gen[Stream[A]] = {
    for { la <- arbitrary[List[A]] }
    yield list2stream(la)
  }

  // TODO: never do this as the framework will give up after cca. 100 tries (happens quite often)
  def genStreamOfN[A] (n: Int) (implicit arbA: Arbitrary[A]): Gen[Stream[A]] = {
    for { la  <- arbitrary[List[A]] suchThat(l => l.length >= n) }
    yield list2stream(la.take(n))
  }

  def genEmptyStream[A] (implicit arbA: Arbitrary[A]): Gen[Stream[A]] = {
    //arbitrary[List[A]].suchThat(_.isEmpty).flatMap(la => list2stream(la))
    for { la <- arbitrary[List[A]].suchThat(_.isEmpty) }
    yield list2stream (la)
  }

  val oneInt: Stream[Int] = cons(1, empty)
  val twoInt: Stream[Int] = cons(1, cons(2, empty))

  val infiniteStreamInt: Stream[Int] = infiniteStream[Int]
//  val infiniteStreamException: Stream[ForcedEvaluationException] = infiniteStream[ForcedEvaluationException]

  //val finiteStreamException: Gen[Stream[ForcedEvaluationException]] = getStream[ForcedEvaluationException]

  val oneIntException: Stream[Int] = cons(1, throw ForcedEvaluationException("Not lazy"))
  val twoIntException: Stream[Int] = cons(1, cons(2, throw ForcedEvaluationException("Not lazy")))
  val intExceptionInf: Stream[Int] = cons(1, infiniteStreamInt)

  val infiniteException: Stream[Nothing] = cons(throw ForcedEvaluationException("Not lazy"), infiniteException)

  def finiteExceptionOfN (n: Int): Stream[ForcedEvaluationException] = {
    if (n == 0) empty else cons(throw ForcedEvaluationException("Not lazy"), finiteExceptionOfN(n - 1))
  }

  def finiteException (implicit aArb: Arbitrary[Int]): Gen[Stream[ForcedEvaluationException]] = {
    for { n <- arbitrary[Int] }
    yield finiteExceptionOfN(n)
  }


  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )
  }

  it should "headOption not force the tail of the stream" in {
    assert(oneIntException.headOption.get == 1)
    assert(twoIntException.drop(1).headOption.get == 2)
    assert(intExceptionInf.headOption.get == 1)
  }

  behavior of "take"

  it should "not force any heads nor tails" in {
    oneIntException.take(1)
    twoIntException.take(0)
    infiniteException.take(10)
    finiteExceptionOfN(1000).take(10)
    infiniteException.take(0).toList
  }

  it should "not force n+1 head ever" in {
    assert(oneIntException.take(1).toList == List(1))
    assert(twoIntException.take(2).toList == List(1,2))
    assert(intExceptionInf.take(1).toList == List(1))
  }

  it should "not force n+1 head ever prop" in check {
    Prop.forAll(Gen.choose(0, 20)) {n => {
      implicit def arbStreamN = Arbitrary[Stream[Int]] (genStreamOfN[Int](n))
      Prop.forAll((s: Stream[Int]) => s.append(infiniteException).take(n).toList == s.take(n).toList)
    }}
  }

  it should "s.take(n).take(n) == s.take(n) for any Stream s and n (idempotency)" in check {
    ("finite" |:
      Prop.forAll(Gen.choose(0, 1000), genNonEmptyStream) { (n, s) => s.take(n).take(n).toList == s.take(n).toList }) &&
    ("infinite" |:
      Prop.forAll(Gen.choose(0, 1000)) { n => infiniteStreamInt.take(n).take(n).toList == infiniteStreamInt.take(n).toList }) &&
    ("empty" |:
      Prop.forAll(Gen.choose(0, 1000)) {n => empty.take(n).take(n) == empty.take(n)})
  }


  behavior of "drop"

  it should "s.drop() testing boundaries" in {
    assert(empty.drop(1).toList == List())
    assert(oneInt.drop(1).toList == List())
    assert(twoInt.drop(1).toList == List(2))
    assert(twoInt.drop(2).toList == List())
    assert(oneInt.drop(0).toList == List(1))
  }

  it should "s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity)" in check {
    ("finite"   |:
      Prop.forAll(Gen.choose(0, 1000), Gen.choose(0, 2000), genNonEmptyStream[Int]) { (n, m, s) =>  s.drop(n).drop(m).take(n + m).toList == s.drop(n + m).take(n + m).toList }) &&
    ("infinite" |:
      Prop.forAll(Gen.choose(0, 1000), Gen.choose(0, 1000)) {(n, m) => infiniteStreamInt.drop(n).drop(m).take(n + m).toList == infiniteStreamInt.drop(n + m).take(m + n).toList}) &&
    ("empty"    |:
      Prop.forAll(Gen.choose(0, 1000), Gen.choose(0, 1000)) {(n, m) => empty.drop(n).drop(m).toList == empty.drop(m + n).toList})
  }

  it should "not force any of the dropped heads" in check {
    ("finite"   |:
      Prop.forAll(Gen.choose(0, 1000), finiteException) { (n, s) => {
        s.drop(n)
        true
      } }) &&
    ("infinite" |:
      Prop.forAll(Gen.choose(0, 1000)) { n => {
        infiniteStreamInt.drop(n)
        true
      }} )
  }


  behavior of "map"

  it should "x.map(id) == x (where id is the identity function)" in {
    assert(empty.map(identity).toList == empty.toList)
  }

  it should "x.map(id) == x (where id is the identity function) prop" in check {
    Prop.forAll(genNonEmptyStream) { s => s.map(identity).toList == s.toList }
  }

  it should "terminates on infinite streams" in {
    intExceptionInf.map(identity)
  }

  it should "terminates on infinite streams prop" in check {
    "finite" |:
      Prop.forAll(Gen.choose(0, 10)) { n => Prop.forAll(genStreamOfN[Int](n)) { s =>
        s.map(s => s + 1)
        true
      }}
  }





}
