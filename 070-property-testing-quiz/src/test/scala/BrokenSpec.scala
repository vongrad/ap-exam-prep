// Advanced Programming, A. Wąsowski, IT University of Copenhagen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

class BrokenSpec extends org.scalatest.FlatSpec with org.scalatest.prop.Checkers {

  def sequence[A] (aos: List[Option[A]]) : Option[List[A]] =
    aos.foldRight[Option[List[A]]] (Some(Nil)) {
      (oa,z) => z flatMap (l => oa map (_::l)) }

  behavior of "sequence"

  it should "succeed if the list has no failures" in check {
    implicit def arbList[A] (implicit arb: Arbitrary[List[A]]) =
      Arbitrary[List[Option[A]]] (arb.arbitrary map { _ map (Some (_)) })

    forAll { (l :List[Option[Int]]) => sequence(l).isDefined }
  }

  it should "fail if the list has one failure" in check {
    forAll { (l :List[Option[Int]]) => sequence(l).isEmpty }
  }
}
