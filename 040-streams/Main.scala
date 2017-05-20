// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscal.laziness._
import fpinscal.laziness.Stream._

import fpinscal.laziness.Empty


// this is how we do simple interactive testing

object Test4 extends App {

  val l1 :Stream[Int] = Empty
  val l2 :Stream[Int] = empty

  val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

  println (l1.headOption)
  println (l2.headOption)
  println (l3.headOption)
}



