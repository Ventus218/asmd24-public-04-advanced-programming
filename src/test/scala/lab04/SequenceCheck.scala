package scala.lab04

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.lab04.Sequences.*
import scala.lab04.Sequences.Sequence.*


object SequenceCheck extends Properties("Sequence"):

  // define a recursive generator of lists, monadically
  def sequenceGen[A: Arbitrary](): Gen[Sequence[A]] = for
    i <- arbitrary[A]
    b <- Gen.prob(0.8)
    s <- if b then sequenceGen().map(s2 => Cons(i, s2)) else Gen.const(Nil())
  yield s

  // define custom arbitrary lists and mappers
  given intSeqArbitrary: Arbitrary[Sequence[Int]] = Arbitrary(sequenceGen[Int]())
  given mapperArbitrary: Arbitrary[Int => Int] = Arbitrary(Gen.oneOf[Int => Int]( _+1, _*2, x => x*x))

  // check axioms, universally
  property("mapAxioms") =
    forAll: (seq: Sequence[Int], f: Int => Int) =>
      //println(seq); println(f(10)) // inspect what's using
      (seq, f) match
        case (Nil(), f) =>  map(Nil())(f) == Nil()
        case (Cons(h, t), f) => map(Cons(h, t))(f) == Cons(f(h), map(t)(f))

  // Operation:
  //  sum: Sequence[Int] => Int
  // Axioms:
  //  sum(nil) = 0
  //  sum(cons(h, t)) = h + sum(t)
  property("sumAxioms") =
    forAll: (seq: Sequence[Int]) =>
      seq match
        case Nil() => sum(seq) == 0
        case Cons(h, t) => sum(seq) == h + sum(t)
      
  // Operation:
  //  filter: Sequence[A] x (A => Boolean) => Sequence[A]
  // Axioms:
  //  filter(nil) = nil
  //  filter(cons(h, t), f) = if f(h) then cons(h, filter(t, f)) else filter(t, f)
  property("filterAxioms") =
    forAll: (seq: Sequence[Int], f: Int => Boolean) =>
      (seq, f) match
        case (Nil(), f) => filter[Int](seq)(f) == Nil()
        case (Cons(h, t), f) => filter(seq)(f) == (if f(h) then Cons(h, filter(t)(f)) else filter(t)(f))

  // Operation:
  //  flatMap: Sequence[A] x (A => Sequence[B]) => Sequence[B]
  // Axioms:
  //  flatMap(nil) = nil
  //  flatMap(cons(h, t), f) = f(h) concat flatMap(t, f)
  property("flatMapAxioms") =
    forAll: (seq: Sequence[Int], f: Int => Sequence[Int]) =>
      (seq, f) match
        case (Nil(), f) => flatMap(seq)(f) == Nil()
        case (Cons(h, t), f) => flatMap(seq)(f) == concat(f(h), flatMap(t)(f))
        
  // how to check a generator works as expected
  @main def showSequences() =
    Range(0,20).foreach(i => println(summon[Arbitrary[Sequence[Int]]].arbitrary.sample))
