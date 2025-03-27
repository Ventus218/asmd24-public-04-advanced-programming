package scala.lab04

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

import scala.lab04.Sets.{BasicSetADT, SetADT}

abstract class SetADTCheck(name: String) extends Properties(name):
  val ADT: SetADT

  given Arbitrary[ADT.Set[Int]] = Arbitrary:
    for
      n <- Arbitrary.arbitrary[Int]
    yield ADT.of(n)


  property("union - commutative") = forAll:
    (a: ADT.Set[Int], b: ADT.Set[Int]) =>
      (a || b) === (b || a)

object BasicSetADTCheck extends SetADTCheck("SequenceBased Set"):
  val ADT: SetADT = BasicSetADT
