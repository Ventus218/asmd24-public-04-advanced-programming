package scala.lab04

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

import scala.lab04.Sets.{BasicSetADT, SetADT}

object SetADTCheck extends Properties("Sets"):

  given Arbitrary[SetADT] = Arbitrary(BasicSetADT)

  property("union - commutative") = forAll:
    (adt: SetADT, a: Int, b: Int) =>
      (adt.of(a) || adt.of(b)) === (adt.of(b) || adt.of(a))
