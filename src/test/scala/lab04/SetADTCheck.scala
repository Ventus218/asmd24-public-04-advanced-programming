package scala.lab04

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.lab04.SetADTs.{BasicSetADT, SetADT}

abstract class SetADTCheck(name: String) extends Properties(name):
  val setADT: SetADT

  // generating a small Int
  def smallInt(): Gen[Int] = Gen.choose(0, 10)
  // generating a Set of Int with approximate size (modulo clashes)
  def setGen[A: Arbitrary](size: Int): Gen[setADT.Set[A]] =
    if size == 0
      then Gen.const(setADT.empty())
    else for
      a <- Arbitrary.arbitrary[A]
      s <- setGen(size - 1)
    yield s.add(a)
  // a given instance to generate sets with small size
  given arb: Arbitrary[setADT.Set[Int]] = Arbitrary:
    for
      i <- smallInt()
      s <- setGen[Int](i)
    yield s

  property("commutativity of union") =
    forAll: (s1: setADT.Set[Int], s2: setADT.Set[Int]) =>
      (s1 || s2) === (s2 || s1)

  /**
    * axioms defining contains based on empty/add:
    * contains(add(X, S), X) = true
    * contains(add(X, S), Y) = contains(S, Y) if X!=Y
    * contains(empty(), _) = false
    */

  property("axioms for contains") =
   forAll: (s: setADT.Set[Int], x: Int) =>
     s.add(x).contains(x)
   &&
     forAll: (s: setADT.Set[Int], x: Int, y:Int) =>
      x == y || s.add(x).contains(y) == s.contains(y)
   &&
     forAll: (x: Int) =>
      !setADT.empty().contains(x)


object BasicSetADTCheck extends SetADTCheck("SequenceBased Set"):
  val setADT: SetADT = BasicSetADT

  @main def visuallingCheckArbitrarySets =
    Range(0,20).foreach(i => println(summon[Arbitrary[setADT.Set[Int]]].arbitrary.sample))
