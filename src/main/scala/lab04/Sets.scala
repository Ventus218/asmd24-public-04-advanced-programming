package scala.lab04

import u04.datastructures.Sequences.*
import Sequence.*

object Sets:
  
  trait SetADT:
    type Set[A]
    def empty[A](): Set[A]
    def of[A](element: A): Set[A]
    extension [A](s: Set[A])
      def contains(a: A): Boolean
      def union(other: Set[A]): Set[A]
      def intersection(other: Set[A]): Set[A]
      infix def ||(other: Set[A]): Set[A] = s.union(other)
      infix def &&(other: Set[A]): Set[A] = s.intersection(other)
      def remove(a: A): Set[A]
      def toSequence(): Sequence[A]
      def size(): Int
      def ===(other: Set[A]): Boolean
    

  object BasicSetADT extends SetADT:

    opaque type Set[A] = Sequence[A]

    def fromSequence[A](s: Sequence[A]): Set[A] = s match
      case Cons(h, t) => Cons(h, fromSequence(t.remove(h)))
      case Nil() => Nil()

    def empty[A](): Set[A] = Nil()

    def of[A](element: A): Set[A] = Cons(element, Nil())

    extension [A](s: Set[A])
      def remove(a: A): Set[A] = s.filter(_ != a)  

      def contains(a: A): Boolean = s match
        case Cons(h, t) if h == a => true
        case Cons(_, t) => t.contains(a)
        case Nil() => false

      def toSequence(): Sequence[A] = s

      def union(s2: Set[A]): Set[A] = s2 match
        case Cons(h, t) => Cons(h, s.remove(h).union(t))
        case Nil() => s

      def intersection(s2: Set[A]): Set[A] = s match
        case Cons(h, t) if s2.contains(h) => Cons(h, t.intersection(s2.remove(h)))
        case Cons(_, t) => t.intersection(s2)
        case Nil() => Nil()

      def size(): Int = s match
        case Cons(_, t) => 1 + t.size()
        case Nil() => 0

      def ===(other: Set[A]): Boolean =
        val union = s.union(other)
        union.size() == s.size()


@main def trySetADTModule =
  import Sets.* 
  val setADT: SetADT = BasicSetADT
  import setADT.*

  val s1: Set[Int] = of(10) || of(20) || of(30)
  val s2: Set[Int] = of(10) || of(11)
  // val s3: Set[Int] = Cons(10, Nil()) // because Set is defined opaque
  println(s1.toSequence()) // (10, 20, 30)
  println(s2.toSequence()) // (10, 11)
  println(s1.union(s2).toSequence()) // (10, 20, 30, 11)
  println(s1.intersection(s2).toSequence()) // (10)