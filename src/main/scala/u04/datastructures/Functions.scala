package scala.u04.datastructures

// a module, including various functions and definitions
object Functions extends App:

  // a function (i.e. method), using recursion and case-match
  def factorial(n: Int): Int = n match
    case 0 | 1 => 1
    case _ => n * factorial(n - 1)

  println(factorial(5)) // 120

  // a function (i.e. method), using currying, and passing a function
  def applyManyTimes[A](initial: A, n: Int)(f: A => A): A = n match
    case 0 => initial
    case _ => applyManyTimes(f(initial), n - 1)(f)

  println(applyManyTimes(0, 10)(i => i + 2)) // 20

  // a record data type
  case class Point2D(x: Double, y: Double)

  def swap(p: Point2D): Point2D = p match
    case Point2D(x, y) => Point2D(y, x)

  println(swap(Point2D(10, 20))) // Point2D(20, 10)
