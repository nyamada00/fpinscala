package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => sys.error("Empty")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil         => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _                     => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => sys.error("Empty")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, y) => y + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((xs, x) => Cons(x, xs))
  }

  def foldLeftByRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
  }

  def foldRightByLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def appendRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((a, b) => Cons(a, b))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((a, b) => appendRight(a, b))

  def addone(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString(), b))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((a, b) => append(f(a), b))

  def filterByFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def listAdd(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (_, Nil)                     => l
    case (Nil, _)                     => r
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, listAdd(t1, t2))
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
    (l, r) match {
      case (_, Nil)                     => Nil
      case (Nil, _)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  def found[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil)                                   => true
    case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => found(t1, t2)
    case _                                          => false
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                    => sub == Nil
    case _ if (found(sup, sub)) => true
    case Cons(h, t)             => hasSubSequence(t, sub)
  }

}

object ListTest {
  import List._
  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }

    println("--match--")
    println("expected:3");
    println("actual:%d".format(x))

    println("--tail--")
    testtail()

    println("--setHead--")
    testsetHead()

    println("--drop--")
    testdrop()

    println("--dropWhile--")
    testdropWhile()

    println("--init--")
    testinit()

    println("--length--")
    testlength()

    println("--sum3--")
    testsum3()

    println("--product3--")
    testproduct3()

    println("--length2--")
    testlength2()

    println("--reverse--")
    testreverse()

    println("--concat--")
    testconcat()

    println("--addone--")
    testaddone()

    println("--doubleToString--")
    testdoubleToString()

    println("--map--")
    testmap()

    println("--filter--")
    testfilter()

    println("--flatMap--")
    testflatMap()

    println("--hasSubSequence--")
    testhasSubSequence()
  }

  def testtail() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.tail(l)
    println("expected:2,3,4,5")
    println(t)

    val one = List(1)
    val t2 = List.tail(one)
    println("expected:Nil")
    println(t2)

    //sys.error
    //val n=Nil
    //val t3 = List.tail(n)

  }

  def testsetHead() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.setHead(l, 9)

    println("expected:9,2,3,4,5")
    println(t)

    val one = List(1)
    val t2 = List.setHead(one, 2)

    println("expected:2")
    println(t2)

    val n = Nil
    val t3 = List.setHead(n, 3)
    println("expected:3")
    println(t3)
  }

  def testdrop() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.drop(l, 3)

    println("expected:4,5")
    println(t);

    val l2 = List(1, 2, 3, 4, 5)
    val t2 = List.drop(l2, 6)

    println("expected:Nil")
    println(t2);

    val l3 = List(1, 2, 3, 4, 5)
    val t3 = List.drop(l3, -1)

    println("expected:1,2,3,4,5")
    println(t3);
  }

  def testdropWhile() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.dropWhile(l, (x: Int) => x < 3)

    println("expected:3,4,5")
    println(t)

    val l2 = Nil
    val t2 = List.dropWhile(l2, (x: Int) => x < 3)

    println("expected:Nil")
    println(t2)
  }

  def testinit() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.init(l)

    println("expected: 1,2,3,4")
    println(t)

    val l2 = List(1)
    val t2 = List.init(l2)

    println("expected: Nil")
    println(t2)

    //sys.error
    //val l3 = Nil
    //val t3 = List.init(l3)
  }

  def testlength() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.length(l)

    println("expected:5")
    println("actual:%d".format(t))

    val l2 = Nil
    val t2 = List.length(l2)

    println("expected:0")
    println("actual:%d".format(t2))
  }

  def testsum3() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.sum3(l)

    println("expected:15")
    println("actual:%d".format(t))
  }

  def testproduct3() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.product3(l)

    println("expected:120")
    println("actual:%d".format(t))
  }

  def testlength2() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.length2(l)

    println("expected:5")
    println("actual:%d".format(t))

    val l2 = Nil
    val t2 = List.length2(l2)

    println("expected:0")
    println("actual:%d".format(t2))
  }
  def testreverse() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.reverse(l)

    println("expected:5,4,3,2,1")
    println(t)

    val l2 = Nil
    val t2 = List.reverse(l2)

    println("expected:Nil")
    println(t2)
  }

  def testconcat() {
    val l = List(List(1, 2, 3, 4, 5), List(8, 9, 10))
    val t = List.concat(l)

    println("expected:1,2,3,4,5,8,9,10")
    println(t)
  }

  def testaddone() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.addone(l)

    println("expected:2,3,4,5,6")
    println(t)
  }

  def testdoubleToString() {
    val l = List(1.1, 2.3, 3.5)
    val t = List.doubleToString(l)

    println("expected:1.1,2.3,3.5")
    println(t)
  }

  def testmap() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.map(l)((x: Int) => x * 2)

    println("expected:2,4,6,8,10")
    println(t)
  }

  def testfilter() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.filter(l)((x: Int) => x % 2 == 0)

    println("expected:2,4")
    println(t)
  }

  def testflatMap() {
    val l = List(1, 2, 3, 4, 5)
    val t = List.flatMap(l)((x: Int) => List(x, x))

    println("expected:1,1,2,2,3,3,4,4,5,5")
    println(t)
  }

  def testhasSubSequence() {
    val l = List(1, 2, 3, 4, 5)
    val s = List(1, 2)
    val t = List.hasSubSequence(l, s)

    println("expected:true")
    println(t)

    val l2 = List(1, 2, 3, 4, 5)
    val s2 = List(4, 5)
    val t2 = List.hasSubSequence(l2, s2)

    println("expected:true")
    println(t2)

    val l3 = List(1, 2, 3, 4, 5)
    val s3 = List(1, 5)
    val t3 = List.hasSubSequence(l3, s3)

    println("expected:false")
    println(t3)
  }
}
