package fpinscala.datastructures.ch3

import scala.annotation.tailrec

enum List[+A]:
  private case Nil
  private case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def tail[A](as: List[A]): List[A] = as match
    case Nil => sys.error("tail not defined on empty list")
    case Cons(x, xs) => xs

  def setHead[A](a: A, as: List[A]): List[A] = as match
    case Nil => Cons(a, Nil)
    case x => Cons(a, x)

  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    if n <= 0 then as
    else as match
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)

  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => as

  def init[A](as: List[A]): List[A] =
    as match
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Any =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Any =
    foldRight(ns, 1.0, _ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0, (x, y) => 1 + y)

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil => acc
      case Cons(hd, tl) => foldLeft(tl, f(acc, hd), f)

  def sumFL(ns: List[Int]): Any =
    foldLeft(ns, 0, _ + _)

  def productFL(ns: List[Int]): Any =
    foldLeft(ns, 1, _ * _)

  def lengthFL(ns: List[Int]): Any =
    foldLeft(ns, 0, (x,y) => x + 1)

  def foldRightViaFL[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(as, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)

  def foldLeftViaFR[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    foldRight(as, (b: B) => b, (a, g) => b => g(f(b, a)))(acc)

  def appendFR[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys, (x,y) => Cons(x, y))

  def appendFL[A](xs: List[A], ys: List[A]): List[A] =
    foldLeft(ys, xs, (x, y) => Cons(y, x))

  def appendEnd[A](xs: List[A], x: A): List[A] =
    foldRight(xs, Nil: List[A], (next, acc) => Cons(next, acc))

  def concatenate[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], appendFR)

  // 3.3.3 More functions for working with lists

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int], (i,acc) => Cons(i+1, acc))

  def doubleToStringEach(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (d, acc) => Cons(d.toString, acc))

  def doubleToStringEachFL(l: List[Double]): List[String] =
    foldLeft(l, Nil: List[String], (acc, d) => appendEnd(acc, d.toString))

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, Nil: List[B], (a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A],
      (a, acc) =>
        if f(a) then Cons(a, acc) else acc)

  def filterOdd(xs: List[Int]): List[Int] =
    filter(xs, _ % 2 != 0)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B], (a, acc) => appendFR(f(a), acc))

  def filterFM[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

