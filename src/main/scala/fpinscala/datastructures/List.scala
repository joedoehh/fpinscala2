package fpinscala.datastructures

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
