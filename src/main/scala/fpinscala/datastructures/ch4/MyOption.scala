package fpinscala.datastructures.ch4

enum MyOption[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): MyOption[B] =
    this match
      case None => None
      case Some(a) => Some(f(a))

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B =
    this match
      case None => default
      case Some(a) => a

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): MyOption[A] =
    flatMap(a => if f(a) then Some(a) else None)