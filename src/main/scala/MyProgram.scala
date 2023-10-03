import scala.annotation.tailrec
// A comment!
/* Another comment */
/** A documentation comment */
object MyProgram:
  private def abs(n: Int): Int =
    if n < 0 then -n
    else n

  private def factorial(n: Int): Int =
    @tailrec
    def go(n: Int, acc: Int): Int =
      if n <= 0 then acc
      else go(n - 1, n * acc)

    go(n, 1)

  private def fib(n: Int): Int =
    @tailrec
    def go(n: Int, current: Int, next: Int): Int =
      if n <= 0 then current
      else go(n - 1, next, current + next)
    go(n, 0, 1)

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean =
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if n + 1 >= as.length then true
      else if gt(as(n), as(n + 1)) then false
      else loop(n + 1)
    loop(0)

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a,b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  private def formatFib(x: Int) =
    val msg = "Fibonacci of of %d is %d"
    msg.format(x, fib(x))

  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  private def formatFac(x: Int) =
    val msg = "Factorial value of %d is %d"
    msg.format(x, factorial(x))

  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n + 1)
    loop(0)

  @main
  def printSomething(): Unit =
    println(formatAbs(-42))
    println(formatFac(3))
    println(formatFib(0))
    println(formatFib(1))
    println(formatFib(2))
    println(formatFib(3))
    println(formatFib(4))
    println(formatFib(5))
    println(formatFib(6))
