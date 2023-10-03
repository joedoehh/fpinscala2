// A comment!
/* Another comment */
/** A documentation comment */
object MyProgram:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n

  def factorial(n: Int): Int =
    def go(n: Int, acc: Int): Int =
      if n <= 0 then acc
      else go(n - 1, n * acc)

    go(n, 1)

  def fib(n: Int): Int =
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

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a,b)

  private def formatFib(x: Int) =
    val msg = "Fibonacci of of %d is %d"
    msg.format(x, fib(x))

  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  private def formatFac(x: Int) =
    val msg = "Factorial value of %d is %d"
    msg.format(x, factorial(x))

  @main def printAbs: Unit =
    println(formatAbs(-42))
    println(formatFac(3))
    println(formatFib(0))
    println(formatFib(1))
    println(formatFib(2))
    println(formatFib(3))
    println(formatFib(4))
    println(formatFib(5))
    println(formatFib(6))
