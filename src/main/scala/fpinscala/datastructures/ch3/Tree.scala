package fpinscala.datastructures.ch3

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def fold[B](f: A => B, g: (B, B) => B): B =
    this match
      case Leaf(l) => f(l)
      case Branch(lt, rt) => g(lt.fold(f, g), rt.fold(f, g))

  def size: Int =
    fold(l => 1, 1 + _ + _)

  def depth: Int =
    fold(l => 0, (l,r) => 1 + (l max r))


object Tree:
  extension (t: Tree[Int]) def maximum: Int =
    t.fold(x => x, (lMax, rMax) => lMax max rMax)

  def map[A, B](t: Tree[A], f: A => B): Tree[B] =
    t match
      case Leaf(l) => Leaf(f(l))
      case Branch(lt,rt) => Branch(map(lt,f), map(rt,f))

  def sampleTree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(5), Branch(Leaf(-1), Leaf(22))))