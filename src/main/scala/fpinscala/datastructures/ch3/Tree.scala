package fpinscala.datastructures.ch3

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int =
    this match
      case Leaf(_) => 1
      case Branch(l, r) => 1 + l.size + r.size

  def depth: Int =
    this match
      case Leaf(_) => 0
      case Branch(l, r) =>
        val maxDepth = l.depth.max(r.depth)
        maxDepth+1
object Tree:
  extension (t: Tree[Int]) def maximum: Int =
    t match
      case Leaf(l) => l
      case Branch(l,r) => l.maximum.max(r.maximum)

  def map[A, B](t: Tree[A], f: A => B): Tree[B] =
    t match
      case Leaf(l) => Leaf(f(l))
      case Branch(l,r) =>
        val mapL: Tree[B] = map(l,f)
        val mapR: Tree[B] = map(r,f)
        Branch(mapL, mapR)

  def sampleTree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(5), Branch(Leaf(-1), Leaf(22))))