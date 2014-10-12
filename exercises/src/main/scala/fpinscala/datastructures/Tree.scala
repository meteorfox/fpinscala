package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(t1, t2) => 1 + size(t1) + size(t2)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(t1, t2) => maximum(t1) max maximum(t2)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(t1, t2) => b(fold(t1)(l)(b), fold(t2)(l)(b))
  }

  def size1[A](t: Tree[A]): Int =
    fold(t)((a: A) => 1)(1 + _ + _)

  def maximum1(t: Tree[Int]): Int =
    fold(t)((a) => a)(_ max _)

  def depth1[A](t: Tree[A]): Int =
    fold(t)((a: A) => 0)((a, b) => 1 + (a max b))

  def map1[A,B](t: Tree[A])(l: A => B): Tree[B] =
    fold(t)(a => Leaf(l(a)): Tree[B])(Branch(_,_))
}
