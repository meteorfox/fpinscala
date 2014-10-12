package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  } // == 3

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Cannot get tail from Nil")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Cannot setHead to a Nil list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => if (f(h)) dropWhile(t, f) else l
  }

  // init returns all but the last element of a List
  // e.g. List(1, 2, 3, 4) => List(1, 2, 3)
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Cannot init a Nil list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil =>  z
    case Cons(h,t) => foldLeft(t, f(z, h))(f)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](list: List[A]): Int = foldLeft(list, 0)((acc,_) => acc + 1)

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil:List[A])((acc, e) => Cons(e, acc))

  def partial[A,B,C](a: A, f: (A,B) => C):  B => C =
    (b: B) => f(a, b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = 
    (a: A) => f(g(a))

  def swap[A,B](f: (B, A) => B): (A, B) => B = 
    (a, b) => f(b, a)

  def partial2[A,B](f: (B, A) => B, a: A): B => B = f(_, a)
    
  def foldLeft2[A,B](l: List[A], z:B)(f: (B, A) => B): B = { 
    def leftToRight(a: A, g: B => B): B => B = 
      (b: B) => g(f(b,a))
    def foldingRight(xs: List[A], x: B): B = 
      foldRight(xs, (b: B) => b)(leftToRight(_, _))(x)
    
    foldingRight(l, z)
  }
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  def append3[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l1), l2)((acc: List[A], a: A) => Cons(a, acc))

  def concat[A](xs: List[List[A]]): List[A] =
    foldRight(xs, Nil: List[A])(append)

  def add1(xs: List[Int]): List[Int] =
    foldRight(xs, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(xs: List[Double]): List[String] =
    foldRight(xs, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filter1[A](as: List[A])(p: A => Boolean): List[A] =
    flatMap(as)(a => if (p(a)) List(a) else List())

  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case(Nil, _) => l2
    case(_, Nil) => l1
    case(Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
  }
    
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case(Nil, _) => Nil
    case(_, Nil) => Nil
    case(Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
  val y = foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
}
