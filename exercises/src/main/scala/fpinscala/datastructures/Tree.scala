package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(t: Tree[_]):Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]):Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => {
      val maxL = maximum(l)
      val maxR = maximum(r)
      if (maxL > maxR)
        maxL
      else
        maxR
    }
  }

  def depth(t: Tree[_]):Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => {
      val depthL = depth(l)
      val depthR = depth(r)
      if (depthL > depthR)
        depthL
      else
        depthR
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Failed fold implementation
  // realised at map
  // can't have initial value if you have no empty tree constructor
  // def fold[A, B](t: Tree[A], i:B)(f: (A, B) => B):B = t match {
  //   case Leaf(a) => f(a, i)
  //   case Branch(l, r) => fold(l, fold(r, i)(f))(f)
  // }

  // def sizeViaFold[A](t: Tree[A]):Int =
  //   fold(t, 0)((a, b) => b + 1)
  // def maximumViaFold(t: Tree[Int]):Int =
  //   fold(t, 0)((a, b) => if (a > b) a else b)
  // def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
  //   fold(t, )

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold(t: Tree[_]):Int =
    fold(t)(_ => 1)(_ + _)

  def maximumViaFold(t: Tree[Int]):Int =
    fold(t)(i => i)((a, b) => if (a > b) a else b)

  def mapViaFold[A,B](t: Tree[A])(f: A => B):Tree[B] =
    fold(t)(
      (i) => Leaf(f(i)):Tree[B]
    )(
      (a,b) => Branch(a, b)
    )
}
