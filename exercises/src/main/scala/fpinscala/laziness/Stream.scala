package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    (this.foldRight(Nil:List[A])(_ :: _))
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, (() => t().take(n-1)))
    case _ => empty
  }

  def takeUnfold(n: Int): Stream[A] = unfold((n, this)){
    case (0, _) => None
    case (_, Empty) => None
    case (i, Cons(h, t)) => Some(h(), (i-1, t()))
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case e => e
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, (() => t().takeWhile(p)))
    case _ => empty
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Empty => None
    case Cons(h, t) => if (p(h())) Some((h(), t())) else None
  }

  def forAll(p: A => Boolean): Boolean = ! this.exists((a) => ! p(a))

  def takeWhileRight(p: A => Boolean): Stream[A] = this.foldRight(
    Empty:Stream[A]
  )((h, t) => p(h) match {
    case true => Stream.cons(h, t)
    case false => Empty
  })

  def headOption: Option[A] = this.foldRight(None:Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this.foldRight(
    Empty:Stream[B]
  ){(h, t) =>
    Stream.cons(f(h), t)
  }

  def mapUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def filter(f: A => Boolean): Stream[A] = this.foldRight(
    Empty:Stream[A]
  ){(h, t) => f(h) match {
    case true => Stream.cons(h, t)
    case false => t
  }}

  def append[B>:A](s: Stream[B]): Stream[B] =
    this.foldRight(s){(h,t) => cons(h,t)}

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]){(h,t) =>
    f(h) append t
  }

  def zipWith[B](s: Stream[B]): Stream[(A, B)] = unfold((this, s)){
    case((_, Empty)) => None
    case((Empty, _)) => None
    case(Cons(h, t), Cons(sh, st)) => Some(((h(), sh()), (t(), st())))
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s)){
    case ((Empty, Empty)) => None
    case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
    case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
    case (Cons(h, t), Cons(hs, ts)) => Some(
      ((Some(h()), Some(hs())), (t(), ts()))
    )
  }

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Empty, _) => false
    case (Cons(h, t), Cons(hs, ts)) => if (h() != hs()) {
      false
    } else {
      t().startsWith(ts())
    }
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case Empty => None
    case a@Cons(_, t) => Some(a, t())
  }.append(Stream(empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))){(a, s) =>
      lazy val sv = s
      val a2 = f(a, sv._1)
      (a2, cons(a2, sv._2))
    }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a,constant(a))
  val ones: Stream[Int] = Stream.constant(1)

  def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n+1))

  def fibs: Stream[Int] = fibsInner(0, 1)
  private def fibsInner(a:Int, b:Int): Stream[Int] = Stream.cons(a, fibsInner(b, a + b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def constantUnfold[A](a: A): Stream[A] = unfold(a)((_) => Some((a, a)))
  def onesUnfold: Stream[Int] = constantUnfold(1)
  def fromUnfold(n: Int): Stream[Int] = unfold(n)((i) => Some((i, i+1)))
  def fibsUnfold: Stream[Int] =
    unfold((0,1)){case (c,n) => Some(c, (n, c+n)) }
}
