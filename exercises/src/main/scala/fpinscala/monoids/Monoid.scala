package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }

  val intMultiplication: Monoid[Int]  = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 0
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op (a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op (a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op (a1: Option[A], a2: Option[A]) = a1 orElse a2
    def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op (a1: A => A, a2: A => A) = a => a1(a2(a))
    def zero = a => a
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  // trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)){case (x, y, z) =>
      m.op(x, m.op(y, z)) == m.op(m.op(x,y), z)
    } && 
    forAll(gen)(s => m.op(m.zero, s) == s && m.op(s, m.zero) == s)
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    sys.error("todo")

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((a, b) => m.op(f(a), b))

  def foldRight[A,B](as: List[A])(z: B)(f: (A, B) => B): B =
    // foldMap(as, new Monoid[B => B] {
    //   def op (a1: B => B, a2: B => B) = a => a1(a2(a))
    //   def zero = a => a
    // })(a => b => f(a, b))(z)
    foldMap(as, endoMonoid[B])(a => b => f(a,b))(z)

  def foldLeftE[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    // foldMap(as, new Monoid[B => B] {
    //   def op (a1: B => B, a2: B => B) = a => a1(a2(a))
    //   def zero = a => a
    // })(a => b => f(b, a))(z)
    foldMap(as, endoMonoid[B])(a => b => f(b,a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as.length match {
    case 0 => m.zero
    case 1 => f(as.head)
    case l => {
      val (h, t) = as.splitAt(l/2)
      m.op(foldMapV(h, m)(f), foldMapV(t, m)(f))
    }
  }

  case class OrderCheck (
    inOrder: Boolean,
    start: Option[Int],
    end: Option[Int]
  )

  def orderCheckM: Monoid[OrderCheck] = new Monoid[OrderCheck] {
    def zero = OrderCheck(true, None, None)
    def op (a1: OrderCheck, a2: OrderCheck) = (a1, a2) match {
      case (OrderCheck(false, _, _), _) => OrderCheck(false, None, None)
      case (_, OrderCheck(false, _, _)) => OrderCheck(false, None, None)
      case (OrderCheck(true, None, _), _) => OrderCheck(true, None, None)
      case (_, OrderCheck(true, None, _)) => OrderCheck(true, None, None)
      case (OrderCheck(true, a1s, Some(a1f)), OrderCheck(true, Some(a2s), a2f)) => {
        if (a1f < a2s)
          OrderCheck(true, a1s, a2f)
        else
          OrderCheck(false, None, None)
      }
      case _ => OrderCheck(false, None, None)
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    (foldMapV(ints, orderCheckM)(i => OrderCheck(true, Some(i), Some(i)))).inOrder

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero = Par.unit(m.zero)
    def op (a: Par[A], b: Par[A]): Par[A] = Par.map2(a, b)(m.op)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = v.length match {
    case 0 => par(m).zero
    case 1 => Par.unit(f(v.head))
    case l => {
      val (h, t) = v.splitAt(l/2)
      par(m).op(parFoldMap(h, m)(f), parFoldMap(t,m)(f))
    }
  }
  // version from the answers, this litters fork's throughout the execution tree
  // and doesn't assume how to lift a value into the monoid
  def parFoldMapA[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  val wcMonoid: Monoid[WC] = sys.error("todo")

  def count(s: String): Int = sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

