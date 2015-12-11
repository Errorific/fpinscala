package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nrng) = rng.nextInt
    val pi = if (i < 0) -(i + 1) else i
    (pi, nrng)
  }

  def doubleMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def double(rng: RNG): (Double, RNG) = {
    val (i, nrng) = nonNegativeInt(rng)
    val d = i.toDouble / Int.MaxValue
    (d, nrng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), nrng) = intDouble(rng)
    ((d, i), nrng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 1)
      (List(), rng)
    else {
      val (i, rng1) = rng.nextInt
      val (r, rngf) = ints(count - 1)(rng1)
      (i :: r, rngf)
    }
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a,b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](
      unit(Nil)
    )(
      map2(_, _)(_ :: _)
    )

  def intsS(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){i =>
      val mod = n % i
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapfm[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(e => unit(f(e)))

  def map2fm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(e => mapfm(rb)(e2 => f(e, e2)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A] (a:A): State[S,A] = State(s => (a, s))

  def sequence[S,A] (sl: List[State[S,A]]) = {
    sl.foldRight(
      unit[S, List[A]](Nil)
    )((f, acc) =>
      f.map2(acc)(_ :: _)
    )
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- getS
    _ <- setS(f(s))
  } yield ()

 def getS[S]: State[S, S] = State(s => (s, s))
 def setS[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  import State._

  def update = (i: Input) => (s: Machine) => {
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- getS
  } yield (s.coins, s.candies)
}
