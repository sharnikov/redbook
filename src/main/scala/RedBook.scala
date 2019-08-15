object RedBook extends App {

  def fib(n: Int): Int = {
    def fibLocal(f: Int, s: Int, number: Int): Int = {
      if (number == 1) s else {
        fibLocal(s, f + s, number - 1)
      }
    }

    fibLocal(0, 1, n)
  }


  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true else {
      as.zip(as.tail).forall { case (x, y) => gt(x, y) }
    }
  }

  def isSorted2[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true else {
      as.zip(as.tail).forall { case (x, y) => gt(x, y) }
    }
  }


  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def tail[A](l: List[A]) = l match {
    case Nil => throw new Exception()
    case _ :: tail => tail
  }

  def setHead[A](newHead: A, l: List[A]) = l match {
    case Nil => throw new Exception()
    case _ :: tail => newHead :: tail
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case tail if n <= 0 => tail
    case Nil => Nil
    case _ :: tail => drop(tail, n - 1)
  }

  val l = List(1, 2, 3, 4, 5)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case head :: tail if f(head) => dropWhile(tail, f)
    case _ => l
  }


  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: Nil => Nil
    case head :: tail => head :: init(tail)
  }


  def length[A](l: List[A]): Int = l.foldRight(0) { case (_, acc) => 1 + acc }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case head :: tail => foldLeft(tail, f(z, head))(f)
  }


  def sum(l: List[Int]): Int = {
    l.foldLeft(0)(_ + _)
  }

  def mult(l: List[Int]): Int = {
    l.foldLeft(1)(_ * _)
  }

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case head :: tail => reverse(tail) :+ head
  }

  def reverse2[A](l: List[A]): List[A] =
    l.foldLeft(List.empty[A]) { case (l, e) => e :: l }

  def frInFl[A](l: List[A], initValue: A, f: => (A, A) => A) = {
    l.foldLeft(List.empty[A]) { case (l, e) => e :: l }.foldLeft(initValue) { case (x, y) => f(y, x) }
  }

  def frInFl2[A](l: List[A], initValue: A, f: => (A, A) => A) = {
  }


  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l.foldLeft((b: B) => b) {
      (accF, e) => v => accF(f(e, v))
    }(z)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l.foldRight((b: B) => b)((e, accF) => v => accF(f(v, e)))(z)

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case head :: tail => head :: append(tail, a2)
    }

  val l2 = List(6, 7, 8, 9, 10)
  val l3 = List(11, 12, 13, 14, 15)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
    l.foldRight(r)((el, list) => el :: list)
  }

  def concat[A](l: List[List[A]]): List[A] = {
    l.foldLeft(List.empty[A])(appendViaFoldRight)
  }

  def addOne1(l: List[Int]) =
    l.foldRight(List.empty[Int])((el, list) => (1 + el) :: list)

  def addOne2(l: List[Int]): List[Int] = {
    l match {
      case Nil => Nil
      case head :: tail => (1 + head) :: addOne2(tail)
    }
  }

  def doubleToString(l: List[Double]): List[String] = {
    l match {
      case Nil => Nil
      case head :: tail => head.toString :: doubleToString(tail)
    }
  }

  val ll = List(l, l2, l3)

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l.foldRight(List.empty[B])((el, list) => f(el) :: list)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case head :: tail if f(head) => head :: filter(tail)(f)
    case _ :: tail => filter(tail)(f)
  }


  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)((e: A) => if (f(e)) List(e) else List.empty[A])
  }

  def combine(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (head1 :: tail1, head2 :: tail2) => (head1 + head2) :: combine(tail1, tail2)
  }

  println(combine(l, l2))

  def combineCommon[A, B, C](l1: List[A], l2: List[B], f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (head1 :: tail1, head2 :: tail2) => f(head1, head2) :: combineCommon(tail1, tail2, f)
  }

  def hasCommon[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (head1 :: tail1, head2 :: tail2) if head1 == head2 => hasCommon(tail1, tail2)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => Nil == sub
    case _ if hasCommon(l, sub) => true
    case _ :: tail => hasSubsequence(tail, sub)
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(e) => e
    case Branch(l, r) => scala.math.max(max(l), max(r))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => scala.math.max(depth(l) + 1, depth(r) + 1)
  }

  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(e) => Leaf(f(e))
    case Branch(left, right) => Branch(map(left, f), map(right, f))
  }

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(value) => Some(f(value))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(value) => f(value)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(value) => value
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case value => value
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(value) if f(value) => Some(value)
      case _ => None
    }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      av <- a
      bv <- b
    } yield {
      f(av, bv)
    }
  }

  def sequence[A](a: List[scala.Option[A]]): scala.Option[List[A]] = {
    a.foldRight(scala.Option(List.empty[A])) { case (value, acc) =>
      for {
        list <- acc
        inValue <- value
      } yield {
        inValue :: list
      }
    }
  }

  def traverse[A, B](a: List[A])(f: A => scala.Option[B]): scala.Option[List[B]] =
    a.foldRight(scala.Option(List.empty[B])) { case (value, acc) =>
      for {
        list <- acc
        inValue <- f(value)
      } yield {
        inValue :: list
      }
    }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(value) => Right(f(value))
      case Left(v) => Left(v)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(value) => f(value)
      case Left(v) => Left(v)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
      case (Right(v1), Right(v2)) => Right(f(v1, v2))
      case (Right(_), Left(v2)) => Left(v2)
      case (Left(v1), Right(_)) => Left(v1)
      case (Left(v1), Left(_)) => Left(v1)
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]


  def sequence[E, A](a: List[scala.Either[E, A]]): scala.Either[E, List[A]] = {
    a.foldRight[scala.Either[E,List[A]]](scala.Right(List.empty[A])) { case (el, acc) =>
      for {
        inAcc <- acc
        inEl <- el
      } yield {
        inEl :: inAcc
      }
    }
  }

  def traverseE[E, A, B](es: List[A])(f: A => scala.Either[E, B]): scala.Either[E, List[B]] = {
    es.foldRight[scala.Either[E,List[B]]](scala.Right(List.empty[B])) { case (el, acc) =>
      for {
        inAcc <- acc
        inEl <- f(el)
      } yield {
        inEl :: inAcc
      }
    }
  }

  def sequence2[E, A](a: List[scala.Either[E, A]]): scala.Either[E, List[A]] = {
    traverseE(a)(x => x)
  }

}
