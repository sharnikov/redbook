object RedBook2 extends App {

  trait Stream[+A] {

    def toListRecursive: List[A] = this match {
      case Empty => List()
      case c: Cons[A] => c.head :: c.tail.toListRecursive
    }

    def uncons: Option[Cons[A]]

    def isEmpty: Boolean = uncons.isEmpty

    def take(n: Int): Stream[A] = this match {
      case c: Cons[A] if n > 1 => Stream.cons(c.head, c.tail.take(n - 1))
      case c: Cons[A] if n == 0 => Stream.cons(c.head, Empty)
      case _ => Empty
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case c: Cons[A] if p(c.head) => Stream.cons(c.head, c.tail.takeWhile(p))
      case _ => Empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons match {
        case Some(c) => f(c.head, c.tail.foldRight(z)(f))
        case None => z
      }

    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Stream.empty){ (el, stream) => if (p(el)) Stream.cons(el, stream.takeWhile(p)) else Empty }

    def forAll(p: A => Boolean): Boolean = this match {
      case c: Cons[A] if p(c.head) => c.tail.forAll(p)
      case _: Cons[A] => false
      case _ => true
    }

    def forAll2(p: A => Boolean): Boolean =
      foldRight(true)((x, y) => p(x) && y)


    def map[B](f: A => B): Stream[B] = this match {
      case c: Cons[A] => Stream.cons(f(c.head), c.tail.map(f))
      case _ => Empty
    }

//    def map2[B](f: A => B): Stream[B] =
//      foldRight()

    def filter(f: A => Boolean): Stream[A] = this match {
      case c: Cons[A] if f(c.head) => Stream.cons(c.head, c.tail.filter(f))
      case c: Cons[A] => c.tail.filter(f)
    }

    def append[B>:A](s: => Stream[B]): Stream[B] = ???

//    def flatMap[B](f: A => Stream[B]): Stream[B] = this match {
//      case c: Cons[A] => f(c.tail).map(Stream.cons(_, c.tail.flatMap(f)))
//      case _ => Empty
//    }
  }

  object Empty extends Stream[Nothing] {
    val uncons = None
  }

  sealed abstract class Cons[+A] extends Stream[A] {
    def head: A
    def tail: Stream[A]
    val uncons = Some(this)
  }

  object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
      lazy val head = hd
      lazy val tail = tl
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}
