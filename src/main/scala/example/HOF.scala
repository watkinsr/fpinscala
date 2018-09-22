class HigherOrderFunction {
  def findFirst[A](as: Array[A], p: A => Boolean) : Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    def loop(n: Int) : Boolean =
      if (n >= as.length)
        true
      else if (n < as.length && ordered(as(n), as(n - 1)))
        loop(n + 1)
      else
        false

    loop(1)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 2.3
  def curry[A,B,C](f: (A, B) => C) : A => (B => C) = {
    a => b => f(a, b)
  }

  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C) : (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
