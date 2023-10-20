def reverse4[T](t: (T, T, T, T)): (T, T, T, T) = {
  val a = t._1
  val b = t._2
  val c = t._3
  val d = t._4
  (d, c, b, a)
}

reverse4(4, 3, 2, 1)
reverse4("a", "b", "c", "d")
reverse4(5, 4, 3, 2, 1)

def sumProd(s: Int, e: Int): (Int, Int) = {
  if (s >= e)
    (0, 1)
  else {
    val (sum, prod) = sumProd(s + 1, e)
    (s + sum, s * prod)
  }
}

sumProd(3, 6)
SumProd(7, 6)
SumProd(3, 3)
sumProd(-3, 5)

def isPerfect(n: Int): Boolean = {
  if (n > 0) {
    var sum = 0
    for (i <- 1 until n) {
      if (n % i == 0) sum += i
    }
    sum == n
  }
  else{
    false
  }
}

isPerfect(6)
isPerfect(-3)
isPerfect(33)

def insert[T] (list: List[T], elem: T, pos: Int): List[T] = {
  if (pos < 0){
    val (leftSplit, rightSplit) = list.splitAt(0)
    leftSplit ::: elem :: rightSplit
  }
  else if (pos >= list.length) list :+ elem
  else {
    val (leftSplit, rightSplit) = list.splitAt(pos)
    leftSplit ::: elem :: rightSplit
  }
}

insert(List("a", "a", "a", "a"), "b", -3)
insert(List("a", "a", "a", "a"), "b", 10)
insert(List("a", "a", "a", "a"), "b", 0)
insert(List("a", "a", "a", "a"), "b", 3)
insert(List("a", "a", "a", "a"), "b", 2)