

def last[T](xs : List[T]) : T  = xs match {
  case List() => throw new NoSuchElementException
  case List(x) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]) : List[T] = xs match {
  case List() => throw new NoSuchElementException
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

val l1 = List(1,3,5,6,7,1,6)

l1 updated (0 , 100) //replaces 0th index element with 100
last(l1)
init(l1)
