
//complexity is N
def last[T](xs : List[T]) : T  = xs match {
  case List() => throw new NoSuchElementException
  case List(x) => x
  case y :: ys => last(ys)
}

//complexity is N
def init[T](xs: List[T]) : List[T] = xs match {
  case List() => throw new NoSuchElementException
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

val l1 = List(1,3,5,6,7,1,6)

l1 updated (0 , 100) //replaces 0th index element with 100
last(l1)
init(l1)

val l2 = List(10,11,2)

//complexity is |xs|
def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs , ys)
}

//complexity is N for reverse times N for ++ so its N * N (quadratic)
def reverse[T](xs: List[T]) : List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}


def removeAt[T](idx: Int, xs: List[T]) : List[T] = {
  if (idx >= xs.length) throw new NoSuchElementException
  else
    xs match {
    case List() => xs
    case y :: ys => if(xs.indexOf(y) == idx) ys else y :: removeAt(idx - 1, ys)
  }
}

def removeAtSimple[T](idx: Int, xs: List[T]) : List[T] = (xs take idx) ::: (xs drop idx + 1)
removeAt(3, l1) == removeAtSimple(3 , l1)

