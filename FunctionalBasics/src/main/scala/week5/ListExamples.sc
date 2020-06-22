

def last[T](xs : List[T]) : T  = xs match {
  case List() => throw new NoSuchElementException
  case List(x) => x
  case y :: ys => last(ys)
}