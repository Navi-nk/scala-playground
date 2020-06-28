val nums = List(2, -4, 5, 7 ,1)
val fruits = List("apple", "pipeapple", "orange", "banana")

nums filter( r => r < 0)
nums filterNot( r => r > 0)
nums partition( r => r > 0)

nums takeWhile(x => x > 0)
nums dropWhile( x => x < 0)
nums span ( x => x < 0)


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (t, u) => f(t) :: u)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (_, u) => u + 1 )

lengthFun(1 :: 2 :: 3 :: 1 :: 1 :: 1::1::1::1 ::Nil)
mapFun(1 :: 2 :: 3 :: Nil, (x:Int) => x + x )

