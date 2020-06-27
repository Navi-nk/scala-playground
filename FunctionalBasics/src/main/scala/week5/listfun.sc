val nums = List(2, -4, 5, 7 ,1)
val fruits = List("apple", "pipeapple", "orange", "banana")

nums filter( r => r < 0)
nums filterNot( r => r > 0)
nums partition( r => r > 0)

nums takeWhile(x => x > 0)
nums dropWhile( x => x < 0)
nums span ( x => x < 0)
