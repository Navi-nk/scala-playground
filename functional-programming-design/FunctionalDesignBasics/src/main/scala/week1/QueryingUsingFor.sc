case class Book(title: String, authors: List[String])

val library= Set(
  Book(title = "One", authors = List("Some Guy")),
  Book(title = "two", authors = List("Some Guy1")),
  Book(title = "Three", authors = List("Some Guy1")),
  Book(title = "Four", authors = List("Some Guy2")),
  Book(title = "Five", authors = List("Some Guy3")),
  Book(title = "Six", authors = List("Some Guy3")),
  Book(title = "Seven", authors = List("Some Guy5")),
)


for ( b <- library ) yield b.title
for(b <- library if b.title.contains("o")) yield b.title

val a:Set[String] = for {b1 <- library
     b2 <- library
     if b1.title <  b2.title
     a1 <- b1.authors
     a2 <- b2.authors
     if  a1.equals(a2)
} yield a1

val b:Set[String] = library
  .flatMap( l1 => library.withFilter( l2 => l1.title < l2.title)
    .flatMap(l3 => l1.authors.flatMap( a1 => l3.authors.withFilter( a2 => a2.equals(a1))
    .map( x => x))
))


println(a) //Set(Some Guy1, Some Guy3)
println(b) //Set(Some Guy1, Some Guy3)






