trait Generator[+T] {
  self =>
  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]) : Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }

  def single[T](x : T) : Generator[T] = new Generator[T] {
    override def generate : T = x
  }
}

class IntGenerator extends Generator[Int] {
  private val rand = new java.util.Random
  override def generate: Int = rand.nextInt()
  def choose(l: Int, h: Int): Generator[Int] = for (x <- this) yield l + math.abs(x) % (h - l)
  def oneOf[T](xs: T*):Generator[T] = for(idx <- choose(0, xs.length)) yield xs(idx)
}

val integers: IntGenerator = new IntGenerator

val booleans: Generator[Boolean] = new Generator[Boolean] {
  override def generate: Boolean = integers.generate > 0
}

val bools = {for {
  i <- 1 to 1000000
  x = booleans.generate} yield x } partition( x => x)

//distribution approximately 50%
bools._1.length
bools._2.length

integers.generate
booleans.generate

integers.oneOf(1,2,3,4,5).generate
integers.choose(-100,100).generate

val pairs: Generator[(Int,Int)] = new Generator[(Int, Int)] {
  override def generate: (Int, Int) = (integers.generate, integers.generate)
}

val betterBooleans: Generator[Boolean] = for(x <- integers) yield x > 0

val betterPairs: Generator[(Int,Int)]= for( x <- integers; y<-integers ) yield (x , y)



def lists: Generator[List[Int]] = {
  def nonEmptyList: Generator[List[Int]] = for {
    head <- integers
    tail <- lists
  } yield head :: tail
  for {
    isEmpty <- booleans
    list <- if(isEmpty) integers.single(Nil) else nonEmptyList
  } yield list
}

lists.generate
lists.generate
lists.generate


trait Tree

case class Inner(left: Tree, right:Tree) extends Tree

case class Leaf(x :Int) extends Tree

def trees: Generator[Tree] = {
  def leafs : Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)

  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l,r)

  for {
    isLeaf <- betterBooleans
    tree <- if(isLeaf) leafs else inners
  } yield tree
}

trees.generate
trees.generate