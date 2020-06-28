package week4

class Foo {
  override def toString: String = "Foo"
}

class Bar extends Foo {
  override def toString: String = "Bar"
}

class Baz extends Bar {
  override def toString: String = "Baz"
}


//Class variance test is covariant meaning VarianceTest[Class1] <: VarianceTest[Class2] given Class1 <: Class2
class VarianceTest[+A] {

  //Co Variance check. parameters should be non variant in params and co variant in return type.
  // Hence print method cannot take param of type A. it has to take a type that is utmost, super type of A (lower bound set to A).
  // Means to say at minimum it has to be A and its subtypes and maximum it can be Z.
  // on the type parameter of method.
  def print[Z >: A](a:List[Z]): Unit = println(a.mkString(","))

}

//Class NonVarianceTest is nonvariant meaning NonVarianceTest[Class1] >: VarianceTest[Class2] given Class1 <: Class2
class NonVarianceTest[-A] {
  //Non Variant check is opposite of Co variant
  def value[Z <: A](x: Z, y: Z): List[Z] = List(x,y)
}

  object test {
  def main(args: Array[String]): Unit = {
    val v: VarianceTest[Foo] = new VarianceTest[Foo]
    val v1: VarianceTest[Bar] = new VarianceTest[Bar]
    val v2: VarianceTest[Baz] = new VarianceTest[Baz]
    val v3: VarianceTest[Foo] = new VarianceTest[Bar]  //VarianceTest[Bar] is a type VarianceTest[Foo] since Bar is a type of Foo

    val l1: List[Foo] = List(new Foo, new Bar, new Baz)
    val l2: List[Bar] = List(new Bar, new Bar, new Baz)
    val l3: List[Baz] = List(new Baz, new Baz, new Baz)
    val l4: List[Object] = List(new Foo, new String)

    //A is Foo
    v.print(l1)   //Z is Foo also
    v.print(l2)   //Z is Bar, but Bar is of type Foo
    v.print(l3)   //Z is Baz, again Baz is of type Foo via Bar. Till now we have seen A and Z gets type inferred to Foo
    v.print(l4)   //Z is object. Now Object is a super type of Foo i.e Object >: Foo

    //A is Bar
    v1.print(l1)  //Z is Foo. Foo >: Bar
    v1.print(l2)  //Z is Bar also
    v1.print(l3)  //Z is Baz. Baz is of type Bar
    v1.print(l4)  //Z is object. Object >: Bar

    //A is Baz
    v2.print(l1)  //Z is Foo. Foo >: Baz
    v2.print(l2)  //Z is Bar. Bar >: Baz
    v2.print(l3)  //Z is Baz also
    v2.print(l4)  //Z is object. Object >: Baz

    //A is Bar at runtime but is of type Foo
    v3.print(l1)  //Z is Foo.
    v3.print(l2)  //Z is Bar which is of type Foo
    v3.print(l3)  //Z is Baz. Baz is of type Foo via Foo
    v3.print(l4)  //Z is object. Object >: Foo

   //Non Variance
    val n1: NonVarianceTest[Foo] = new NonVarianceTest[Foo]
    val n2: NonVarianceTest[Bar] = new NonVarianceTest[Bar]
    val n3: NonVarianceTest[Baz] = new NonVarianceTest[Baz]
    //val n4: NonVarianceTest[Foo] = new NonVarianceTest[Bar]  -- wont compile

    val n4 : NonVarianceTest[Baz] = new NonVarianceTest[Foo] //compiles

    val r1: List[Foo] = n1.value(new Foo, new Baz)  //A is Foo and Z can Foo,bar or baz
    val r2: List[Bar] = n1.value(new Bar, new Baz)

    val r3: List[Bar] = n2.value(new Bar, new Baz)  //A is Bar so Z can be only bar and baz
    //val r3: List[Foo] = n2.value(new Foo, new Baz) // Error, Z cannot be Foo

    val r4: List[Baz] = n3.value(new Baz, new Baz)   // A and Z can be only Baz
    //val r4: List[Foo] = n3.value(new Foo, new Bar) //Error

    val r5: List[Baz] = n4.value(new Baz, new Baz)  //same as above

    //val r6: List[Object] = n1.value(new String, new Foo) // Error, Z cannot be Object

  }

}