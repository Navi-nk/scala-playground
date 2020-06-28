val x = Array(1,2,3,4)
x map (_ + 4)

val s = "Hello World"
s filter (_.isLower)

x exists(_ < 3)

x forall(_ < 3)

val zipped = x zip s
zipped.unzip

s flatMap(List('.',_)) // .H.e.l.l.o. .W.o.r.l.d
s. map (List('.',_)) //Vector(List(., H), List(., e), List(., l), List(., l), List(., o), List(.,  ), List(., W), List(., o), List(., r), List(., l), List(., d))

def scalarProduct(xs: Vector[Int], ys: Vector[Int]): Int =
  //(xs zip ys) map (xy => xy._1 * xy._2) sum
  (xs zip ys) map { case (x,y) => x * y } sum

scalarProduct(Vector(1,2,3), Vector(2,3,4))





