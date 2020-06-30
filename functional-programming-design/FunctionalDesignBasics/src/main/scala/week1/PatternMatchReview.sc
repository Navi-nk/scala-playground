
  abstract class Json
  case class JSeq(elems: List[Json]) extends Json
  case class JObj(values: Map[String, Json]) extends Json
  case class JNum(num: Double) extends Json
  case class JStr(str: String) extends Json
  case class JBool(b: Boolean) extends Json
  case object JNull extends Json

  def show(json: Json) : String = {
    json match {
      case JSeq(e) =>
        "[" + (e map show mkString ",")+"]"
      case JObj(v) =>
        val values = v map {
          case (key, value) => "\"" + key +"\":" + show(value)
        }
        "{" + ( values mkString "," ) + "}"
      case JNum(n) => n.toString
      case JStr(s) => "\"" + s + "\""
      case JBool(b) => b.toString
      case JNull => "null"
    }
  }

  val data=JObj(Map(
    "firstName" -> JStr("John"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("some street"),
      "state" -> JStr("Some State"),
      "postalCode" -> JNum(12345))),
    "contact" -> JSeq(List(JObj(Map(
      "type" -> JStr("home"),
      "number" -> JStr("555055-213")
    )),
      JObj(Map(
        "type" -> JStr("office"),
        "number" -> JStr("555055-214")
      ))
    ))
  ))


  println(show(data))

  val f: String => String = {
    case "ping" => "pong"
    case _ => throw new Error("no data found")
  }

  val f1: PartialFunction[String, String] = {case "ping" => "pong"}

  f1.isDefinedAt("1")
  f1.isDefinedAt("ping")


  val g : PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: tail => tail match {
      case Nil => "two"
    }
  }


  g.isDefinedAt(List(1,2,3))
  g(List(1,2,3)) //error