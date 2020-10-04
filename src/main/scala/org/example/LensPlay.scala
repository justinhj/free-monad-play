package org.example

import monocle.Prism

object LensPlay extends App {

  sealed trait Json
  case object JNull extends Json
  case class JStr(v: String) extends Json
  case class JNum(v: Double) extends Json
  case class JObj(v: Map[String, Json]) extends Json

  val jStr: Prism[Json, String] = Prism[Json, String]{
      case JStr(v) => Some(v)
      case _ => None
    }(b => JStr(b))

  // Prism let's us always reverse get an A from a B
  // and optionally get a B from an A...

  val getOpt1 = jStr.getOption(JStr("Justin"))
  // Some(Justin)

  val getOpt2 = jStr.getOption(JNull)
  // None

  val rGet = jStr.reverseGet("Justin")
  // JStr(Justin)

  // Not sure what the point is of this
  val what = jStr.getOrModify(JNum(2.0))
  val what2 = jStr.getOrModify(JStr("Justin"))

  println(s"what $what\nwhat2 $what2")
}
