package hayago.gtp

import hayago._

sealed trait ResponseType
object ResponseType {
  object Success extends ResponseType
  object Failure extends ResponseType
}

sealed case class Response (responseType: ResponseType, id: Option[Int], entities: List[String]) {
  override def toString = {
    val prefix = responseType match {
      case ResponseType.Success => "="
      case ResponseType.Failure => "?"
    }
    val i = id match {
      case Some (i) => s"[$i] "
      case None => ""
    }
    prefix + i + entities.mkString (" ") + newLine
  }
}

object Response {
  def success(id: Option[Int], entities: List[String]) = Response(ResponseType.Success, id, entities)
  def failure(msg: String) = Response(ResponseType.Failure, None, msg :: Nil)
}

