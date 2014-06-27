package fr.enst.plnc

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import rx.lang.scala._
import scala.io.Source.fromURL

object TD2 extends App {
  def fromURL(url: String): Observable[Char] =
  Observable[Char] {suscriber: Subscriber[Char] =>
    val stream = io.Source.fromURL(url)
    for (c <- stream) suscriber.onNext(c)
}
  def getContent(url: String): Observable[String] =
  {
    fromURL(url).map(_.toString).reduce( _ ++ _)
  }
}

object Main extends APP
{
// subscriber
}
