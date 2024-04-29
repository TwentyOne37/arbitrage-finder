#!/usr/bin/env -S scala-cli shebang

// Scala CLI dependencies
//> using dep "com.softwaremill.sttp.client4::core:4.0.0-M12"
//> using dep "com.softwaremill.sttp.client4::circe:4.0.0-M12"
//> using dep "org.typelevel::cats-core:2.10.0"
//> using dep "org.typelevel::cats-effect:3.5.4"
//> using dep "io.circe::circe-core:0.14.7"
//> using dep "io.circe::circe-generic:0.14.7"
//> using dep "io.circe::circe-parser:0.14.7"

import sttp.client4._
import sttp.client4.circe._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import cats.implicits._
import io.circe.generic.auto._
import sttp.client4.httpclient.HttpClientSyncBackend

import scala.math.log
import scala.collection.mutable

/* Borger, feel free to let your imagination shine but do not change this snippet. */
val url: String = args.length match {
  case 0 => "https://api.swissborg.io/v1/challenge/rates"
  case _ => args(0)
}

/* Add your stuff, be Awesome! */

case class RatesResponse(rates: Map[String, String])
case class Edge(from: String, to: String, weight: Double)

def findArbitrage(
    edges: List[Edge],
    vertices: Set[String],
    startCurrency: String
): IO[Option[(Double, List[Edge])]] = IO {
  val distances =
    mutable.Map(vertices.map(v => v -> Double.PositiveInfinity).toSeq *)
  val predecessors = mutable.Map[String, Edge]()

  distances(startCurrency) = 0.0

  for (_ <- 1 until vertices.size) {
    for (edge <- edges) {
      if (distances(edge.from) + edge.weight < distances(edge.to)) {
        distances(edge.to) = distances(edge.from) + edge.weight
        predecessors(edge.to) = edge
      }
    }
  }

  edges.foldLeft(None: Option[(Double, List[Edge])]) { (acc, edge) =>
    if (distances(edge.from) + edge.weight < distances(edge.to)) {
      var cycle = List[Edge]()
      var current = edge.to
      val visited = mutable.Set[String]()
      while (!visited.contains(current)) {
        cycle ::= predecessors(current)
        visited += current
        current = predecessors(current).from
      }

      cycle.find(_.from == startCurrency) match {
        case Some(startEdge) =>
          val startIndex = cycle.indexOf(startEdge)
          val finalCycle = cycle.drop(startIndex) ++ cycle.take(startIndex)

          val initialAmount = 100.0
          var currentAmount = initialAmount
          finalCycle.foreach { edge =>
            val rate = math.exp(-edge.weight)
            currentAmount *= rate
          }
          val profit = currentAmount - initialAmount

          Some(profit, finalCycle)

        case None => None
      }
    } else acc
  }
}

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
    val backend = HttpClientSyncBackend()
    val request = basicRequest.get(uri"$url").response(asJson[RatesResponse])

    for {
      response <- IO.blocking(request.send(backend))
      _ <- response.body match {
        case Right(ratesResponse) =>
          val edges = ratesResponse.rates.flatMap { case (pair, rate) =>
            val Array(from, to) = pair.split("-")
            if (rate.toDouble > 0) Some(Edge(from, to, -log(rate.toDouble)))
            else None
          }.toList
          val vertices = ratesResponse.rates.keys.flatMap(_.split("-")).toSet

          vertices
            .map(vertex => findArbitrage(edges, vertices, vertex))
            .toList
            .parTraverse(identity)
            .map { results =>
              results.flatten.maxByOption(_._1) match {
                case Some((profit, cycle)) =>
                  println("Arbitrage Opportunity Detected:")
                  cycle.foreach { edge =>
                    val rate = math.exp(-edge.weight)
                    println(
                      f"Trade from ${edge.from} to ${edge.to} at rate $rate%.8f"
                    )
                  }
                  println(f"Profit: +$profit%.2f%%")
                case None =>
                  println("No arbitrage opportunity detected.")
              }
            }
        case Left(error) =>
          IO.raiseError(new Exception(s"Failed to fetch data: $error"))
      }
      _ <- IO(backend.close())
    } yield ()
  }
}

Main.run.unsafeRunSync()
