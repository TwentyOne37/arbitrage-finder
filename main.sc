#!/usr/bin/env -S scala-cli shebang

// Scala CLI dependencies
//> using dep "com.softwaremill.sttp.client4::core:4.0.0-M12"
//> using dep "com.softwaremill.sttp.client4::circe:4.0.0-M12"
//> using dep "io.circe::circe-core:0.14.7"
//> using dep "io.circe::circe-generic:0.14.7"
//> using dep "io.circe::circe-parser:0.14.7"

import sttp.client4._
import sttp.client4.circe._
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
): Option[List[Edge]] = {
  val distances =
    mutable.Map(vertices.map(v => v -> Double.PositiveInfinity).toSeq: _*)
  val predecessors = mutable.Map[String, Edge]()

  // Start from the specified initial currency
  distances(startCurrency) = 0.0

  // Relax edges up to |V|-1 times
  for (_ <- 1 until vertices.size) {
    for (edge <- edges) {
      if (distances(edge.from) + edge.weight < distances(edge.to)) {
        distances(edge.to) = distances(edge.from) + edge.weight
        predecessors(edge.to) = edge
      }
    }
  }

  // Check for negative-weight cycles and track the order of trades
  edges.foldLeft(None: Option[List[Edge]]) { (acc, edge) =>
    if (distances(edge.from) + edge.weight < distances(edge.to)) {
      var cycle = List[Edge]()
      var current = edge.to
      val visited = mutable.Set[String]()
      while (!visited.contains(current)) {
        cycle ::= predecessors(current)
        visited += current
        current = predecessors(current).from
      }

      // Close the loop properly by ensuring it starts and ends with startCurrency
      cycle.find(_.from == startCurrency) match {
        case Some(startEdge) =>
          val startIndex = cycle.indexOf(startEdge)
          Some(cycle.drop(startIndex) ++ cycle.take(startIndex))
        case None => None
      }
    } else acc
  }
}

// Create an HTTP client backend
val backend = HttpClientSyncBackend()

// Send a request and fetch the response
val request = basicRequest
  .get(uri"$url")
  .response(asJson[RatesResponse])
val response = request.send(backend)

// Process the response
response.body match {
  case Right(ratesResponse) =>
    val edges = ratesResponse.rates.flatMap { case (pair, rate) =>
      val Array(from, to) = pair.split("-")
      if (rate.toDouble > 0) Some(Edge(from, to, -log(rate.toDouble)))
      else None
    }.toList
    val vertices = ratesResponse.rates.keys.flatMap(_.split("-")).toSet

    findArbitrage(edges, vertices, "DAI") match {
      case Some(cycle) =>
        println("Arbitrage Opportunity Detected:")

        // Assuming starting with an arbitrary amount of one of the currencies in the cycle
        val initialCurrency = cycle.head.from
        val initialAmount = 100.0
        var currentAmount = initialAmount

        // Iterate through the cycle
        cycle.foreach { edge =>
          val rate = math.exp(
            -edge.weight
          ) // Convert the logarithmic weight back to the rate
          currentAmount *= rate // Update the amount based on the rate
          println(
            f"Trade from ${edge.from} to ${edge.to} at rate $rate%.8f, new amount: $currentAmount%.2f ${edge.to}"
          )
        }

        // Final output showing the total profit
        val profit = currentAmount - initialAmount
        println(
          f"Started with $initialAmount%.2f $initialCurrency and ended with $currentAmount%.2f $initialCurrency, profit: $profit%.2f $initialCurrency"
        )

      case None =>
        println("No arbitrage opportunity detected.")
    }

  case Left(error) =>
    println(s"Failed to fetch data: $error")
}

// Close the backend
backend.close()
