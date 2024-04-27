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
    vertices: Set[String]
): Option[List[Edge]] = {
  val distances = collection.mutable.Map(
    vertices.map(v => v -> Double.PositiveInfinity).toSeq: _*
  )
  val predecessors = collection.mutable.Map[String, Edge]()

  // Start from an arbitrary node, assuming we can trade any currency into any other
  distances(vertices.head) = 0.0

  // Relax edges up to |V|-1 times
  for (_ <- 1 until vertices.size) {
    for (edge <- edges) {
      if (distances(edge.from) + edge.weight < distances(edge.to)) {
        distances(edge.to) = distances(edge.from) + edge.weight
        predecessors(edge.to) = edge
      }
    }
  }

  // Check for negative-weight cycles
  for (edge <- edges) {
    if (distances(edge.from) + edge.weight < distances(edge.to)) {
      // Reconstruct the negative cycle
      var cycle = List[Edge]()
      var current = edge.to
      val visited = collection.mutable.Set[String]()
      while (!visited.contains(current)) {
        cycle ::= predecessors(current)
        visited += current
        current = predecessors(current).from
      }

      // Return only the part of the cycle from the first repeated currency
      val startIndex = cycle.indexWhere(_.from == current)
      return Some(cycle.drop(startIndex).reverse)
    }
  }
  None
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
    findArbitrage(edges, vertices) match {
      case Some(cycle) =>
        println("Arbitrage Opportunity Detected:")
        cycle.foreach { edge =>
          println(
            f"Trade from ${edge.from} to ${edge.to} at rate ${math.exp(-edge.weight)}"
          )
        }
      case None =>
        println("No arbitrage opportunity detected.")
    }
  case Left(error) =>
    println(s"Failed to fetch data: $error")
}

// Close the backend
backend.close()
