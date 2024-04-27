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

/* Borger, feel free to let your imagination shine but do not change this snippet. */
val url: String = args.length match {
  case 0 => "https://api.swissborg.io/v1/challenge/rates"
  case _ => args(0)
}

/* Add your stuff, be Awesome! */

// Define a case class to map the JSON structure
case class RatesResponse(rates: Map[String, String])

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
    println("Currency Pairs and their Rates:")
    ratesResponse.rates.foreach { case (pair, rate) =>
      println(f"$pair%-12s -> $rate")
    }
  case Left(error) =>
    println(s"Failed to fetch data: $error")
}

// Close the backend
backend.close()
