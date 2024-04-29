# Currency Arbitrage Finder

## Overview

This application detects arbitrage opportunities in real-time currency exchange rates using Scala. It utilizes the Bellman-Ford algorithm for finding profitable trading loops in a graph of currency exchange rates.

## Features

- **Real-Time Data Fetching**: Integrates with a currency data API to pull live exchange rates.
- **Arbitrage Detection**: Uses the Bellman-Ford algorithm to identify and report profitable currency exchanges.

### Algorithmic Complexity Analysis

The Bellman-Ford algorithm, used in this application for arbitrage detection, operates with a complexity of 𝑂(𝑉𝐸), where 𝑉 is the number of vertices (currencies) and 𝐸 is the number of edges (currency pairs). This complexity is suitable for detecting negative cycles in the currency exchange graph, which indicate potential arbitrage opportunities. The algorithm is efficient for moderate-sized graphs typical in currency markets, making it well-suited for real-time analysis.

## Getting Started

### Prerequisites

- Scala 2.13 or higher
- Scala CLI installed

### Installation

Clone the repository:

```bash
git clone https://github.com/TwentyOne37/arbitrage-finder.git
cd arbitrage-finder
```

### Usage

Run the application using Scala CLI:

```bash
scala-cli run main.sc
```

## Technologies

- **Scala CLI**: For script compilation and execution.
- **Circe**: For JSON parsing.
- **STTP Client**: For HTTP requests.
- **Cats Effect**: For functional effect management.

## License

This project is available under the MIT License.
