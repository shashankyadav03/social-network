# Haskell Social Network Simulation

## Overview
This project is a Haskell-based simulation of a social network. It demonstrates concurrent computing in Haskell, using threads to represent individual users in a network. Users interact by sending messages at random intervals to randomly chosen recipients. The project showcases thread management, synchronization with MVars, and SQLite for data persistence.

## Features
- **Concurrent User Simulation**: Simulates multiple users interacting within a social network using separate threads.
- **Message Exchange**: Users send messages to randomly chosen recipients, simulating realistic social interactions.
- **SQLite Database Integration**: Utilizes an SQLite database to store and query user interaction history.
- **Thread Safety**: Implements thread-safe operations to manage shared state and avoid race conditions.

## Installation
Ensure you have [GHC](https://www.haskell.org/ghc/) and [Stack](https://docs.haskellstack.org/en/stable/README/) installed on your machine.

1. Clone the repository:
git clone https://github.com/shashankyadav03/social-network

2. Build the project:
stack build

## Usage
Run the simulation using Stack:
stack run

## Testing
To run the unit tests:
stack test

## License
This project is not open-source and is under the [QMUL License](LICENSE).

## Acknowledgments
- Thanks to all contributors who have helped to build and improve this simulation.