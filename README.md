## Advent of code 2020

My solutions to the puzzles of the advent of code 2020 (https://adventofcode.com/2020)

The code I write is usually to solve quickly the puzzle so there is no error handling: if the input is not expected, the program stops.
If I were writing production code, the 'exceptions' in the input would be logged to be analyzed and the program would continue.

### Usage

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL.

`sbt test` will run the unit tests (usually one for every solution and the smaller example input). Again, no intent to be complete here !
