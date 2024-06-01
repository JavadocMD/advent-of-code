# advent-of-code

https://adventofcode.com is an annual code puzzle competition held around Christmas. I wanted to share my (Tyler Coles, JavadocMD) answers for posterity so here they are!

I program my solutions in Scala.

The code files (in `aoc/src/aocYYYY`) are my solutions. Sometimes I write unit tests to support solution development (in `aoc/test/src/aocYYYY`). I've included my inputs and the answers, but these are separate from the other components (in `aoc/resources/aocYYYY`) so you can avoid them in case you'd see that as spoilers, or whatever.

## Setup

I develop using VS Code and Metals, with Mill as my build tool.

I generally use the integrated UI features to run solutions or tests, but if you can also run from the console.

Run a single solution:

`./mill aoc.runMain aoc2023.Day01`

Run all solutions:

`./mill aoc.runMain aoc2023.All`

Run a single test:

`./mill aoc.test aoc2023.Day01Test`

Run all tests:

`./mill aoc.test aoc2023`

## Resources

- https://adventofcode.com/
- https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html
- https://github.com/com-lihaoyi/utest
