# advent-of-code

https://adventofcode.com is an annual code puzzle competition held around Christmas. I wanted to share my (Tyler Coles, JavadocMD) answers for posterity so here they are!

I program my solutions in Scala.

## Organization

The code files (in `aocYYYY/src/aocYYYY`) are my solutions. If you're looking for the current year (during the normal run of AoC), there should be a branch named for the year.

Sometimes I write unit tests to support solution development (in `aocYYYY/test/src/aocYYYY`).

Encrypted inputs and answers files are in `aocYYYY/resources/aocYYYY`.

## Setup

I use git-crypt to encrypt input and answers files, because AoC creator Eric Wastl has requested these not be shared.

When first cloning the repo, these files will be encrypted. Use the stored symmetrical key to unlock the repo.

`git-crypt unlock /path/to/key`

## Dev

I develop using VS Code and Metals, with Mill as my build tool. I typically use the integrated UI for running solutions or tests, but you can also run from the console.

Run a single solution:

`./mill aoc2023.runMain aoc2023.Day01`

Run all solutions:

`./mill aoc2023.runMain aoc2023.All`

Run a single test:

`./mill aoc2023.test aoc2023.Day01Test`

Run all tests:

`./mill aoc2023.test aoc2023`

## Resources

- https://adventofcode.com/
- https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html
- https://github.com/com-lihaoyi/utest
