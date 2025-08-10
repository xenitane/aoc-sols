# AoC Solutions

> This repository contains my solutions for [Advent of Code](https://adventofcode.com).

## Repository structure

```plaintext
aoc-sols/
|- inputs/
|- test_inputs/
|  |- YYYY-DD.txt
|- test_outputs/
|  |- YYYY-DD.txt
|- .cookie
|- aoc*
|- license.md
|- readme.md
|- <language>/
|  |- run*
|  |- sol/
|  |  |- YYYY-DD.<ext>
```

### Notes

- Place your puzzle inputs in the inputs directory in a file names `YYYY-DD.txt`
- Make sure the input files are without a newline character at the end.
- There are directories for the solutions per language.
- Place your AoC session cookie in the `.cookie` file to be used to fetch the puzzle input automatically.

## Requirements

- A POSIX based operating system.
- Language toolchains

  | Language | Toolchain                                                |
  | :------- | :------------------------------------------------------- |
  | haskell  | [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) |
  | zig      | [Zig compiler toolchain](https://ziglang.org)            |

## Usage Instructions

- Run `chmod +x ./aoc ./*/run` to make the runner scripts executable.
- Run `./aoc help` to get the help message to guide you further.

## [MIT License](license.md)
