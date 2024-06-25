# mezima

The goal of this project is to build an interpreter to a scheme like language in Haskell.

This project is part pf my journey tinkering with with parsers, interpreters and compilers. This is also a part of my journey becoming a proficient functional programmer.

## Project name

Mezima means plot, scheme or conspiracy in the Hebrew language.

## Steps

- [x] Parse S-expressions
- [x] Define an evaluator of S-expressions
- [x] Evaluate numeric sums
- [x] Evaluate numeric products
- [x] Evaluate numeric equality
- [x] Evaluate numeric negatives
- [ ] Evaluate numeric inversion and division
- [ ] Evaluate logical operations
- [ ] Define and evaluate global variables
- [ ] Define and evaluate global functions

## Execute

- Run `stack exec -- mezima-exe` to see "We're inside the application!"
- With `stack exec -- mezima-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test`
