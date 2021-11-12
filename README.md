# RobotState

## Quick Intro

In short, RobotState is provides functionality for storing information about a robot in a single location (ie: a determinstic state machine), as well as a number of features for interacting with the state.

For a small example of it's, see https://github.com/zevbo/RobotState/tree/main/simple_test.

## Installation

### Dependencies

RobotState currently depends on dune, core and ppx_jane. If you have opam installed (installation instructrions for opam: https://opam.ocaml.org/doc/Install.html) you can install all other necessary dependencies with:
```opam install dune core ppx_jane```

### RobotState Installation

Currently, because it's still starting up, robot state is not available as a standard package. If you would like to use it in it's current form, simply clone the repository on to your local, and use folders src and state_estimators in any dune project as desired.
