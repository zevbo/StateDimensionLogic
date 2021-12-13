# RobotState

## Quick Intro

In short, RobotState is provides functionality for storing information
about a robot in a single location (ie: a determinstic state machine),
as well as a number of features for interacting with the state.

For a small example of it's, see
https://github.com/zevbo/RobotState/tree/main/simple_test.

## Installation

### Dependencies

RobotState currently depends on dune, core and ppx_jane. If you have
opam installed (installation instructrions for opam:
https://opam.ocaml.org/doc/Install.html) you can install all other
necessary dependencies with:

```
opam install dune core ppx_jane
```

### RobotState Installation

Currently, because it's still starting up, robot state is not
available as a standard package. If you would like to use it in it's
current form, simply clone the repository on to your local, and use
folders src and state_estimators in any dune project as desired.

## A Note On Tooling

We're all trying to become better software engineers and programers
all the time. When we say "better," we're mainly thinking about a few
things:

1. Writing more reliable code
2. Completeling projects more quickly
3. Creating faster programs
4. Creating programs with enhanced utility

Ultimately, this project is an attempt to provide tooling to
dramatically help with 1 and 4. In the spirit of this project, I'd
like to encourage some extra thought into your choice of tooling with
the goal of optimizing these 4 ideals. In additon, there are a couple
of tools in specific I would like to recommend for working with robot
state:

- I _strongly_ recommend using
  [dune](https://dune.readthedocs.io/en/stable/quick-start.html) for
  compilation and execution. Notably, the linked tutorial does not
  explain how to create a dune-project file, which you can do simply
  by copying the one in this repository, and putting it at the bade of
  your project. If the linked tutorial is further confusing, feel free
  to take a look at the source code for this project as an example.
- I recommend using [VSCode](https://code.visualstudio.com/download)
  for an IDE
- The ocaml-platform VSCode extension provides very nice linting
- I also recommend using the auto-formatter provided by
  ocaml-platform. To do so, you need a .ocamlformat file at the root
  of your proejct (I suggest copying the one from this repository),
  and run the below opam command. Alternatively, you can use the
  ocaml-format extention on VSCode, but I suggest trying to get the
  auto-formatter to work through ocaml-platform first.

  ```
  opam install ocamlformat ocaml-lsp-server
  ```

## Using RobotState: Tutorial

This tutorial will be split up into three stand-alone sections:

- _Beginner_: this section will be more than enough to get a simple
  robot or simulation working, and keep many of the benefits of the
  package
- _Intermediate_: this section is perfect for anyone looking to create
  a decently sized project using this package. It will give you the
  tools to use all of the provided features effectively and in the
  manner that they were meant
- _Advanced_: this section will take a look at more of the underlying
  implementation, and will be useful for anyone who is spending more
  substnatial time with this package. It will allow for faster
  debugging, more effective design, and even the ability to add more
  features.

### Beginner

The first concept we will introduce is an `'a Sd.t`.

Let's turn out attention to a simple example, in which RobotState is
used to implement a simulation of a body moving in a single
dimension. After starting stationary, every tick, this body's linear
velocity will increase by 0.1 plus a number chosen at random from the
uniform distribution (0.5, -0.5). And then, the position of the body
will increase by the velocity. We will also print both the velocity
and position of the body each tick. It also has a light, that will
turn on once the robot passes position 50.

The example can be found at the following link, and we will look
through the example bottom up:
https://github.com/zevbo/RobotState/tree/main/simple_test
To run it, simply cd into the simple_test directory, and run:
```dune exec ./simple_test.exe```

Let's first turn our attention towards the simplest file:
sds.ml. Aside from some `open`s, it only contains three short let
declerations:

```ocaml
let (x : float Sd.t) = Sd.create "x" Float.sexp_of_t
let (v : float Sd.t) = Sd.create "v" Float.sexp_of_t
let (light_on : bool Sd.t) = Sd.create "light on" Bool.sexp_of_t
```
Here, x and v both represent a unique key (called a state dimension or
Sd.t) corresponidng to a value being stored corresponding to our robot
simulation. The first value to Sd.create is the name of the state
dimension for debugging purposes, and the second value is a sexp_of
function. This sexp_of function t specifies what data is stored with
that state dimension. The x position and velocity are both floats, so
we use Float.sexp_of_t to initialize those state dimensions. Whether
or not the light is on is a boolean, so we pass Bool.sexp_of_t to
initalize its Sd.t.

Now let's turn our attention twoards update_v.ml and update_x.mls. Update_v.ml defines
an Est.t instance that corresponds to the logic for updating the
velocity of the robot. Update_x.ml does the same for updating the position.
We can see the major portion of the two files are the
following:

```ocaml
let logic =
  [%map_open.Sd_lang
    let v = sd_past Sds.v 1 (V 0.0) in
    let diff = 0.1 +. Random.float_range (-0.5) 0.5 in
    Rs.set Rs.empty Sds.v (v +. diff)]
```

```ocaml
let logic =
  [%map_open.Sd_lang
    let x = sd_past Sds.x 1 (V 0.0)
    and v = sd Sds.v in
    Rs.set Rs.empty Sds.x (x +. v)]
;;
```

To create the logic for an estimator, you write the logic inside the
```%map_open.Sd_lang``` syntax. The logic for an estimator can always
be broken up into three parts:

- Decleration of required state dimensions
- Logic
- Return of new robot state

Let's start with "the decleration of required state dimensions." This section is marked by the first let statement, and in it you can use the following functions to rechieve data other estimators have declared about the robot:
```ocaml
sd : 'a Sd.t -> 'a
sd_past : 'a Sd.t -> int -> Sd_lang.default -> 'a
```
The sd function gives you the value of a state dimension that has been estimated in the current tick. sd_past gives you the value of a state dimension that was estimated some number of ticks ago (0 = this tick, 1 is previous tick). The default value says what value to use in the case where there are fewer than the request number of states recorded so far. The decleration for Sd_lang.default is the following:
``` ocaml
type 'a default =
  | Safe_last of 'a (* like last, except in case of just one state and length is not 0, use 'a)
  | V of 'a (* in case of too few states, return associated value of type 'a)
  | Last (* in case of too few states, use the oldest state *)
  | Unsafe (* in case of too few states, fail *)
 ```
To get full safety, it is recommended to try and stick to using the ```Safe_last``` and ```V``` cases. 

In the first let statemnt, you declare all values about the robot

### Intermediate
### Advanced

Here, we introduce five major concepts:

- **'a Sd.t** (stands for state dimension): a name for a piece of data
  you would like to store. Examples: yaw : float Sd.t, joint2_angle :
  float SDdt, button_reading : bool Sd.t. The type 'a determines the
  type of value associated with Sd.t.
- **'a Sd_lang.t**: an 'a Sd_lang.t represents some process that uses
  Sd bindings, and returns an 'a when applied
- **RobotState.t**: a RobotState.t is an object that maps any 'a Sd.t
  to an 'a. It is designed so that you can store SDs paramterized over
  different types.
- **Estimators**: an estimator is a struct with two values: a
  Robot_state.t Sd_lang.t, and a Sd.Packed.t set. The Robot_state.t
  returned by the Sd_lang.t corresponds to new bindgins about the
  robot. The Sd.Packed.t set corresponds to the SDs that are expected
  in the Robot_state.t returned by the Sd_lang.t.
- **Seq_model.t** (short for Sequential Model): A Seq_model.t is a
  list of estimators that can be applied one after another, with
  guarantees that all Sd bindings that any given estimator requests
  will be there.
- **Rprogram.t** (short for Robot Program): An Rprogram.t contains a
  Seq_model.t, as well as a unit Sd_lang.t. When run, the Rprogram.t
  will on loop run the model, and then run the unit Sd_lang.t.  There
  are 5 major concepts in RobotState
- **RobotStateHistory.t**: Stores a sequence of robot states, each one
  representing a different time stamp. For most robots, there will be
  one copy that your code treats as the true value, although this is
  not inforced.
