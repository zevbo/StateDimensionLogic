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

Let's turn out attention to a simple example, simulaitng a body moving in a single dimension. It starts stationary, and every tick, its linear velocity increases by 0.1 + a random number between 0.5 and -0.5. It also has a light, that will turn on once the robot passes position 50.

The example can be found at the following link, and we will look
through the example bottom up:
https://github.com/zevbo/RobotState/tree/main/simple_example.
To run it, simply cd into the run_simple_example directory, and run:

```dune exec ./run_simple_example.exe```

#### State Dimensions

Let's first turn our attention towards the simplest file:
sds.ml. Aside from some `open`s, it only contains three short let
declerations:

```ocaml
let (x : float Sd.t) = Sd.create "x" Float.sexp_of_t
let (v : float Sd.t) = Sd.create "v" Float.sexp_of_t
let (light_on : bool Sd.t) = Sd.create "light on" Bool.sexp_of_t
```
Here, x, v and light_on are each a unique key (called a state dimension or
Sd.t) corresponidng to a value being stored corresponding to our robot
simulation. The first value to Sd.create is the name of the state
dimension, and the second value is a sexp_of function. This sexp_of function specifies what data is stored with
that state dimension. The x position and velocity are both floats, so
we use Float.sexp_of_t to initialize those state dimensions. Whether
or not the light is on is a boolean, so we pass Bool.sexp_of_t to
initalize its Sd.t.

#### Estimators

Now let's turn our attention towards update_v.ml and update_x.mls. Update_v.ml defines
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
- Create and return new robot state

Let's start with "the decleration of required state dimensions." This section is marked by the first let statement, and in it you can use the following functions to rechieve data other estimators have declared about the robot:
```ocaml
val sd : 'a Sd.t -> 'a
val sd_past : 'a Sd.t -> int -> Sd_lang.default -> 'a
```
The sd function gives you the value of a state dimension that has been estimated in the current tick. sd_past gives you the value of a state dimension that was estimated some number of ticks ago (0 = this tick, 1 is previous tick). The default value says what value to use in the case where there are fewer than the request number of states recorded so far. The decleration for Sd_lang.default is the following:
``` ocaml
type 'a default =
  | Safe_last of 'a (* like last, except in case of just one state and length is not 0, use 'a *)
  | V of 'a (* in case of too few states, return associated value of type 'a *)
  | Last (* in case of too few states, use the oldest state *)
  | Unsafe (* in case of too few states, fail *)
 ```
To get full safety, it is recommended to try and stick to using the ```Safe_last``` and ```V``` cases. 

If you need multiple state dimensions, you can use the ```and``` keyword as seen in update_x.ml.

The middle section, the logic, is the simplest: you simply write code just the way you normally would.

In the final section, you need to create and then return a RobotState.t (also aliases as Rs.t). An Rs.t maps 'a Sd.t values to 'a values. The Rs.t you return from the estimator indicates the new values that those Sd.t values should have for this time step. The following functions and values give you all the functionality you need to create an Rs.t:
```ocaml
val empty : Rs.t (* an empty Rs.t *)
val set : Rs.t -> 'a Sd.t -> 'a -> Rs.t (* returns a new Rs.t with all the bindings of the previous one, as well as the new binding *)
```

Often, you will want to write logic that does not estiamte anything about the state. In this case, you want to simply return ```Rs.empty```.

Finally, to create an estimator, you also need to create a set declaring what Sd.t values your estimator returns. For example, in the case of update_v.ml we have:
```ocaml
let sds_estiamting = (Set.of_list (module Sd.Packed) [ Sd.pack Sds.v ])
let est = Est.create logic sds_estiamting
```

Now, I encourage you to take a look at light_on.ml as well as ```print_est``` declared in main.ml, and attempt to understand them on your own. If you run into trouble, come back and look at this tutorial!

#### Sequential Model

At this point, we've written all of the logic of the program. We simply need to run it. To do this, we are going to use a ```Seq_model.t``` at the end of main.ml:
```ocaml
let model = Seq_model.create [ Update_v.est; Update_x.est; Light_on.est; print_est ]
let run () = Seq_model.run model ~ticks:(Some 100)
```
Here, ```Seq_model.run``` will take a number of ticks (None for no limit) and run each estimator, as defined by the list passed to ```Seq_model.create``` one after the other on each tick.

#### Safety Checks

One of the major features of this package is the safety checks it provides. When you create and then run a ```Seq_model.t```, it is guaranteed that every state dimension requested by each state estimator will be available. To see this check in action, let's try flipping the ```Update_x.est``` and ```Update_v.est```.
``` ocaml
++ let model = Seq_model.create [ Update_x.est; Update_v.est; Light_on.est; print_est ]
-- let model = Seq_model.create [ Update_v.est; Update_x.est; Light_on.est; print_est ]
```
Notably, if we were to run this, because Update_x.est now comes before Update_v.est, it will not know the current velocity. Thus, it should fail. So, what happens when we run ```dune exec ./run_simple_example.exe```?

```
Uncaught exception:  
  
  State_estimators.Seq_model.Premature_sd_req("v")

Raised at State_estimators__Seq_model.create in file "state_estimators/seq_model.ml", line 105, characters 33-82
Called from Simple_example__Main.model in file "simple_example/main.ml", line 15, characters 12-84
```

Rather than failing after you run the program, the error is caught by the sequential model when the model is created! The ```Seq_model.t``` will also detect whether or not two estimators are attempting to estimate the same Sd.t, or if an estimator requires a state dimension that is never estiamted (rather than requiring one before it estimated).

To see one other kind of safety, let's say we forgot to add the value for ```light_on``` in the light on estimator:
```ocaml
++ let _x = sd Sds.x in
++ Rs.empty
-- let x = sd Sds.x in
-- Rs.set Rs.empty Sds.light_on Float.(x > 50.0)
```
When we run we get:
```
Uncaught exception:  
  
  State_estimators.Est.Missing_sd("light_on")

Raised at State_estimators__Est.execute in file "state_estimators/est.ml", line 35, characters 26-69
Called from State_estimators__Seq_model.apply.(fun) in file "state_estimators/seq_model.ml", line 29, characters 28-76
Called from Stdlib__list.fold_left in file "list.ml", line 121, characters 24-34
Called from State_estimators__Seq_model.tick in file "state_estimators/seq_model.ml", line 125, characters 48-57
Called from State_estimators__Seq_model.run in file "state_estimators/seq_model.ml", line 131, characters 18-26
Called from Dune__exe__Run_simple_example in file "run_simple_example/run_simple_example.ml", line 1, characters 9-35
```
This error is unfortunatley not catchable before we run the program. But, if an estimator ever forgets to return a binding for state dimension it said it was estiamting (or returns an extra state dimension), the program will still catch it.

And that's it! You're now ready to use this package on whatever robot you choose!

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
