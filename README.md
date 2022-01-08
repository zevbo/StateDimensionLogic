# State Dimension Logic

## Quick Intro

In short, State Dimension Logic provides functionality for storing information
about a robot (or any other process with similar logic with time steps) in a single location (ie: a determinstic state machine).

For a small example of it's, see
https://github.com/zevbo/StateDimensionLogic/tree/main/simple_example.

## Installation

### Dependencies

If you have opam installed (installation instructrions for opam:
https://opam.ocaml.org/doc/Install.html) you can install all other
necessary dependencies with:

### Sd_logic Installation

```
opam install sd_logic
```

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

## Using State Dimension Logic: Tutorial

This tutorial will be split up into three stand-alone sections:

- _Simple_: this section will be more than enough to get a simple
  robot or simulation working, and keep many of the benefits of the
  package
- _Detailed_: this section is perfect for anyone looking to create
  a decently sized project using this package. It will give you the
  tools to use all of the provided features effectively and in the
  manner that they were meant
- _In-depth_: this section will take a look at more of the underlying
  implementation, and will be useful for anyone who is spending more
  substnatial time with this package. It will allow for faster
  debugging, more effective design, and even the ability to add more
  features.

### Simple

Let's turn out attention to a simple example, simulaitng a body moving in a single dimension. It starts stationary, and every tick, its linear velocity increases by 0.1 + a random number between 0.5 and -0.5. It also has a light, that will turn on once the robot passes position 50.

The example can be found at the following link, and we will look
through the example bottom up:
https://github.com/zevbo/RobotState/tree/main/simple_example.
To run it, simply cd into the run_simple_example directory, and run:

```dune exec ./run_simple_example.exe```

#### State Dimensions

Let's first turn our attention towards the simplest file:
sds.ml. Aside from some `open`s, it only contains three short let
declarations:

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

#### Sd_nodes (aka "nodes")

Now let's turn our attention towards update_v.ml and update_x.mls. Update_v.ml defines
an Sd_node.t instance that corresponds to the logic for updating the
velocity of the robot. Update_x.ml does the same for updating the position.
We can see the major portion of the two files are the
following (don't worry if it doesn't immediatly make sense):

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
```

To create the logic for a node, you write the logic inside the
```%map_open.Sd_lang``` syntax. The logic for a node can always
be broken up into three parts:

- Declaration of required state dimensions
- Logic
- Create and return new robot state

Let's start with "the declaration of required state dimensions." This section is marked by the first let statement, and in it you can use the following functions to retrieve data other nodes have declared about the robot:
```ocaml
val sd : 'a Sd.t -> 'a
val sd_past : 'a Sd.t -> int -> Sd_lang.default -> 'a
```
The sd function gives you the value of a state dimension that has been estimated in the current tick. sd_past gives you the value of a state dimension that was estimated some number of ticks ago (0 = this tick, 1 is previous tick). The default value says what value to use in the case where there are fewer than the request number of states recorded so far. The declaration for Sd_lang.default is the following:
``` ocaml
type 'a default =
  | V of 'a (* in case of too few states, return associated value of type 'a *)
  | Last (* in case of too few states, use the oldest state *)
  | Safe_last of 'a (* like last, except in case of too few states and only current state exists, use 'a *)
  | Unsafe (* in case of too few states, fail *)
 ```
To get full safety, it is recommended to try and stick to the ```Safe_last``` and ```V``` cases. 

If you need multiple state dimensions, you can use the ```and``` keyword as seen in update_x.ml.

The middle section, the logic, is the simplest: you simply write code just the way you normally would.

In the final section, you need to create and then return a RobotState.t (also aliases as Rs.t). An Rs.t maps 'a Sd.t values to 'a values. The Rs.t you return from the Sd_node.t indicates the new values that those Sd.t values should have for this time step. The following functions and values give you all the functionality you need to create an Rs.t:
```ocaml
val empty : Rs.t (* an empty Rs.t *)
val set : Rs.t -> 'a Sd.t -> 'a -> Rs.t (* returns a new Rs.t with all the bindings of the previous one, as well as the new binding *)
```

Often, you will want to write logic that does not estiamte anything about the state. In this case, you want to simply return ```Rs.empty```. For an example of this, we can look at 

Finally, to create an Sd_node.t, you also need to create a set declaring what Sd.t values your Sd_node.t returns. For example, in the case of update_v.ml we have:
```ocaml
let sds_estiamting = (Set.of_list (module Sd.Packed) [ Sd.pack Sds.v ])
let node = Sd_node.create logic sds_estiamting
```

Now, I encourage you to take a look at light_on.ml as well as ```print_est``` declared in main.ml, and attempt to understand them on your own. If you run into trouble, come back and look at this tutorial!

#### Sequential Model

At this point, we've written all of the logic of the program. We simply need to run it. To do this, we are going to use a ```Seq_model.t``` at the end of main.ml:
```ocaml
let model = Seq_model.create [ Update_v.node; Update_x.node; Light_on.node; Print.node ]
let run () = Seq_model.run model ~ticks:(Some 100)
```
Here, ```Seq_model.run``` will take a number of ticks (None for no limit) and run each sd_node, as defined by the list passed to ```Seq_model.create``` one after the other on each tick.

#### Safety Checks

One of the major features of this package is the safety checks it provides. When you create and then run a ```Seq_model.t```, it is guaranteed that every state dimension requested by each node will be available. To see this check in action, let's try flipping the ```Update_x.est``` and ```Update_v.est```.
``` ocaml
++ let model = Seq_model.create [ Update_x.node; Update_v.node; Light_on.node; Print.node ]
-- let model = Seq_model.create [ Update_v.node; Update_x.node; Light_on.node; Print.node ]
```
Notably, if we were to run this, because Update_x.est now comes before Update_v.est, it will not know the current velocity. Thus, it should fail. So, what happens when we run ```dune exec ./run_simple_example.exe```?

```
Uncaught exception:
  
  Sd_logic.Seq_model.Premature_sd_req("v")

Raised at Sd_logic__Seq_model.create in file "sd_logic/seq_model.ml", line 104, characters 33-82
Called from Simple_example__Main.model in file "simple_example/main.ml", line 5, characters 12-88
```

Rather than failing after you run the program, the error is caught by the sequential model when the model is created! The ```Seq_model.t``` will also detect whether or not two nodes are attempting to estimate the same Sd.t, or if a node requires a state dimension that is never estiamted (rather than requiring one before it is estimated).

To see one other kind of safety, let's say we forgot to add the value for ```light_on``` in the light on node:
```ocaml
++ let _x = sd Sds.x in
++ Rs.empty
-- let x = sd Sds.x in
-- Rs.set Rs.empty Sds.light_on Float.(x > 50.0)
```
When we run the example using ```dune exec ./run_simple_example.exe``` we get:
```
Uncaught exception:
  
  Sd_logic.Sd_node.Missing_sd("light_on")

Raised at Sd_logic__Sd_node.execute in file "sd_logic/sd_node.ml", line 34, characters 26-69
Called from Sd_logic__Seq_model.apply.(fun) in file "sd_logic/seq_model.ml", line 28, characters 28-81
Called from Stdlib__list.fold_left in file "list.ml", line 121, characters 24-34
Called from Sd_logic__Seq_model.tick in file "sd_logic/seq_model.ml", line 124, characters 48-57
Called from Sd_logic__Seq_model.run.tick in file "sd_logic/seq_model.ml", line 129, characters 12-18
Called from Sd_logic__Seq_model.run in file "sd_logic/seq_model.ml", line 137, characters 18-26
Called from Dune__exe__Run_simple_example in file "run_simple_example/run_simple_example.ml", line 1, characters 9-35
```
This error is unfortunatley not catchable before we run the program. But, if a node ever forgets to return a binding for state dimension it said it was estiamting (or returns an extra state dimension), the program will still catch it.

And that's it! You're now ready to use this package on whatever robot you choose!

### Detailed

This project has a number of layers. Fully understanding how to use the project requires understanding each individual layer, as well as how they fit together. Before we take a look at each layer individually, we're going to take a step back for a quick overview of each major section.

- **State Dimensions, Sd.t**: An ```'a Sd.t``` is a unique key meant to represent some data about the robot. That can be anything from the position of the robot, to that status of a button, to some intermediate data for estimating state about the robot. The type they are paramterized over represents the type of the data that is stored with them.
- **Robot State, RobotState.t or Rs.t**: An ```Rs.t``` is a glorified univ map from ```'a Sd.t``` values to ```'a``` values.
- **Robot State History, RobotStateHistory.t or Rsh.t**: An ```Rsh.t``` will store a number of robot states, each one corresponding to a different time stamp.
- **Sd_lang, 'a Sd_lang.t**: In this section, we will keep the underlying mechanics of what an sd lang is a little bit abstract (for more information go to the in-depth section). What you really need to know is that an Sd_lang defines some peice of logic that uses some data from an ```Rsh.t``` and outputs some value of type 'a, just like a function might.
- **Node, Sd_node.t**: An ```Sd_node.t``` is made up of a ```Rs.t Sd_lang.t``` and a variable representing the ```Sd.t``` values that are expected to be returned by the ```Sd_lang.t```.
- **Model**: A model is not an officially defined concept. Rather, it is meant to denote any type based mainly on (directly or indirectly) ```Sd_lang.t```s that runs the logic of the entire program. Currently, the only model we offer is a sequential model (```Seq_model.t```), which each tick runs the same sequence of ```Sd_node.t```s one after the other.

#### State Dimensions, 'a Sd.t

An ```'a Sd.t``` is a simply a unique key with an associated phantom type as well as a name for debugging. You can use them to refer to data you want to consistently store on the robot. The phantom type represents the type of data that is intended to be stored with the state dimension.

To create one, you can use ```Sd.create : string -> ('a -> Sexp.t) -> 'a t```. The below example shows how we could create two state dimensions of differnt types:
```ocaml
let (yaw : float Sd.t) = Sd.create "yaw" Float.sexp_of_t
let (light_on : bool Sd.t) = Sd.create "light on" Bool.sexp_of_t
```

Notice that the ```sexp_of_t``` function indicated to ```Sd.create``` what type the state dimension should be. Notably, if you'd like to create a state dimension without a natural ```sexp_of_t``` function, you could do the following:

```ocaml
type a = (* some type without a natural sexp_of_t function *)
let (a_sd : a Sd.t) = Sd.create "a sd" (fun (a : a) -> String.sexp_of_t "some-a")
```

We also offer an ```Sd.Packed``` module for doing type-indepdent ```Sd.t``` operations. First of all, the following is the type defintion for ```Sd.Packed.t```:
```ocaml
type t = P : _ sd_t -> t
```

Also, the following function is provided in the ```Sd``` module:
```ocaml
val pack : 'a Sd.t -> Sd.Packed.t
```

Thus, if you wanted a list of state dimensions of different types, the following would be valid:
```ocaml
let (sd_list : Sd.Packed.t list) = [Sd.pack yaw; Sd.pack light_on]
```

Both ```Sd``` and ```Sd.Packed``` modules provide comparison functionality, along with a number of other things. 

#### Robot State, RobotState.t or Rs.t

The ```RobotState``` module provides functionality for creating a pure map from ```'a Sd.t```s to ```'a```s. The following is a subset of the module's mli:
```ocaml
type t [@@deriving sexp_of]

(** [empty] is a [t] with no data *)
val empty : t

(** [find t sd] returns [Some] (the current binding) of [sd] in [t],
   or [None] if no such binding exists. O(log(n)) time complexity in
   size of [t]. *)
val find : t -> 'a Sd.t -> 'a option

(** [set t sd v] sets the given [sd] to [v]. O(log(n)) time complexity
   in size of [t]. *)
val set : t -> 'a Sd.t -> 'a -> t

(** [remove t sd] removes the current binding for [sd] if such binding
   exists. O(log(n)) time complexity in size of [t]. *)
val remove : t -> 'a Sd.t -> t
val removep : t -> Sd.Packed.t -> t

(** [keys t] returns a list of all [sd] for which there exists a
   binding in [t]. O(n) time complexity in size of [t]. *)
val keys : t -> Set.M(Sd.Packed).t
```
 
The above functions provide all the core functionality for robot state.

To build up a ```RobotState.t```, you start with the empty state and then use ```set``` in order to add values. You can then use ```find``` to query, and ```remove``` to get rid of a value. Notably, ```removep``` has an option to be called with an ```Sd.Packed.t```, but ```find``` and ```set``` do not as their functionality is dependent on the type paramter of the ```'a Sd.t``` the recieve.

Using those functions, along with the ```keys``` query, you can build up all the functionality you should need. But for convinece sake, we provide a number of other functions:

```ocaml
(** [mem t sd] returns whether or not [t] has data stored for
   [sd]. O(log(n)) time complexity in size of [t]. *)
val mem : t -> 'a Sd.t -> bool
val memp : t -> Sd.Packed.t -> bool

(** [find_exn t sd] is an unsafe version of find *)
val find_exn : t -> 'a Sd.t -> 'a

(** [use t1 ?to_use t2] calls [set t1 (get t2 sd)] for each [sd] where
   (a) there exists a binding in [t2] and (b) it is in [?to_use] or
   [?to_use] is [None]. O(m*log(n + m)) time complexity where n is the
   size of [t1] and m is the size of [t2]. *)
val use : t -> ?to_use:Set.M(Sd.Packed).t option -> t -> t
val use_extras : t -> t -> t

(** [trim_to t sd_set] removes all [sd]s from [t] that are in [sd_set] *)
val trim_to : t -> Set.M(Sd.Packed).t -> t
```

A quick note on representation, runtime and space complexity: the map is represented as a red-black tree. Therefore, most queries take log(n) time, and sets add log(n) space.

### RobotStateHistory, RobotStateHistory.t or Rsh.t 

A robot state history stores a set of states, each corresponding to a different "tick." The bare bones of the mli is the following:

```ocaml

type t [@@deriving sexp_of]

val create
  : ?sd_lengths:(Sd.Packed.t, int, Sd.Packed.comparator_witness) Map.t
       (** Specify amount of history to keep for particular state
           dimensions. *)
  -> ?min_default_length:int
       (** Defaults to 1. Amount of history to keep for any state dimension
           not mentioned in [sd_lengths] is the maximum of [min_default_lengths] and the largest length in [sd_lengths] *)
  -> unit
  -> t

(** [nth_state t i] returns [Some] of the [i]th most recent state from
   [t], or [None] if [t] doesn't have more than [i] states. O(log(n))
   time complexity in length of [t].  *)
val nth_state : t -> int -> Robot_state.t option
val add_state : t -> Robot_state.t -> t

(** [length t] returns the length of [t]. O(n) time complexity in the
   length of [t]. *)
val length : t -> int
(** [default_length t] returns the maximum length of [t]. O(1) time
   complexity. *)
val default_length : t -> int
```

The create function takes two optional arguments. The first of them allows you to specify a map 


### In-depth
