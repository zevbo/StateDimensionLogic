# State Dimension Logic

## Quick Intro

State Dimension Logic provides a framework for storing information
about a robot (or any other process with similar logic with time
steps) in a single location (i.e.: a deterministic state machine).

You can find a small example of how to use it
[here](https://github.com/zevbo/StateDimensionLogic/tree/main/simple_example).

## Installation

If you have opam installed (installation instructions for opam:
https://opam.ocaml.org/doc/Install.html) you can install `sd_logic`
along with all necessary dependencies with:

```
opam install sd_logic
```

## A Note On Tooling

We're all trying to become better software engineers and programmers
all the time. When we say "better," we're mainly thinking about a few
things:

1. Writing more reliable code
2. Completing projects more quickly
3. Creating faster programs
4. Creating programs with enhanced utility

Ultimately, this project is an attempt to provide tooling to
dramatically help with 1 and 4. In the spirit of this project, I'd
like to encourage some extra thought into your choice of tooling with
the goal of optimizing these 4 ideals. In addition, there are a couple
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

Often, you will want to write logic that does not estimate anything about the state. In this case, you want to simply return ```Rs.empty```. For an example of this, we can look at the logic of print.ml:

```ocaml
let logic =
  [%map_open.Sd_lang
    let v = sd Sds.v
    and x = sd Sds.x
    and light_on = sd Sds.light_on in
    printf "v: %f, x: %f, light on?: %b\n" v x light_on;
    Out_channel.flush stdout;
    Rs.empty]
;;
```

Finally, to create an Sd_node.t, you also need to create a set declaring what Sd.t values your Sd_node.t returns. For example, in the case of update_v.ml we have:
```ocaml
let sds_estimating = (Set.of_list (module Sd.Packed) [ Sd.pack Sds.v ])
let node = Sd_node.create logic sds_estimating
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

One of the major features of this package is the safety checks it provides. When you create and then run a ```Seq_model.t```, it is guaranteed that every state dimension requested by each node will be available. To see this check in action, let's try flipping the ```Update_x.node``` and ```Update_v.node```.
``` ocaml
++ let model = Seq_model.create [ Update_x.node; Update_v.node; Light_on.node; Print.node ]
-- let model = Seq_model.create [ Update_v.node; Update_x.node; Light_on.node; Print.node ]
```
Notably, if we were to run this, because Update_x.node now comes before Update_v.node, it will not know the current velocity. Thus, it should fail. So, what happens when we run ```dune exec ./run_simple_example.exe```?

```
Uncaught exception:

  Sd_logic.Seq_model.Premature_sd_req("v")

Raised at Sd_logic__Seq_model.create in file "sd_logic/seq_model.ml", line 104, characters 33-82
Called from Simple_example__Main.model in file "simple_example/main.ml", line 5, characters 12-88
```

Rather than failing after you call ```Seq_model.run```, the error is caught by the sequential model when the model is created! The ```Seq_model.t``` will also detect whether or not two nodes are attempting to estimate the same Sd.t, or if a node requires a state dimension that is never estimated (rather than requiring one before it is estimated).

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
This error is unfortunatley not catchable before we call ```Seq_model.run```. But, if a node ever forgets to return a binding for state dimension it said it was estimating (or returns an extra state dimension), the program will still catch it.

And that's it! You're now ready to use this package on whatever robot you choose!

### Detailed

This package has a number of layers. Fully understanding how to use the package requires understanding each individual layer, as well as how they fit together. Before we take a look at each layer individually, we're going to take a step back for a quick overview of each major section.

- **State Dimensions, Sd.t**: An ```'a Sd.t``` is a unique key meant to represent some data about the process you are using this package for. That can be anything from the position of a robot, to that status of a button, to some intermediate calculation data. The type they are paramterized over represents the type of the data that is stored with them.
- **Robot State, RobotState.t or Rs.t**: An ```Rs.t``` is a glorified univ map from ```'a Sd.t``` values to ```'a``` values. A little note at this point: this pacakge was originally intended soley for robotics, so for the moment some of the names have "robot" in them. It isn't meant to indicate they can't also be used to describe a different process.
- **Robot State History, RobotStateHistory.t or Rsh.t**: An ```Rsh.t``` will store a number of robot states, each one corresponding to a different time stamp.
- **Sd_lang, 'a Sd_lang.t**: In this section, we will keep the underlying mechanics of what an sd lang is a little bit abstract (for more information go to the in-depth section). What you really need to know is that an Sd_lang defines some peice of logic that uses some data from an ```Rsh.t``` and outputs some value of type ```'a```, just like a function might.
- **Node, Sd_node.t**: An ```Sd_node.t``` is made up of a ```Rs.t Sd_lang.t``` and a variable representing the ```Sd.t``` values that are expected to be returned by the ```Sd_lang.t```.
- **Model**: A model is not an officially defined concept. Rather, it is meant to denote any type based mainly on (directly or indirectly) ```Sd_lang.t```s that runs the logic of the entire program. Currently, the only model we offer is a sequential model (```Seq_model.t```), which each tick runs the same sequence of ```Sd_node.t```s one after the other.

#### State Dimensions, 'a Sd.t

An ```'a Sd.t``` is a simply a unique key with an associated phantom type as well as a name for debugging. You can use them to refer to data you want to consistently store about your robot (or whatever process you're writing code for). The associated type represents the type of data that is intended to be stored with the state dimension.

To create one, you can use ```Sd.create : string -> ('a -> Sexp.t) -> 'a t```. The below example shows how we could create two state dimensions of differnt types:
```ocaml
let (yaw : float Sd.t) = Sd.create "yaw" Float.sexp_of_t
let (light_on : bool Sd.t) = Sd.create "light on" Bool.sexp_of_t
```

Notice that the ```sexp_of_t``` function indicates to ```Sd.create``` what type the state dimension should be. Notably, if you'd like to create a state dimension without a natural ```sexp_of_t``` function, you could do the following:

```ocaml
type a = (* some type without a natural sexp_of_t function *)
let (a_sd : a Sd.t) = Sd.create "a sd" (fun (a : a) -> String.sexp_of_t "some-a")
```

We also offer an ```Sd.Packed``` module for doing type-indepdent ```Sd.t``` operations. The following is the type defintion for ```Sd.Packed.t```:
```ocaml
type t = P : _ sd_t -> t
```

And the following function is provided in the ```Sd``` module:
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

A quick note on representation, runtime and space complexity: the map is created using ```Core.Univ_map```, so it is represented as a red-black tree. Therefore, most queries take log(n) time, and sets add log(n) space if you keep both copies.

### RobotStateHistory, RobotStateHistory.t or Rsh.t

A robot state history stores a set of states, each corresponding to a different "tick." The bare bones of the mli is the following:

```ocaml

type t [@@deriving sexp_of]

val create :
  ?sd_lengths:(Sd.Packed.t, int, Sd.Packed.comparator_witness) Map.t
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
(** [default_length t] returns the default length for a state dimension in [t]. O(1) time
   complexity. *)
val default_length : t -> int
```

The create function takes two optional arguments. ```~sd_lengths``` is a map that specifies for how many ticks of data you'd like to keep for each assocaited state dimension. For state dimensions that aren't keys in ```~sd_lengths```, they are automatically kept for the largest entry in ```~sd_lengths``` (or 1 if no value is passed for ```~sd_lengths```). The ```~min_default_length``` allows you to increase the default length from the maximum entry in ```~sd_lengths```, to ```~min_default_length``` if it is larger. This is clearly inconvinient: it would be much better if ```~min_default_length``` simply dictated the default length on it's own. This currently isn't implemented for computational reasons, but it may be implemented in the future.

As for the rest of the methods, with the doc-comments they should be relativelly self-explanatory.

For convience sake, ```RobotStateHistory``` also has the following methods. It might seem daunting, but the last 60% are all simply variations on ```mem``` and ```find```.
```ocaml

(** [get_current_state t] is equivalent to [get_state 0]. O(1) time
   complexity. *)
val curr_state : t -> Robot_state.t

(** [add_state t state] adds [state] to [t]. O(log(n)) time complexity
   in length of [t]. *)
val add_empty_state : t -> t

(** [use t state] replaces the current state with [Robot_state.use
   (curr_state t) state] *)
val use : t -> ?to_use:Sd.set option -> Robot_state.t -> t

val use_extras : t -> Robot_state.t -> t

(** [find t sd] is equivilant to [Robot_state.find (curr_state t) sd] *)
val find : t -> 'a Sd.t -> 'a option

val find_exn : t -> 'a Sd.t -> 'a

(** [find_past t n sd] is equivalent to [Robot_state.find (Option.value_exn (nth_state t
   n)) sd] when there are at least [n + 1] states. *)
val find_past : t -> int -> 'a Sd.t -> 'a option

val find_past_exn : t -> int -> 'a Sd.t -> 'a
val find_past_def : t -> default:'a -> int -> 'a Sd.t -> 'a
val find_past_last_def : t -> int -> 'a Sd.t -> 'a option

(** [mem t sd] is equivilant to [Robot_state.mem (curr_state t) sd] *)
val mem : t -> 'a Sd.t -> bool

(** [mem_past t n sd] is equivilant to [Robot_state.mem (nth_state t
   n) sd] *)
val mem_past : t -> int -> 'a Sd.t -> bool option

(** [memp t sd] is equivilant to [Robot_state.memp (curr_state t) sd] *)
val memp : t -> Sd.Packed.t -> bool

(** [memp_past t n sd] is equivilant to [Robot_state.memp (nth_state t
   n) sd] *)
val memp_past : t -> int -> Sd.Packed.t -> bool option
```

The first thing I want to draw your attention to is the fact that there is no function to directly set the current (or any past) state. However, you can change the current state using ```use```. You may want to reread the description of ```RobotState.use``` in order to understand ```RobotStateHistory.use```. This means that are only two natural ways to build up the  next state of a ```Rsh.t```:
- Create the entire ```Rs.t``` beforehand, and add it with ```Rsh.add_state```
- Add an empty state to ```Rs.t```, and then utilize ```Rs.use``` to build up the current robot state

The rest of the functions are just variations on ```find``` and ```mem```.

#### Sd language, Sd_lang.t

A quick note about this section: a lot of the inner workings of ```Sd_lang``` will be left as a black box in this section. For a look inside, please look at the in-depth explanation. Either way, this will probably be the most difficult section of the tutorial, so if you don't feel comfortable with it immediatly after finishing, I suggest giving it a second read through and/or playing around with it a little

An ```'a Sd_lang.t``` is used to represent some function on an ```Rsh.t``` that returns a value of type ```'a```. For example, if you wanted to create an ```bool Sd_lang.t``` that simply returned the value of ```bool_sd : bool Sd.t```, it would look like this (don't worry if this is confusing at first; it will be explained more):

```ocaml
let sd_bool = Sd.create "sd_bool" Bool.sexp_of_t
let (simple_sd_lang : bool Sd_lang.t) =
  [%map_open.Sd_lang
    let b = sd bool_sd in
    b]
;;
```
Then, the following snippet of code will create an appropriate ```Rsh.t```, and execute ```simple_sd_lang``` on it:
```ocaml
let rsh = Rsh.create ~min_default_length:2 ()
let rs = Rs.set Rs.empty bool_sd false
let rsh = Rsh.add_state rsh rs

let result = Sd_lang.execute simple_sd_lang rsh
print_string (Bool.to_string result)
```
This will output ```false```.

Let's now take a deeper look at the decleration of an ```Sd_lang.t```. The decleration can be broken up into two parts

- Declaration of required state dimensions
- Simple Body

Let's start with "the declaration of required state dimensions." This section is marked by the first let statement, and in it you can use the following functions to retrieve data other nodes have declared about the robot:
```ocaml
val sd : 'a Sd.t -> 'a t
val sd_past : 'a Sd.t -> int -> 'a default -> 'a t
val sd_history : 'a Sd.t -> int -> (int -> 'a option) t
val state : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> Rs.t t
val state_past : (Sd.Packed.t, Sd.Packed.comparator_witness) Set.t -> int -> Rs.t t
val full_rsh : unit -> Rsh.t t
```
In the given example, ```sd``` is the only of these functions that is used. It gives you the value of a state dimension that has been estimated in the current tick. However, there is substnatially more functionality one might want when creating sd_langs.

```sd_past``` gives you the value of a state dimension that was estimated some number of ticks ago (0 = this tick, 1 is previous tick). The default value denotes what value to use in the case where there are fewer than the request number of states recorded so far.
``` ocaml
type 'a default =
  | V of 'a (* in case of too few states, return associated value of type 'a *)
  | Last (* in case of too few states, use the oldest state *)
  | Safe_last of 'a (* like last, except in case of too few states and only current state exists, use 'a *)
  | Unsafe (* in case of too few states, fail *)
 ```
To get full safety, it is recommended to try and stick to the ```Safe_last``` and ```V``` cases.

```sd_history``` gives back a function where going from indecies to values for the given state dimension. Just like in ```Rsh.find_past```, 0 corresponds to the most recent state, and each larger number corresponds to 1 state earlier.

```state``` and ```state_past``` are useful for grabbing the bindings for a number of ```Sd.t```s as a ```Rs.t```. Notably, if the index for ```state_past``` is larger than the current number of states in the given ```Rsh.t```, then it will simply return an empty ```Rs.t```.

Finally, ```full_rsh``` allows you to simply get the entire ```Rsh.t```. It's usually not recommended, because it is both not specific, and foregoes a lot of the advantages of the ```Sd_lang.t```s. However, we keep it as we understand there may be functionality that the current methods simply don't provide.

In the given example, it only uses one of these functions. If you'd like to use more, simply use Ocaml's ```and``` operator like so:
```ocaml
let (simple_sd_lang : bool Sd_lang.t) =
  [%map_open.Sd_lang
    let b = sd bool_sd
    and c = sd_past bool_sd 1 (Safe_last true) in
    print_string "in the previous tick, bool_sd was ";
    print_endline (Bool.to_string c);
    b]
;;
```

There's one final, and critical, feature in this module:
```ocaml
val dependencies : t -> int Map.M(Sd.Packed).t
```
```dependencies``` takes an ```Sd.Packed.t```, and tells you how many ticks that state dimension needs to be kept for. A little look ahead: this will be useful for passing in a good value for ```~sd_lengths``` when creating an ```Rsh.t```. It might not be immediatly clear what this means, so let's go over an example.

```ocaml
let logic =
  [%map_open.Sd_lang
    let x = sd_past Sds.x 2 (V 0.0)
    and _x2 = sd Sds.x in
    and v = sd Sds.v in
    Rs.set Rs.empty Sds.x (x +. v)]
;;

let deps = Sd_lang.dependencies logic
```
In ```deps```, there are two bindigns: ```Sds.v``` is bound to 0, and ```Sds.x``` is bound to 2. This is because it needs the current velocity, and the value for ```Sds.x``` from state with index 2. Notably, each value in ```deps``` is the oldest version we may need.

#### Sd node, Sd_node.t

An ```Sd_node.t``` is defined as follows:
```ocaml
type t =
  { logic : Robot_state.t Sd_lang.t
  ; sds_estimating : Set.M(Sd.Packed).t
  }
```
The output of ```logic``` of the node represents new bindings to store about the robot, and ```sds_estimating``` is the set of state dimensions that should have bindings. To execute an ```Sd_node.t```, you can use the following function:

```ocaml
type safety =
  | Safe
  | Warnings
  | Unsafe

val execute : safety:safety -> t -> Rsh.t -> Rs.t
```

If you choose ```Safe``` as the safety for the execution, two checks are performed: one to make sure that no extra ```Sd.t```s were in the returned ```Rs.t```, and one to make sure that every requested ```Sd.t``` has a binding in the returned ```Rs.t```.

#### Sequential Model, Seq_model.t

The idea behind a sequential model is to first take a list of ```Sd_node.t```s that should be executed one after another. Importantly, it should provide the ability to run a number of safety checks on the code. What the checks are in specific will be discussed later. For now, here's the mli for using it:

```ocaml
type t

type safety =
  | Safe
  | Warnings
  | Unsafe

val create : ?safety:safety -> Sd_node.t list -> t
val tick : t -> t
val run : ?min_ms:float -> t -> ticks:int option -> unit
```

To create a model, you simply give it a list of ```Sd_node.t```s, and a safety if you wish (it defaults to ```Safe```). The model is create with an empty ```Rsh.t```. Then, you can run one tick using the ```tick``` function, outputing a ```t``` with a  new ```Rsh.t```. Alternativelly, you can run the model for a number of ticks (or without stop), using ```run```. To see this in action, check out ```https://github.com/zevbo/StateDimensionLogic/tree/main/simple_example``` and/or the simple explanation above.

Finally, let's go over the safety checks that it provides. The following checks are performed on creation of the model:
- All requirements of an ```Sd.t``` from the current tick have bindings returned by a previous ```Sd_node.t```
- No two ```Sd_node.t```s return bindgins for the same ```Sd.t```

And these requiremnts are performed whenever the model is run:
- Each ```Sd_node.t``` returns an ```Rs.t``` with the state dimensions it promised, and only the ones it promised

#### Wrap up

Ultimately, when using sd_logic, you should mainly be writing ```Sd_lang.t```s. But, sometimes you will wish to write more complicated pieces of logic that require a deeper understanding of what's going on. Heck, you can even implement your own kind of model that isn't sequential. So have fun with it!

### In-depth

## Coming Soon
