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

## Using RobotState: Tutorial

This tutorial will be split up into three stand-alone sections:
- _Beginner_: this section will be more than enough to get a simple robot or simulation working, and keep many of the benefits of the package
- _Intermediate_: this section is perfect for anyone looking to create a decently sized project using this package. It will give you the tools to use all of the provided features effectivelly and in the manner that they were meant
- _Advanced_: this section will take a look at more of the underlying implementation, and will be useful for anyone who is spending more substnatial time with this package. It will allow for faster debugging, more effective design, and even the ability to add more features.

### Beginner

Let's turn out attention to a simple example, in which RobotState is used to implement a simulation of a body move in a single dimension. After starting stationary, every tick, this body's linear velocity will increase by 0.1 plus a number chosen at random from the uniform distribution (0.5, -0.5). And then, the position of the body will increase by the velocity. We will also print both the velocity and position of the body each tick.

The example can be found at the following link, and we will look through the example from top down: https://github.com/zevbo/RobotState/tree/main/simple_test

Let's start by looking at simple_test.ml. At line 4, we can see we declare a variable "model" with the following line:
![alt text](https://github.com/zevbo/RobotState/blob/main/images/simple-test-model.png)


### Intermediate
### Advanced

Here, we introduce five major concepts:
- **'a Sd.t** (stands for state dimension): a name for a piece of data you would like to store. Examples: yaw : float Sd.t, joint2_angle : float SDdt, button_reading : bool Sd.t. The type 'a determines the type of value associated with Sd.t. 
- **'a Sd_lang.t**: an 'a Sd_lang.t represents some process that uses Sd bindings, and returns an 'a when applied
- **RobotState.t**: a RobotState.t is an object that maps any 'a Sd.t to an 'a. It is designed so that you can store SDs paramterized over different types.
- **Estimators**: an estimator is a struct with two values: a Robot_state.t Sd_lang.t, and a Sd.Packed.t set. The Robot_state.t returned by the Sd_lang.t corresponds to new bindgins about the robot. The Sd.Packed.t set corresponds to the SDs that are expected in the Robot_state.t returned by the Sd_lang.t.  
- **Seq_model.t** (short for Sequential Model): A Seq_model.t is a list of estimators that can be applied one after another, with guarantees that all Sd bindings that any given estimator requests will be there.
- **Rprogram.t** (short for Robot Program): An Rprogram.t contains a Seq_model.t, as well as a unit Sd_lang.t. When run, the Rprogram.t will on loop run the model, and then run the unit Sd_lang.t.
There are 5 major concepts in RobotState
- **RobotStateHistory.t**: Stores a sequence of robot states, each one representing a different time stamp. For most robots, there will be one copy that your code treats as the true value, although this is not inforced.
