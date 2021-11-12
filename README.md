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

## Overview

There are 5 major concepts in RobotState.
- 'a SD.t (stands for state dimension): a name for a piece of data you would like to store. Examples: yaw : float SD.t, joint2_angle : float SD.t, button_reading : bool SD.t. The type 'a determines the type of value associated with SD.t. 
- RobotState.t: a RobotState.t is an object that stores data about the robot at a single instance in time. It can be though of a map from 'a SD.t to 'a values (a single robot state may store SD.t values of different types).
- Robot State History: Stores a sequence of robot states, each one representing a different time stamp. For most robots, there will be one copy that your code treats as the true value, although this is not inforced.
- Estimators: an estimator is a user-defined module that updates a robot state history, that derive from one of the pre-defined estimator module types. 
- Models: data structures made up of estimators for updating the state of the robot. Generally, you should update your any robot state history using a single model.
