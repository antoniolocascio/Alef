# Alef
Implementation of the Alef language. Alef is a core calculus with algebraic effects, based on Bauer and Pretnar's Eff. This implementation's goal is to showcase Alef's bidirectional type-and-effect system.

## Getting Started

### Prerequisites

* GHC
* Stack

### Installing

To install Alef you should clone this repository, move into the root directory and run:

```
stack setup
stack build
```

This will build an executable called `alef`, which can be found in the `.stack-work` directory.

## Running an Alef program

You can find many examples of Alef programs in the directory `examples`.
The command that runs an Alef program is:

```
stack exec alef examples/good/state.alef
```
