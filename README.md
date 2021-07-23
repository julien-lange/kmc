# KMC: a tool for checking k-multiparty compatibility in communicating session automata 

This is a tool that implements the theory introduced in "[_Verifying Asynchronous Interactions via
Communicating Session Automata_](https://arxiv.org/abs/1901.09606)" by Julien Lange and Nobuko Yoshida.


## Requirements:

You will need [ghc](https://www.haskell.org/platform/) installed on
your machine to run our tool, and python (with some additional
libariries) to run our benchmark scripts. Depending on your setup, you
might need to extra Haskell packages (see below).

The full requirements on a typical ubuntu machine can met as follows:

```
sudo apt install haskell-platform
sudo apt install python-matplotlib
sudo apt install python-scipy
cabal update
cabal install cmdargs ansi-terminal parallel split MissingH --lib
```

## Getting started (download and compile)

* Open a terminal
* Clone the repository: `git clone https://bitbucket.org/julien-lange/kmc-cav19.git`
* Change direcotry:  `cd kmc-cav19`
* Compile: `ghc KMC -threaded` and `ghc GenCFSMs`

## Usage

### Usage for k-MC checking:

* **Interactive:** to check for k-MC (of input file):
  `./KMC <path-to-automata> <k>  +RTS -N<number-of-cpus>`

* **Benchmarking:** to find the least k s.t. k-MC holds for input file:
  `./KMC <path-to-automata> <min-bound> <max-bound> +RTS -N<number-of-cpus>`



See [*the artifact guidelines*](https://bitbucket.org/julien-lange/kmc-cav19/src/master/artifact.md) for more details.


### Other usages:
  
* More information: see `./KMC --help` and `./artifact.md`

* To generate examples: `./GenCFSMs <n> <k> <m>` (run `ghc GenCFSMs.hs` first!)

* To run benchmark script for literature examples: `./lit-benchmarks.py`

* To run benchmark script for generated examples: `./parameterised-benchmarks.py` (run `ghc GenCFSMs.hs` first!)

* Examples are available in `./tests/` (`./tests/benchmarks/` contains examples from other papers).

## Input languages

### Syntactical session types (default)

KMC takes files that contain a list of "named" session types, e.g.,

    S: rec x . C?req; { C!ko;C?data;x
       	   	          , C!ok;C?data;L!log;end}

    L: S?log;end

    C: rec x . S!req;S!data; {S?ko;x, S?ok;end, S?err;end}


### CFSMs (use `--fsm` flag)

KMC takes the same input language as the [GMC tool](https://bitbucket.org/julien-lange/gmc-synthesis/), e.g.,

    -- This is machine #0
    .outputs
    .state graph
    q0 1 ! hello q1
    q0 1 ! bye q1
    .marking q0
    .end

    -- This is machine #1
    .outputs 
    .state graph
    q0 0 ? hello q1
    q0 0 ? bye q1
    .marking q0
    .end
	
	
# Experimental synthesis of global graph

For this you will need to have [petrify](https://www.cs.upc.edu/~jordicf/petrify/) in you PATH. 
