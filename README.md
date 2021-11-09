# KMC: a tool for checking k-multiparty compatibility in communicating session automata 

This is a tool that implements the theory introduced in "[_Verifying Asynchronous Interactions via
Communicating Session Automata_](https://link.springer.com/chapter/10.1007/978-3-030-25540-4_6)".


## Understanding the KMC tool

The KMC tool has one purpose: check that a given system of
communicating automata is *correct*, i.e., all messages that are sent
are received, and no automaton gets permanently stuck in a receiving
state; this is called **safety** in the paper.

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
cabal build
```

## Getting started (download and compile)

* Open a terminal
* Clone the repository: `git clone https://bitbucket.org/julien-lange/kmc-cav19.git`
* Change direcotry:  `cd kmc-cav19`
* Compile: `cabal build`

## Usage

### Usage for k-MC checking:

* **Interactive:** to check for k-MC (of input file):
  `cabal run KMC -- <path-to-automata> <k>  +RTS -N<number-of-cpus>`

* **Benchmarking:** to find the least k s.t. k-MC holds for input file:
  `cabal run KMC -- <path-to-automata> <min-bound> <max-bound> +RTS -N<number-of-cpus>`

### Examples

The KMC tool takes two parameters: a (path to
a file containing a) system of communicating automata and a bound `k`.

Example with k=2

```
$ cabal run KMC -- tests/benchmarks/gmc-runningexample 2 --fsm
CSA: True            			    # The automata are deterministic and have no mixed states (Remark 1)
reduced 2-OBI: True			    # The automata validate 2-OBI (Def 6)
reduced 2-SIBI: False			    # The automata validate 2-SIBI (Def 11)
reduced 2-exhaustive: True 		    # The automata validate 2-exhaustivity (Def 9)
reduced 2-safe: True  			    # The automata validate 2-safety (Def 4)
```

In the above case, we have that this example validates all necessary
conditions, except for 2-SIBI. The k-CIBI check is disabled by default
as it is generally a lot more expensive. It can be activated using the
`--cibi` flag, e.g.,

```
$ cabal run KMC -- tests/benchmarks/gmc-runningexample 2 --fsm --cibi
CSA: True
reduced 2-OBI: True
reduced 2-SIBI: False		# The automata _do not_ validate 2-SIBI (Def 11)
reduced 2-CIBI: True		# The automata validate 2-CIBI (Def 10)
reduced 2-exhaustive: True
reduced 2-safe: True
```

Now, we know that the automata are 2-OBI, IBI, and k-MC, hence they
are *safe* (see Theorem 1 and Lemma 1).


The k-OBI and k-*IBI are only executed when necessary (i.e., if the
automata are not directed). For instance the automata in our running
example are basic (directed), hence we have:

```
$ cabel run KMC -- tests/running-example.txt 3 
CSA: True
Basic: True
reduced 3-exhaustive: True
reduced 3-safe: True
```

In general, *Basic* implies *k-OBI* and *k-SIBI* (for all k), we also
have that *k-SIBI* implies *k-CIBI*. For *k-MC* to (soundly) imply
general safety, k-OBI and k-CIBI must hold.

The `--debug` flag displays information regarding the size of the
generated reduced transition system (RTS).


### Other usages:
  
* More information: see `cabal run KMC -- --help` and `./artifact.md`

* To generate examples: `cabal run GenCFSMs -- <n> <k> <m>`

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







	
## Experimental synthesis of global graph

For this you will need to have [petrify](https://www.cs.upc.edu/~jordicf/petrify/) in you PATH. 
