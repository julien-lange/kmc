# Verifying Asynchronous Interactions via Communicating Session Automata (This is a COPY the ORIGINAL is [HERE](https://bitbucket.org/julien-lange/kmc-cav19/))

###	  by Julien Lange and Nobuko Yoshida  ![aec-badge-cav.png](https://bitbucket.org/julien-lange/kmc-cav19/raw/4f6a3942fe3c5e6dd41a62eff4293888f9abb53e/aec-badge-cav.png)

---
The purpose of this document is to describe in details the steps
required to assess the artifact associated to our paper.

We would like you to be able to

1. understand how to use the tool to verify given communicating automata,
   
2. reproduce our benchmarks (i.e., Table 2 and Figure 6), and

3. use the tool to verify your own communicating automata.

/!\ *Bear in mind that the benchmark data in the paper was generated
using an 8-core Intel i7-7700 machine (the tool makes heavy use of
multicore, when available), while the script attached here are
tailored to a **single-core** VM. Instructions to run the tool in
**multicore** mode are in README.md (see
http://bitbucket.org/julien-lange/kmc-cav19).*
---

## STEP 0: Getting started

For instructions for compiling the tool on your own machine,
see [README.md](https://bitbucket.org/julien-lange/kmc-cav19/src/master/README.md).


Alternatively, you may use the VM prepared for the CAV'19 artifact evaluation.

1. Download our [modified VM](https://www.cs.kent.ac.uk/people/staff/jl703/cav19-ae/kmc-lange-yoshida.ova).
2. Load it in [Virtual Box](https://www.virtualbox.org/) and boot it.
3. Open a terminal and navigate to `~/kmc-cav19`.
4. Follow the instructions below.

In the following, we assume that you are in the `kmc-cav19` directory
(where all *.hs file are located).


## STEP 1: Understanding the KMC tool

The KMC tool has one purpose: check that a given system of
communicating automata is *correct*, i.e., all messages that are sent
are received, and no automaton gets permanently stuck in a receiving
state; this is called **safety** in the paper.

The KMC tool has two modes, one is "interactive" the other is oriented
towards benchmarking. We explain each one separetely.


### Interactive mode

In the interactive mode, the KMC tool takes two parameters: a (path to
a file containing a) system of communicating automata and a bound `k`.

Example with k=2

```
$ ./KMC tests/benchmarks/gmc-runningexample 2 --fsm
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
$ ./KMC tests/benchmarks/gmc-runningexample 2 --fsm --cibi
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
$ ./KMC tests/running-example.txt 3 
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

### Benchmarking mode
In the benchmarking mode the tool takes a pair of bounds (min and
max). For instance, you can run

```
$ ./KMC tests/ex-benchmark.txt 1 5 --fsm --debug
Trying with bound 1 (*7* states and *6* transitions in RTS)
Trying with bound 2 (*13* states and *12* transitions in RTS)
Trying with bound 3 (*37* states and *36* transitions in RTS)
3-OBI OK, 3-C/SIBI OK, 3-exhaustivity OK, checking for safety...
3-MC: True
```

The tool will find the smallest k between 1 and 5 such that the system
is k-OBI, k-SIBI (or k-CIBI), and k-MC. If not such k is found, the
tool says *"I don't know"*, e.g.,

```
$ ./KMC tests/ex-benchmark.txt 1 2 --fsm  --debug
Trying with bound 1 (*7* states and *6* transitions in RTS)
Trying with bound 2 (*13* states and *12* transitions in RTS)
I don't know!
```

This is expected as the tool cannot find a k within the given bounds
for which the system is k-MC.

NB: here the flag `--debug` enables the verbose mode (intermediate
attempts are printed).


---------------------------------------------------------------------

## STEP 2: Running the benchmarks

### Results in Table 2 (examples from the literature)

The purpose of these examples is to demonstrate how the tool works on
existing examples from the literature.

The examples in this table are located `./tests/benchmarks/` (those
coming from [47] are in `tests/Protocols/`). The data for these
benchmarks can be re-generated using the following script:

```
./ae-lit-benchmarks.py  	      	  # should take a few seconds
```

which runs our tool (in benchmark mode) on each example listed in Table
2 with min bound = 1 and max bound = 2.

The results will be in the file `benchmarks-fromlit.csv` where:

* Column 1: file name,
* Column 2: number of states in RTS
* Column 3: number of transitions in RTS
* Column 4: average checking time
* Column 5- : timing of each run 

For the artifact evaluation, this script runs 2 iterations of each
example. You can change this by adapting the `maxiterations` variable
in `ae-lit-benchmarks.py`. The script contains the path to each
example.

The script above only records timings and sizes. To verify that each
example satisfy the stated properties, please use the *interactive
mode* on the example files.

NB: please rename or delete `benchmarks-fromlit.csv` before running
the script again or new results will be appended to the old file.


### Results in Figure 6 (generated examples)

The purpose of these set of benchmarks is to demonstracte the
scalability of the tool on large examples. All the data used in the
paper is available in `data/benchmarks-data-run-on-8core-machine/`.


#### Running the entire benchmark set (12 hours)

```
./ae-parameterised-benchmarks.py  # This will take about 12 hours
./mkplot.py
```

NB: we have executed this script on the CAV VM, and running the whole
script took just over 12 hours. The results are available in
`./data/benchmarks-data-run-on-CAV-VM`. We propose a similar but
smaller set of benchmarks below.

The `ae-parameterised-benchmarks.py` scripts generate
intermediate timing data that is recorded in 3 files (see files named
`parametrised-benchmarks-X-*.csv`).

The structure of these files is as follows:

* Column 1: n (size of alphabet) 
* Column 2: k (bound)
* Column 3: m (number of peers / 2)
* Column 2: number of states in RTS
* Column 3: number of transitions in RTS
* Column 4: average checking time
* Column 5- : timing of each run 

For the artifact evaluation, this script runs 1 iteration of each
example. You can change via the `maxiterations` variable in
`ae-lit-benchmarks.py` (larger values will give smoother data).

The `mkplot.py` generates .eps plots which should replicate Figure 6
(modulo slowdown due to single core), these plots will be found in the
./plots` directory.

#### Running a smaller benchmark set

You can run a smaller set of benchmarks using the following script:

```
./ae-quick-parameterised-benchmarks.py  # This will take about 2h20m
./mkplot.py
```

This takes about 2h20m on the VM by using smaller k,n, and m. The
structure of the outputs are the same the ones described above.

#### Replicating data points


Alternatively, you can run specific data points using the command
`./GenCFSMs <n> <k> <m>`. For instance,

```
$ ./GenCFSMs 2 4 1 > cfsm.txt
$  time ./KMC cfsm.txt 1 4 --fsm --debug
Trying with bound 1 (*7* states and *6* transitions in RTS)
Trying with bound 2 (*31* states and *30* transitions in RTS)
Trying with bound 3 (*127* states and *126* transitions in RTS)
Trying with bound 4 (*766* states and *1020* transitions in RTS)
4-OBI OK, 4-C/SIBI OK, 4-exhaustivity OK, checking for safety...
4-MC: True

real	0m1.606s
user	0m1.604s
sys	0m0.000s
```

This generate a system of communicating automata consiting of 4
partitions of 2 automata (8 peers), that exchange messages 'a' and 'b'
(n=2), and send a series of 4 messages before receiving 4 times (k=4).

Above the second parameter of `GenCFSM` (2) should be greater than or
equal to the third parameter of `KMC`. Otherwise, you will get

```
$ ./GenCFSMs 2 4 1 > cfsm.txt
$ time ./KMC cfsm.txt 1 3 --fsm --debug  # Note: 3 < 4
Trying with bound 1 (*7* states and *6* transitions in RTS)
Trying with bound 2 (*31* states and *30* transitions in RTS)
Trying with bound 3 (*127* states and *126* transitions in RTS)
I don't know!

real	0m0.024s
user	0m0.020s
sys	0m0.000s

```




---------------------------------------------------------------------
## STEP 3: Checking your own communicating automata

You can write your own examples using a (1) textual (session
type-like) language or (2) finite-state machine language.


### Textual (default).

The file should consist of a list of named session types, i.e., a list
of `A: Ta, ..., Z:Tz` where each Ta..Tz belongs to a language
generated by the following grammar:

```
T ::= rec x . T
  |   A!msg; T
  |   {B1!m1; T1, ..., Bk!mk; Tk}
  |   end
  |   x
```

For instance, the encoding of our running example
(Figure 1) in this language is as follows:
```
C: rec x . S!req;S!data; {S?ko;x, S?ok;end, S?err;end}

S: rec x . C?req; { C!ko;C?data;x
       	   	  , C!ok;C?data; rec y. L!log;y}

L: rec y. S?log;y
```


See `tests/running-example.txt` for the corresponding file.

Example of invocation:

```
$ ./KMC tests/running-example.txt 3
CSA: True
Basic: True
reduced 3-exhaustive: True
reduced 3-safe: True
```

### State machines (--fsm flag).

The file should consist of a list of numbered state machines, e.g.,
this is the encoding of our running example (Figure 1) where "--"
indicate a comment.

```haskell
.outputs  -- <-- This is machine #0
.state graph
q0 1 ! req q1
q1 1 ! data q2
q2 1 ? ko q0
q2 1 ? error q3
q2 1 ? ok q4
.marking q0 -- <-- initial state
.end


.outputs -- <-- This is machine #1
.state graph
q0 0 ? req q1
q1 0 ! ko q2
q2 0 ? data q0
q1 0 ! ok q3
q3 0 ? data q4
q4 2 ! log q4
.marking q0  -- <-- initial state
.end


.outputs -- <-- This is machine #2
.state graph
q0 1 ? log q0
.marking q0  -- <-- initial state
.end
```

This example is also in `tests/benchmarks/client-server-logger.txt`


Example of invocation:
```
$ ./KMC tests/benchmarks/client-server-logger.txt --fsm 3
CSA: True
Basic: True
reduced 3-exhaustive: True
reduced 3-safe: True
```






# ADDITIONAL INFORMATION

All set-up and benchmark was performed on the following machine:

* Intel Core i7-7700 CPU @ 3.60GHz x 8
* 15.5 GiB memory
* OS: ubuntu 16.04 LTS (64-bit)
* GHC 7.10.3
* Python 2.7.12

The original benchmarks were generated using:

* Compile: `ghc KMC -threaded 	       # to enable multicore-support`
* Run: `./KMC <path> <bound> +RTS -N8   # to use 8 cores`

See main instructions ([README.md](https://bitbucket.org/julien-lange/kmc-cav19/src/master/README.md)) for more information.

The source code is included in the root directory (*.hs files). The
most interesting file is CFSM.hs which includes functions to build the
reduced transition system (buildReduceTS) and the several functions
checking the desidred properties (ksafe, kexhaustive, koutputindep,
kinputindep, etc).


NB: This tool is part of a large tool suite (under-construction) which
is available at http://bitbucket.org/julien-lange/k-checking/.

