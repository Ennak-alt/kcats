# kcats
Interpreter and inverter for an instruction set for a reversible stack machine

## Build
To build the executable and place it in the current directory run `make`.

Alternatively you can run `cabal install --installdir=dir --overwrite-policy=always` to place it in the directory `dir`.

## Run
After building you should have an executable `kcats`, alternatively you can run
```sh
cabal run kcats -- ...
```
Where `...` are the arguments to the program

## Interpreter examples
In these examples, the inputs are given in static memory

### Fall simulation
Running the the fall simulation in forward and backward direction (respectively). The stack has the following layout:

    Stack layout (bottom to top)
    |    v       |   3 
    |    h       |   2
    |    tend    |   1
    |    t       |   0

Here 'tend' (at address 1) is set to 3 and 'h' (height at address 2) is set to 176:
```sh
./kcats sample-programs/fall-forward.kc --mem 1 3 2 176 
```

Here 't' (at address 0) is set to 4, 'tend' (at address 1) is set to 3 and 'v' (velocity at address 3) is set to 40
```sh
./kcats sample-programs/fall-backward.kc --mem 0 4 1 4 3 40
```

### Fibonacci
Running the the fibonacci programs in forward and backward direction. The stack has the following layout:

    Stack layout (bottom to top)
    |    n        |   2
    |    x2       |   1
    |    x1       |   0

The inputs to these are loaded directly onto the stack in the file, and the outputs is on the stack. There is two versions: one implented with an iterative approach, and one using a recrusive approach. First the two versions in forward direction, thse have n=13:
```sh
./kcats sample-programs/fib-it-forward.kc
```
```sh
./kcats sample-programs/fib-rec-forward.kc
```
There there is the backwards versions, which has x1=377 and x2=610:
```sh
./kcats sample-programs/fib-it-backward.kc
```
```sh
./kcats sample-programs/fib-rec-backward.kc
```


### Run-length encoder
Here is an example of running the run-length encoder with the array [7,7,7,3,3,5,5,5,5], which starts from address 0:
```sh
./kcats sample-programs/encode.kc --mem 0 7,7,7,3,3,5,5,5,5
```
The result is stored in the static memory from address 50.

### Run-length decoder
Here is an example of running the run-length decoder with the array [7,3,3,2,5,4] from address 50:
```sh
./kcats sample-programs/decode.kc --mem 50 7,3,3,2,5,4
```
The result is stored in the static memory from address 0.
`decode.kc` is the result of inverting the encoder in `encode.kc`,

## Inverter example
This will invert the encoder into a decoder written to `decode.kc`:
```sh
./kcats --inv sample-programs/encode.kc -o sample-programs/decode.kc 
```
Or turn the decoder back into an encoder:
```sh
./kcats --inv sample-programs/decode.kc -o sample-programs/encode-from-decode.kc 
```