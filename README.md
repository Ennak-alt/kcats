# kcats
Interpreter and inverter for an instruction set for a reversible stack machine

## build
To build the executable and place it in the current directory run `make`.

Alternatively you can run `cabal install --installdir=dir --overwrite-policy=always` to place it in the directory `dir`.

## run
After building you should have an executable `kcats`, alternatively you can run
```sh
cabal run kcats -- ...
```
Where `...` are the arguments to the program

## Examples
In both fibonacci and fall programs, the input values are directoy loaded into the stack, and are therefore hard-coded. However the encoder and decoder uses the memory as input

### fibonacci
Running the the fibonacci programs in forward and backward direction (respectively):
```sh
./kcats sample-programs/fall-forward
```
```sh
./kcats sample-programs/fall-backward
```

### fall
Running the the fall simulation in forward and backward direction (respectively). The stack has the following layout:

    Stack layout (bottom to top)
    |    v       |   3 
    |    h       |   2
    |    tend    |   1
    |    t       |   0

Here 'tend' (at index 1) is set to 3 and 'h' (height at index 2) is set to 176:
```sh
./kcats sample-programs/fall-forward.kc --mem 1 3 2 176 
```

Here 't' (at index 0) is set to 4, 'tend' (at index 1) is set to 3 and 'v' (velocity at index 3) is set to 40
```sh
./kcats sample-programs/fall-backward.kc --mem 0 4 1 4 3 40
```

### run-length encoder
Here is an example of running the run-length encoder with the array [7,7,7,3,3,5,5,5,5]
```sh
./kcats sample-programs/encode.kc --mem 0 7,7,7,3,3,5,5,5,5
```
The result is stored in the static memory

### run-length decoder
Here is an example of running the run-length decoder with the array [7,3,3,2,5,4]:
```sh
./kcats sample-programs/decode.kc --mem 50 7,3,3,2,5,4
```
`decode.kc` is the result of inverting the encoder in `encode.kc`


### inverting encode to decode
This will invert the encoder into a decoder written to `decode.kc`:
```sh
./kcats --inv sample-programs/encode.kc -o sample-programs/decode.kc 
```