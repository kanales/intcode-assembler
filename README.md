# intcode-assembler

Simple assembler to write programs for a complete (Intcode)[https://adventofcode.com/2019/day/9] computer.

## Specification

### Parameter modes

Parameters can be used in severalm modes:
+ **immediate:** indicated by a naked number; `100` uses the integer 100 as a parameter.
+ **position:** indicated by brackets; `[100]` uses the number in **position** 100.
+ **relative:** indicated by `R` preceding the number; `R100` iuses the number 100 steps after the relative base.

### Avaliable instructions

#### Arithmetic instructions

`ADD P1 P2 D`: set `D` to the sum of `P1` and `P2`
`MUL P1 P2 D`: set `D` to the product of `P1` and `P2`

#### Comparison instructions

`EQU P1 P2 D`: set `D` 1 if `P1 = P2`, 0 otherwise.
`LT  P1 P2 D`: set `D` 1 if `P1 < P2`, 0 otherwise.

where two parameters `a = b` if they have the same value after dereferencing. As an example:

```
ADD 1 1 [100]
EQU 1 [100] [200]
```
Then `[200]` contains `1` as `[100]` is 1 after dereferencing.

#### IO instructions

`INP D`: set `D` to a value obtained from stdin.
`OUT P`: output parameter `P` to stdout

#### Jumps

`JNZ P1 P2`: jump to cell in position `P2` if `P1 = 0`
`JZ  P1 P2`: output parameter `P` to stdout

Notice that each *word* (instructions and parameters) occupies a position, the following program:
```
ADD 100 0 [100]
ADD -1 [100] [100]
JNZ [100] 4
```
Will jump to the instruction `ADD -1 [100] [100]` until `[100] = 0`.

#### Other

`RBS P`: sets relative base (used by relative parameters).
`HLT`: stops the program
