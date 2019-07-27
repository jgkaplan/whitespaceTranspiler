# White Space Compiler

WSC is a transpiler from a readable programming language to the esoteric language [Whitespace](https://esolangs.org/wiki/Whitespace). Built with OCaml.

NOTE: This does not run whitespace programs; it generates them.

## Installation

Install [OCaml](http://ocaml.org/) and opam.
Install Menhir with
```bash
opam install menhir
```

Build the project with make.
```bash
cd <installDirectory>
make
```

## Usage

```bash
./main.byte <inputFile.wsr>
```
This will output a whitespace file with a `.ws` extension.

## Readable Whitespace Language Spec
A .wsr file must contain a `main` function that has no parameters. This is the entry point and where code execution will start.

### Functions
#### Define a function
```
function fname(arg1, arg2){
    <statements>
}
```

### Statements
A statement is one of the following:

#### If
```
if(<expression>){
    <statements>
}
```
where expression evaluates to a boolean
#### If-Else
```
if(<expression>){
    <statements>
}else{
    <statements>
}
```
where expression evaluates to a boolean
#### Expression with semicolon
```
<expression>;
```
#### Break
```
break;
```
#### Loop
```
loop <expression> {
    <statements>
}
```
where expression evaluates to a positive integer

#### While
```
while(<expression>) {
    <statements>
}
```
where expression evaluates to a boolean

#### Do-While
```
do {
    <statements>
} while(<expression>);
```
where expression evaluates to a boolean

#### Print integer
```
print_int <expression>;
```
#### Print character
```
print_char <expression>;
```
#### Read integer
```
read_int <variable>;
```
Reads in an integer and saves it to the variable <variable>.
#### Read character
```
read_char <variable>;
```
Reads in a character and saves it to the variable <variable>.
#### Return
```
return <expression>;
```

### Expressions
An expression is one of the following.

#### Integer literal
Such as `42` or `0x3B` or `0o123` or `0b101101`

#### Character literal
Such as `'p'` or `' '` or `'\n'`

#### Boolean literal
`true` or `false`

#### Variable
Such as `x` or `foo` or `potato`

#### Assignment
```
<variable> := <expression>
```
Example: `x := 2 * 2`

#### Arithmetic
```
<expression> + <expression>
<expression> - <expression>
<expression> * <expression>
<expression> / <expression>
<expression> % <expression>
-<expression>
```
Where all expressions evaluate to integers.

#### Parentheses
```
(<expression>)
```

#### Boolean Logic (AND, OR, XOR)
```
<expression> && <expression>
<expression> || <expression>
<expression> ^ <expression>
! <expression>
```
Where all expressions evaluate to booleans.

#### Comparisons
```
<expression> < <expression>
<expression> <= <expression>
<expression> > <expression>
<expression> >= <expression>
<expression> == <expression>
<expression> != <expression>
```

#### Function application
```
<functionName>(<expression>, <expression>, ...)
```

Example: `double(1)` or `foo()` or `factorial(5 + 5)`

## Example Program
```
function main(){
    print_int factorial(5);
}

function factorial(n){
    if(n == 0){
        return 1;
    }
    return n * factorial(n-1);
}
```
More examples can be seen in the `test` folder.

## TODO and limitations
- Boolean short circuiting (and check that booleans work as intended)
- Builtin functions
- Including other files (preprocessor)
- Type checker
- Strings
- Arrays
- Structs / custom objects (to allow for fractions)
- Currently, no optimizations are done by the compiler.

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to test thoroughly.

## License
[MIT](https://choosealicense.com/licenses/mit/)
