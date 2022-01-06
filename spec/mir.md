## Semantic
MIR is:
1. Semi-typed.
2. Have difference between stack and heap variables.
3. Does not have mutations.

TODO: partially application function.

### Statements
Possible statements is:
1. Variable definition `<<vardef>> ::= "_" ID "=" <<ass>>`.
2. Case expression `<<case>> ::= "case" ID ID "{" <<stmts>> "}"`.
3. Return expression `<<ret>> ::= "ret" ID`.

### Assigment
Assigment expressions can be:
1. Binary operation `ID <<bop>> ID`.
2. Discriminant `"D" "[" <<dval>> "]"`.
    - discriminant of value, e.g. `D[_0]`.
    - discriminant of data variant, e.g. `D[Data.Var]`.
3. Field of value `"F" "[" <<variant>> "|" <<field>> "]" ID`.
4. Compressing values into a list `"@map" "[" <<ids>> "]"`.
5. Calling a function `"@call" ID ID` (function ID, then argument ID).
6. Function name, e.g. `_0 = func`.
7. Allocating memory and putting variable into it `@alloc ID`.

### Types
There are simple types. Each variable, argument and return type must have type.
1. Enum type `"Enum" "[" <<ty>> "]"`. Can be either stacked or heap.
2. Discriminant output always is `DC`. Can be only stacked.
3. Field output can be any type.
4. Pointer type `"*" <<ty>>`. It shows that memory placed in the heap.
5. Function type `"fn" "(" <<tys>> ")"`. Can be only heap (will change).