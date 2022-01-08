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
4. Dealloc `<<dealloc>> ::= "@dealloc" ID`. Note that `ID` must have pointer or enum type.

### Assigment
Assigment expressions can be:
1. Binary operation `ID <<bop>> ID`.
2. Discriminant `"D" "[" <<dval>> "]"`.
    - discriminant of value, e.g. `D[_0]`.
    - discriminant of data variant, e.g. `D[Data.Var]`.
3. Field of value `"F" "[" <<variant>> "|" <<field>> "]" ID`.
4. Compressing values into a list `"@map" "[" <<ids>> "]"`.
5. Calling a function `"@call" ID <<ids>>` (function ID, then argument IDs).
6. Function name, e.g. `_0 = func`.
7. Allocating memory and putting variable into it `@alloc ID`.
8. Dereference `"*" ID`. Puts value from a heap to the stack, so cannot be applied to the stacked values.
9. Const value. Integer or unit value.
10. Cloning of variable `"@clone" ID`. Note that `ID` must have pointer type.

### Cloning semantic
Main difference within cloning and simply copying is that value cloned _recursively_. E.g.
if we clone a value `*Enum.Variant(Int, *Foo)` then and `*Foo` and `*Enum.Varian(..)` will be cloned.

Cloning is deeply - means that when we clone heaped value we clone both reference to the value and the value itself
(new memory for the cloned value is allocated too). If we clone RC value, then only reference counter is incremented.

### Types
There are simple types. Each variable, argument and return type must have type.
1. Enum type `"Enum" "[" <<ty>> "]"`. Can be either stacked or heap.
2. Discriminant output always is `DC`. Can be only stacked.
3. Field output can be any type.
4. Function type `"fn" "(" <<tys>> ")"`. Can be only heap (will change).

#### Type Location
There are 3 types of type location:
1. Stacked. Means that _value_ stored onto the stack.
2. Heaped. Means that _value_ stored onto the heap and there are only 4/8-byte pointer on the stack.
3. RC. Means that _value_ stored onto the heap with 4-byte counter and there are only 4/8-byte pointer on the stack.

### (De)allocation semantic
1. Only stacked values can be allocated (e.g. there are no possibility to create pointer-to-pointer type).
2. Only heaped and RC values can be deallocated.
3. If we deallocate RC value, then reference of the value is decremented. If reference count is =0, then memory will be deallocated.

## Analysis
Base on [ASAP paper](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-908.pdf).

MIR analysis checks whether heap