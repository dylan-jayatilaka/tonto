# Foo Language Quick Reference Guide

## Grammar Elements and Examples

### Module Structure

**Grammar Rule:**
```
module_declaration
    : MODULE_KEYWORD IDENTIFIER newline
      module_content*
      END_KEYWORD newline
    ;
```

**Example:**
```foo
module STR
   implicit none
   ! ... module content ...
end
```

---

## Variable Declarations

### Single Variable

**Grammar Rule:**
```
local_variable_declaration
    : variable_name DOUBLE_COLON type_declaration 
      (COMMA attribute)* (EQUAL initializer)? newline
    ;
```

**Examples:**
```foo
i :: INT
x :: REAL
flag :: BIN
s :: STR
```

### Multiple Variables (Same Type)

```foo
i, j, k :: INT
x, y, z :: REAL
```

### With Attributes

```foo
self :: IN
factor :: REAL, IN
result :: STR, OUT
data :: VEC{REAL}, ALLOCATABLE
ptr :: INT, POINTER
```

### With Initialization

```foo
letters :: STR(len=52) = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
opening :: VEC{STR}(len=1,6) = ["'",'"',"{","(","[","<"]
i :: INT = 0
```

---

## Type Declarations

### Primitive Types

```foo
i :: INT              ! Integer
x :: REAL             ! Double precision real
z :: CPX              ! Complex
flag :: BIN           ! Logical/Boolean
text :: STR           ! Character string
```

### Array Types (1D Vector)

```foo
v :: VEC{REAL}        ! 1D vector of reals
iv :: VEC{INT}        ! 1D vector of integers
sv :: VEC{STR}        ! 1D vector of strings
```

### Array Types (2D Matrix)

```foo
m :: MAT{REAL}        ! 2D matrix of reals
im :: MAT{INT}        ! 2D matrix of integers
```

### Higher Dimensional Arrays

```foo
t :: MAT3{REAL}       ! 3D tensor
q :: MAT4{CPX}        ! 4D tensor of complex
```

### Arrays with Dimensions

```foo
v :: VEC{REAL}(10)                    ! Vector of 10 reals
m :: MAT{REAL}(3,4)                   ! 3×4 matrix
s :: STR(len=256)                     ! String of length 256
arr :: VEC{STR}(len=1,6)             ! Vector of 6 strings of length 1
```

### Pointer Types

```foo
ptr :: INT*           ! Pointer to integer
pv :: VEC{REAL}*      ! Pointer to vector of reals
pm :: MAT{INT}*       ! Pointer to matrix of integers
```

### Allocatable Types

```foo
arr :: VEC{REAL}@     ! Allocatable vector
mat :: MAT{INT}@      ! Allocatable matrix
```

### Nested Generic Types

```foo
nested :: VEC{VEC{REAL}}              ! Vector of vectors
mat_of_matrices :: VEC{MAT{REAL}}     ! Vector of matrices
```

---

## Procedure Declarations

### Simple Subroutine

**Grammar Rule:**
```
procedure_declaration
    : procedure_signature newline
      procedure_body
      END_KEYWORD newline
    ;

procedure_signature
    : IDENTIFIER procedure_arguments? procedure_result?
      TRIPLE_COLON procedure_attributes* newline
    ;
```

**Example:**
```foo
multiply(factor) ::: pure
   self :: INOUT
   factor :: REAL, IN
   
   self = self * factor
   
end
```

### Function with Result

```foo
n_items result (res) ::: PURE
   self :: IN
   res :: INT
   
   res = count_items(self)
   
end
```

### With Multiple Procedures

```foo
equals(i) result (same) ::: pure
   self :: IN
   i :: INT, IN
   same :: BIN
   same = self == i
end

is_zero result (res) ::: pure
   self :: IN
   res :: BIN
   res = self == 0
end
```

### Procedure Attributes

**PURE** - No side effects:
```foo
double_value(x) result (y) ::: pure
   self :: IN
   x :: INT, IN
   y :: INT
   y = self * 2
end
```

**ELEMENTAL** - Can operate on arrays:
```foo
square result (res) ::: elemental
   self :: IN
   res :: INT
   res = self * self
end
```

PURE and ELEMENTAL are C macros which may be empty definitions if compiled under debug.
Otherwise they evaluate to `pure` and `elemental` which have the definitions in Fortran.

**Inherited Implementation** (get_from):
```foo
to_str result (string) ::: get_from(INTRINSIC, FMT=>*), pure
   ! Implementation inherited from INTRINSIC module
end
```

---

## Generic Interfaces

**Grammar Rule:**
```
interface_declaration
    : INTERFACE IDENTIFIER newline
      interface_body
      END_KEYWORD newline
    ;
```

**Example:**
```foo
interface to_str
   to_str_int_0
   to_str_int_1
   to_str_int_2
end

interface trim
   trim_blanks_from_end
end
```

---

## Statements and Expressions

### Assignment

```foo
i = 5
x = 3.14159d0
s = "hello"
same = self == i
```

### Arithmetic Expressions

```foo
res = ONE
res = res * self
res = ONE / res
si = abs(value)
```

### Method Calls

```foo
.get_next_item(item, f, l)
self(end+1:).get_next_item(...)
res = mod(self, 2)
sl = size(vector)
```

### Control Structures

**If Statement:**
```foo
if (condition) then
   res = 1
else if (other) then
   res = 2
else
   res = 0
end
```

**Case Statement:**
```foo
select case (self)
   case (0)
      res = 1.0d0
   case (1)
      res = 1.0d0
   case (2)
      res = 2.0d0
   case default
      res = 3628800.0d0
end
```

**Do Loop:**
```foo
do i = 1, abs(n)
   res = res * self
end
```

### Special Values

```foo
TRUE, FALSE           ! Boolean values
ZERO, ONE             ! Numeric constants
NULL                  ! Null pointer
```

---

## Attributes and Intents

### Parameter Intent

```foo
self :: IN           ! Input (read-only)
result :: OUT        ! Output (write-only)
value :: INOUT       ! Input and output
```

### Visibility

```foo
E_Fermi :: REAL, private     ! Private variable
data :: INT, readonly        ! Read-only component
```

### Storage

```foo
persistent :: INT, save      ! Static storage
ephemeral :: VEC{REAL}       ! Automatic storage
```

### Pointer/Allocatable

```foo
dynamic :: VEC{REAL}@        ! Allocatable
reference :: REAL*           ! Pointer
```

---

## Comments

```foo
! Single line comment
x :: INT  ! Comment at end of line

! Multi-line comment spans several
! lines in the source code
! Each line needs its own ! marker
```

---

## Common Patterns

### Variable Declaration Block

```foo
module MY_MODULE
   implicit none
   
   ! Module-level variables
   global_config :: INT, private = 0
   debug_mode :: BIN, private = FALSE
   
   interface public_operation
      do_operation_int
      do_operation_real
   end
   
contains
   
   ! Procedures follow
   
end
```

### Procedure with Local Variables

```foo
calculate(input_vector) result (output) ::: pure
   ! Parameters
   self :: IN
   input_vector :: VEC{REAL}, IN
   output :: REAL
   
   ! Local variables
   i, n :: INT
   sum_val :: REAL
   
   ! Implementation
   n = size(input_vector)
   sum_val = ZERO
   do i = 1, n
      sum_val = sum_val + input_vector(i)
   end
   
   output = sum_val / n
   
end
```

### Type with Parameters and Attributes

```foo
data :: VEC{REAL}(100)          ! Vector of 100 reals
text :: STR(len=1000)           ! String of length 1000
ptr_matrix :: MAT{INT}*         ! Pointer to matrix
dyn_array :: VEC{CPX}@          ! Allocatable vector
```

---

## Grammar to Fortran Mapping

| Foo Syntax | Fortran 90+ |
|-----------|-----------|
| `i :: INT` | `integer :: i` |
| `x :: REAL` | `real(8) :: x` |
| `flag :: BIN` | `logical :: flag` |
| `s :: STR` | `character(len=*) :: s` |
| `v :: VEC{REAL}` | `type(vector_real) :: v` |
| `ptr :: INT*` | `integer, pointer :: ptr` |
| `arr :: VEC{REAL}@` | `type(vector_real), allocatable :: arr` |
| `self :: IN` | `integer, intent(in) :: self` |
| `result :: OUT` | `real, intent(out) :: result` |
| `procedure(...) result (...) ::: PURE` | `pure function procedure(...) result (...)` |

---

## Notes

1. **Case Sensitivity**: Foo is case-insensitive for keywords and variables but preserves identifier case by convention
2. **Type Parameters**: All capitals are used, and they are expanded as C- macros in `includes/macros.in`.
                        Generic types use `{...}` syntax, arrays use `(...)` syntax.
3. **Modules and submodules**: In most cases a `.foo` file contains a single module or class.
4. **File naming convention**: The head part of the file name befpre the `.` is the
lower case form of the corresponding Type. Large modules may be split into submodules 
e.g. `MOLECULE.BASE` in file `molecule.base.foo` is a submodule which `contains`
methods pertaining to the type `MOLECULE`. 
5. **Intent Attributes**: IN, OUT, INOUT specify procedure parameter intents. 
6. **Semicolons**: Can be used as statement separators on single lines.
7. **Line continuation**: Indicated by the & character
8. **Comments**: Start with `!` and continue to end of line

---

## How to Use the ANTLR4 Grammar

```bash
# Generate parser for Python
antlr4 -Dlanguage=Python3 Foo.g4

# Generate parser for Java
antlr4 -Dlanguage=Java Foo.g4

# Generate parser for C++
antlr4 -Dlanguage=Cpp Foo.g4
```

Then in your code:
```python
from antlr4 import *
from FooLexer import FooLexer
from FooParser import FooParser

# Parse a foo file
input_stream = FileStream("myfile.foo")
lexer = FooLexer(input_stream)
stream = CommonTokenStream(lexer)
parser = FooParser(stream)
tree = parser.program()
```
