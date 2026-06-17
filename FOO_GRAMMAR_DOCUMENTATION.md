# Foo Language ANTLR4 Grammar Documentation

## Overview

This document describes the **Foo language**, a custom preprocessor language that compiles to Fortran 90+. The grammar has been extracted by analyzing:
- The Perl converter script (`scripts/foo.pl`) 
- Sample foo language files from `foofiles/`
- Corresponding generated Fortran code in `release/`
- Type macro definitions in `include/macros.in`

## Key Language Features

### 1. Reverse Variable Declaration

The defining feature of Foo is **reverse declaration syntax** compared to standard Fortran:

**Foo syntax:**
```foo
varname :: TYPE
```

**Standard Fortran:**
```fortran
TYPE :: varname
```

**Examples:**
```foo
i :: INT
x :: REAL
s :: STR
matrix :: MAT{REAL}
```

### 2. Module Declaration

Modules are defined with the `module` keyword and contain procedures and global variables.

```foo
module STR
   implicit none
   
   ! Global variables
   opening :: VEC{STR}(len=1,6) = [...]
   
   ! Procedures and interfaces
   interface trim
      trim_blanks_from_end
   end
   
   contains
   ! procedure definitions follow
   
end
```

### 3. Type System

#### Primitive Types
- `INT` - Integer
- `REAL` - Double precision real
- `CPX` - Complex numbers
- `BIN` - Logical/Boolean
- `STR` - Character strings

#### Array Types (Parameterized)
Array types use curly braces for type parameters:

- `VEC{T}` - 1D vector of type T
- `MAT{T}` - 2D matrix of type T
- `MAT3{T}` - 3D tensor
- `MAT4{T}` - 4D tensor
- `MAT5{T}` - 5D tensor
- `MAT6{T}`, `MAT7{T}` - Higher dimensional tensors

**Examples:**
```foo
v :: VEC{REAL}
matrix :: MAT{INT}
tensor :: MAT3{CPX}
nested :: VEC{VEC{REAL}}
```

#### Type Parameters

Array types can have parameters:

```foo
s :: STR(len=256)           ! String of length 256
v :: VEC{STR}(len=1,6)     ! Vector of 6 strings, each length 1
m :: MAT{REAL}(3,4)        ! 3x4 real matrix
```

#### Pointer and Allocatable Types

```foo
ptr :: INT*                 ! Pointer to integer
arr :: VEC{REAL}@          ! Allocatable vector
```

### 4. Procedure Declarations

Procedures (functions and subroutines) start with the procedure name, followed by optional arguments and result specification, with attributes after `:::`.

#### Function Declaration

```foo
n_items result (res) ::: PURE
   ! Return the number of items in the string
   self :: IN
   res :: INT
   
   ! ... implementation
   
end
```

#### Subroutine Declaration

```foo
multiply(factor) ::: pure
   ! Multiply self by factor
   self :: INOUT
   factor :: REAL, IN
   
   ! ... implementation
   
end
```

#### Procedure Attributes

Attributes are specified after `:::`:
- `PURE` - Pure function (no side effects)
- `ELEMENTAL` - Can operate on arrays element-wise
- `get_from(MODULE)` - Inherit from another module

### 5. Variable Attributes

Variables can have multiple attributes after the type:

```foo
x :: INT, IN              ! Input parameter
y :: REAL, OUT            ! Output parameter
z :: STR, INOUT           ! Input/Output parameter
ptr :: INT, POINTER       ! Pointer attribute
arr :: VEC{REAL}, ALLOCATABLE
flag :: BIN, private      ! Private component
```

#### Common Attributes:
- `IN` - Input (intent in)
- `OUT` - Output (intent out)
- `INOUT` - Input/Output
- `PRIVATE` - Private visibility
- `READONLY` - Read-only component
- `POINTER` - Pointer declaration
- `TARGET` - Can be target of pointer
- `SAVE` - Static/saved variable
- `ALLOCATABLE` - Dynamically allocated
- `OPTIONAL` - Optional argument

### 6. Generic Interfaces

Generic interfaces are declared with the `interface` keyword:

```foo
interface trim
   trim_blanks_from_end
end

interface scan
   index_of_character_in
end
```

### 7. Initialization

Variables can be initialized at declaration:

```foo
letters :: STR(len=52) = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
opening :: VEC{STR}(len=1,6) = ["'",'"',"{","(","[","<"]
coefficients :: VEC{REAL}(0:6) = [1.0d0, 76.18d0, -86.50d0, ...]
```

### 8. Control Structures

Standard Fortran-like control structures:

#### If Statement
```foo
if (condition) then
   ! statements
else if (other_condition) then
   ! statements
else
   ! statements
end
```

#### Select Case
```foo
select case (variable)
   case (value1)
      ! statements
   case (value2)
      ! statements
   case default
      ! statements
end
```

#### Do Loop
```foo
do i = 1, n
   ! statements
end

do  ! infinite loop
   ! statements
   if (condition) exit
end
```

### 9. Comments

Comments start with `!` and continue to end of line:

```foo
! This is a comment
self :: IN  ! Comment after code
```

### 10. Method Calls and Expressions

Expressions follow Fortran rules with standard operators:

```foo
! Assignment
res = self == i

! Method calls
.get_next_item(item, f, l)
self(end+1:).get_next_item(...)

! Arithmetic
res = ONE
res = res * self
res = ONE / res

! Logical
same = self == i
res = mod(self, 2) == 0
```

#### Special Constants
- `TRUE`, `FALSE` - Boolean values
- `ZERO`, `ONE` - Numeric constants
- `NULL` - Null pointer

## Grammar Structure

The ANTLR4 grammar (`Foo.g4`) is organized into sections:

### Program Structure
- `program` - Top-level program node
- `module_declaration` - Module definition
- `module_content` - Module body items

### Declarations
- `global_variable_declaration` - Module-level variables
- `local_variable_declaration` - Procedure-level variables
- `procedure_declaration` - Function/subroutine definitions

### Procedures
- `procedure_signature` - Procedure header
- `procedure_arguments` - Parameter list
- `procedure_result` - Function result specification
- `procedure_body` - Procedure implementation

### Types and Attributes
- `type_declaration` - Type specification
- `primitive_type` - Basic types (INT, REAL, etc.)
- `parameterized_type` - Generic types with parameters (VEC{T})
- `array_type` - Array type declarations
- `attribute` - Variable attributes

### Expressions
- `expression` - Full expression grammar
- Operators: arithmetic, relational, logical
- Method calls
- Array constructors

### Control Flow
- `if_statement` - Conditional
- `select_case_statement` - Case selection
- `do_loop_statement` - Looping
- `exit_statement`, `cycle_statement` - Loop control

## Conversion to Fortran 90+

The Perl script `scripts/foo.pl` performs the following transformations:

### 1. Reverse Declaration Conversion
```
foo:       varname :: TYPE
fortran:   TYPE :: varname
```

### 2. Module Naming
```
foo file: str.foo
fortran module: STR_MODULE
```

### 3. Procedure Transformation
```
foo:       n_items result (res) ::: PURE
fortran:   PURE function n_items(self) result (res)
```

### 4. Type Parameterization
```
foo:       VEC{STR}(len=1,6)
fortran:   VEC(STR(len=1),6)
```

### 5. Macro Expansion
Type declarations use C-style macros:
```c
#define INT integer(INT_KIND)
#define REAL real(REAL_KIND)
#define VEC(T,N) type(vector_T_N)
```

## Multi-Pass Processing

The Perl script processes foo files in **two passes**:

### Pass 1: Analysis
- Extract procedure signatures and interfaces
- Build symbol tables
- Analyze type information
- Determine generic routine overloads

### Pass 2: Code Generation
- Generate Fortran 90+ code
- Expand macros
- Generate `.int` (interface) files
- Generate `.use` (usage) files

## Output Files

For each `module.foo` input file, the compiler generates:
- `module.F90` - Main Fortran source
- `module.int` - Fortran interface definitions
- `module.use` - Fortran usage interface blocks

## Notable Features

### 1. Generic Procedures
Multiple procedures with the same name can be grouped under a generic interface:

```foo
interface to_str
   to_str_int_0
   to_str_int_1
   to_str_int_2
end
```

### 2. Attribute Inheritance
The `get_from` directive allows inheriting procedure implementations:

```foo
to_str result (string) ::: get_from(INTRINSIC, FMT=>*), pure
! Implementation inherited from INTRINSIC module
end
```

### 3. String Handling
Special handling for strings with arbitrary length:

```foo
! Strings can be parameters
s :: STR
s(end+1:).get_next_item(...)  ! Substring operations
```

### 4. Pointer and Allocatable Arrays
```foo
data :: VEC{REAL}*           ! Pointer to vector
matrix :: MAT{REAL}@         ! Allocatable matrix
```

## Known Limitations

Based on the grammar extraction, the following features are currently captured:

1. ✅ Basic variable declarations
2. ✅ Module structure
3. ✅ Parameterized types
4. ✅ Procedure declarations with results
5. ✅ Attributes and visibility
6. ✅ Control structures
7. ✅ Expressions
8. ✅ Comments

Potential areas for refinement:
- Complex nested type parameters
- Advanced `get_from` directive parameters
- Special case handling for function result types
- Optional parameters in type declarations
- Advanced macro expansion contexts

## Grammar Usage

The ANTLR4 grammar can be compiled with:

```bash
antlr4 -Dlanguage=Python3 Foo.g4
# or
antlr4 -Dlanguage=Java Foo.g4
# or
antlr4 -Dlanguage=Cpp Foo.g4
```

This generates lexer and parser classes that can be used to:
- Parse foo language files
- Build abstract syntax trees (ASTs)
- Implement analysis and transformation tools
- Create IDE support (syntax highlighting, code completion)
- Develop new code generators

## References

- Source Perl script: `scripts/foo.pl`
- Sample modules: `foofiles/` directory
- Generated Fortran: `release/` directory
- Type definitions: `include/macros.in`

## Grammar Validation

The grammar was validated against actual foo language files including:
- `str.foo`, `bin.foo`, `int.foo`, `real.foo` - Basic type modules
- `atom.foo`, `basis.foo`, `molecule.*.foo` - Complex domain modules
- Type system with nested generics: `vec{emat{real}}`, `mat{evec{int}}`

The grammar captures the essential syntax of the language and should parse most valid foo files correctly.
