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

   ! Procedure definitions follow
   ! Functions always used the result (res) syntax, subroutines do not.
   
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

#### Parameterized types

- MAP{KEY,VAL} is a type MAP parameterized by types KEY and VAL.


**Examples:**
```foo
v :: VEC{REAL}
matrix :: MAT{INT}
tensor :: MAT3{CPX}
nested :: VEC{VEC{REAL}}
```

#### Derived Types (Parameterized)
These are defined in the `types.foo` file as combinations of primitive
or other derived types. An example is below:
```
   type ATOM

     start_time5 :: VEC{INT}(5), readonly  DEFAULT(0)
     ! Contains real start time, in Julian day,h,m,s,ms

     stop_time5 :: VEC{INT}(5), readonly  DEFAULT(0)
     ! Contains real stop time, in Julian day,h,m,s,ms

     cpu_start_time :: REAL, readonly  DEFAULT(ZERO)
     ! Contains CPU start time, in seconds

     cpu_stop_time :: REAL, readonly  DEFAULT(ZERO)
     ! Contains CPU stop time, in seconds

   end
```
The `readonly` attribute specifies that it is illegal to directly change this field outside the defining module.

The DEFAULT macro specifies a DEFAULT(X) value for the type component, which evaluates to `= X` in Fortran
as defined in the `include/macros.in` C preprocessor file.

The `private` attribute (not shown) means that the type compnent may not even be used by dot notation outside the dining module.

#### Array of array types
Arrays of arrays are, in fact, arrays of derivaewd types which contain arrays. Thus an EVEC{REAL} is

```
type EVEC{REAL}

     element :: VEC{REAL}@
     ! Encapsulated vec type

end
```

And a VEC{EVEC{REAL}} is an Array of EVEC{REAL} derived types.

They are defined in the `types.foo` file only to indicate that they will be used in the library later on to help the translator; such declarations produce no Fortran code.

**Examples:**
```
val :: REAL
i,j :: INT
nested :: VEC{VEC{REAL}}@

nested.create(3,4)
val = nested(i)%element(j)
val = nested(i)[j]
nested.destroy
```
Note that the second assignment to `val` is equiovalent to the first and defines the square-bracket simplification to avoid the use of the `element` array component. This scheme extend to multidimensional arrays. The create method called by dot notation is used to allocate the object, while destroy is used to deallocate. These are standard names, by convention.

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

Note that the `self` variable of the same type as the module is an implicit first argument to the function. It's intent must be IN for functions.

#### Subroutine Declaration

```foo
multiply(factor) ::: pure
   ! Multiply self by factor
   self :: INOUT
   factor :: REAL, IN
   
   ! ... implementation
   
end
```

Note that the `self` variable of the same type as the module is an implicit first argument to the function. It's intent should be declared.

#### Procedure arguments to procedures
If the argument of a procedure is itself procedure, it's calling interface amust be specified.
An example is shown below.
```
   line_search(dself,alphamax,x,p,c1,c2,b) ::: routinal, public
   ! Given a real vector, x, function f, gradient function
   ! df, calculaes the ideal stepping scale alpha, given
   ! stepping p and constants c1, c2.
   ! Interface for vector functions   
      interface
         self(x,res)
            x :: VEC{REAL}, IN
            res :: REAL, OUT
         end
      end
      interface
         dself(x,res)
            x :: VEC{REAL}, IN
            res :: VEC{REAL}, OUT
         end
      end
      x :: VEC{REAL}, IN
      p :: VEC{REAL}, IN
      c1,c2,alphamax :: REAL, IN
      b :: REAL, OUT
```
Here the procedure has the `routinal` attribute which means the `self` argumant is a function, not a variable.
The procedure takes another function argument `dself` whose explicit interface is also declared.
Noe the three character indentation and the `end` keyword to terminate the interface scope.

#### Procedure Attributes

Attributes are specified after `:::`:
- `PURE` - Pure function (no side effects)
- `ELEMENTAL` - Can operate on arrays element-wise
- `get_from(MODULE)` - Inherit from another module
- `selfless` - the procedure lacks an implicit `self` argument
- `functional` or `routinal` - the procedure takea a function argument as the first argument rather than a `self` variable.

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

The declaration of the ptr and array may be simplified:

```foo
ptr :: INT*   
arr :: VEC{REAL}@
```
The use of pointers is very rare.

#### Common Attributes:
- `IN` - Input (intent in)
- `OUT` - Output (intent out)
- `INOUT` - Input/Output
- `PRIVATE` - Private visibility
- `READONLY` - Read-only component
- `POINTER` - Pointer declaration. Prefer to use * as abbreviation.
- `TARGET` - Can be target of pointer
- `SAVE` - Static/saved variable
- `ALLOCATABLE` - Dynamically allocated. Prefer to use @ as abbreviation.
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

Note the preferred indentation style.

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

Note that `.get_next_item(item, f, l)` is equivalent to `self.get_next_item(item, f, l)`.

Explicit procedure calls are also allowed and equivalent e.g. `STR:get_next_item(self,item, f, l)`.
Note that the `self` argument appears explicitly. A single colon indicates that a generic funtion call is used
i.e. there may be other calls with the same name but with different arguments.

Explicit non-generic function calls are also permitted e.g. `STR::get_next_item(self,item, f, l)`.
In this case the name `get_next_item` must not be overloaded. Non-generic funtion calls may
ionly appear in the module they are defined in.

Explicit calls within the module they are defined may be simplified to 
`:get_next_item(self,item, f, l)` or `::get_next_item(self,item, f, l)` for
generic and non-generic calls, respectively.

Dot notation foer procedures in submodules must be modified to specify the particular submodule in which
the proceudre appears e.g. in the examp[le below from file `diffraction_data.inq.foo` defining the
corresponding submodule DIFFRACTION_DATA.INQ
```
!      ! Clean up leak here
!      .SET:delete_atom_SCF_archives
```
the call .SET:delete_atom_SCF_archives refers to method `delete_atom_SCF_archives` in file `diffraction_data.set.foo`
defining `DIFFRACTION_DATA.SET`. This is a generic call. As before a non-generic call is with two colons.

Generic dot methods calls to a submodule procedure of a given type are written `.MAIN:setup(basis_library_dir)` or, if within
the same submodule, `.:setup(basis_library_dir)`. Non-generic calls use the double colon.

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

Here FMT is a macro partameter whose value is substituted when the code is inherited from file `intrinsic.foo`.

It is preferred that the macro partameter should be defined as FMT? rather than FMT.

Inheritance is simply text inclusion with macro substitution. It is not recursive.

### 3. String Handling
Special handling for strings with arbitrary length:

```foo
! Strings can be parameters
s :: STR
s(end+1:).get_next_item(...)  ! Substring operations
```

`get_next_item` is a method defined in the file `str.foo` which defined module `STR`.

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


