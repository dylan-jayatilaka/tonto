# Tonto documentation

Everything lives in this repository, versioned with the code it describes.

## Building

| | |
|---|---|
| [**Linux**](BUILDING_ON_LINUX.md) | Ubuntu/Debian, the best-supported platform |
| [**macOS**](BUILDING_ON_MACOS.md) | via Homebrew |
| [**Windows**](BUILDING_ON_WINDOWS.md) | via WSL2, and the four traps it adds |

Each page is self-contained: prerequisites, build, tests, other build types and
parallel (MPI) builds for that platform.

## Running the programs

| | |
|---|---|
| [**Running Tonto**](RUNNING_TONTO.md) | the main program, its options and input conventions |
| [**Running `hart`**](RUNNING_HART.md) | standalone Hirshfeld atom refinement |
| [**Running `rgbi`**](RUNNING_RGBI.md) | Roby-Gould bond indices and their pictures |
| [**Installing the RGBI picture tools**](INSTALLING_RGBI.md) | LaTeX, Open Babel, mol2chemfig |

## Learning

| | |
|---|---|
| [**Workshop**](../workshop/WORKSHOP.md) | three worked exercises: HAR, bond indices, XCW fitting |
| [**Workshop answers**](../workshop/WORKSHOP_ANSWERS.md) | answers to the questions in the workshop |
| [**`examples/`**](../examples) | the input files those exercises run |

## Developing

| | |
|---|---|
| [**Source and executable layout**](TONTO_LIBRARY_STRUCTURE.md) | what lives where, and the module structure |
| [**Developer reference**](TONTO_DEVELOPER.md) | including writing parallel (MPI) code in Foo |
| [**The Foo language**](FOO_GRAMMAR_DOCUMENTATION.md) | the language and its translation to Fortran |
| [**Foo compared with Fortran**](FOO_LANGUAGE_VS_FORTRAN.md) | for readers who know Fortran |
| [**Tonto and MPI**](TONTO_AND_MPI.md) | the parallel build, its numerics, and the defect register |
| [**Continuous integration**](TONTO_CONTINUOUS_INTEGRATION.md) | what each workflow runs, and what each badge means |
| [**Call graphs**](MAKING_CALL_GRAPHS.md) | call/use graphs and dead-code elimination |
| [**Editing with vim**](EDITING_TONTO_WITH_VIM.md) | tags, folding, completion |
