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
| [**Workshop**](../workshop/WORKSHOP.md) | four worked exercises: HAR, bond indices, XCW fitting, deformation density |
| [**Workshop answers**](../workshop/WORKSHOP_ANSWERS.md) | answers to the questions in the workshop |
| [**`examples/`**](../examples) | the input files those exercises run |
| [**`workshop/`**](../workshop) | the workshop, its answers, and the slides and notes as PDFs |

## Developing

| | |
|---|---|
| [**Source and executable layout**](TONTO_LIBRARY_STRUCTURE.md) | what lives where, and the module structure |
| [**Developer reference**](TONTO_DEVELOPER_INFO.md) | including writing parallel (MPI) code in Foo |
| [**The Foo language**](FOO_GRAMMAR_DOCUMENTATION.md) | the language and its translation to Fortran |
| [**Foo compared with Fortran**](FOO_LANGUAGE_VS_FORTRAN.md) | for readers who know Fortran |
| [**Tonto and MPI**](TONTO_AND_MPI.md) | the parallel build, its numerics, and the defect register |
| [**DFT standardisation**](DFT_STANDARDISATION.md) | the DFT machinery, its silent defects, and the libxc plan |
| [**Nearest-neighbour HAR**](NN_HAR_REPORT.md) | HAR on a covalent network solid, the quartz reproduction, and where the sus go wrong |
| [**Bader basin analysis**](BADER_REPORT.md) | the `archive/Bader` port: what landed, and the two defects found by running it |
| [**MP2 teaching lab**](TEACHING_MP2.md) | the two non-default MP2 programs, and how they were validated |
| [**cctbx into Tonto**](CCTBX_INTO_TONTO.md) | the refinement capabilities Tonto lacks, and the staged plan to write them in Foo |
| [**Extinction correction**](EXTINCTION_REPORT.md) | why it has been dormant since 2016, its defect register, and the plan to bring it back |
| [**GoF², not chi2**](GOF_NOT_CHI2.md) | the misnamed goodness of fit, and reporting GoF in place of its square |
| [**gfortran-16 debug builds**](GFORTRAN16_DEBUG_CRASH.md) | a compiler bug, and the build workaround |
| [**Continuous integration**](TONTO_CONTINUOUS_INTEGRATION.md) | what each workflow runs, and what each badge means |
| [**Call graphs**](TONTO_CALL_GRAPHS.md) | call/use graphs and dead-code elimination |
| [**Repository branches**](TONTO_REPOSITORY_BRANCHES.md) | what is live, what was archived, how to recover it, and why archived work must be ported |
| [**Editing with vim**](TONTO_EDITING_WITH_VIM.md) | tags, folding, completion |
