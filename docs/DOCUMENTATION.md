# Tonto documentation

Everything lives in this repository, versioned with the code it describes.

## Building

| | |
|---|---|
| [**Linux**](BUILDING_ON_LINUX.md) | Ubuntu/Debian, the best-supported platform |
| [**macOS**](BUILDING_ON_MACOS.md) | via Homebrew |
| [**Windows**](BUILDING_ON_WINDOWS.md) | via WSL2, and the four traps it adds |
| [**With MPI**](BUILDING_WITH_MPI.md) | a parallel build on any platform: the matching MPI, the build, the checks, running |

Each page is self-contained: prerequisites, build, tests, the build types and
parallel (MPI) builds for that platform.

## Running the programs

| | |
|---|---|
| [**Running Tonto**](RUNNING_TONTO.md) | the main program, its options and input conventions |
| [**Running `hart`**](RUNNING_HART.md) | standalone Hirshfeld atom refinement |
| [**Running `rgbi`**](RUNNING_RGBI.md) | Roby-Gould bond indices and their pictures |
| [**Installing the RGBI picture tools**](INSTALLING_RGBI.md) | LaTeX, Open Babel, mol2chemfig |
| [**Known issues and limits**](TONTO_KNOWN_ISSUES.md) | what Tonto does not do, or does wrongly, that you may meet in ordinary use |

## Methods

| | |
|---|---|
| [**The X-ray constrained SCF: convergence and the choice of lambda**](REPORT_ON_XCW_CONVERGENCE.md) | Why the constrained SCF blows up, the stiffness of the data per reflection, the effective number of parameters, AIC, BIC, GCV and leave-one-out from one run, leverage, the halting methods, and the cure |
| [**ADPs from rigid-body motion and internal modes**](REPORT_ON_MODE_FITTING.md) | TLS refinement against F, stiff-mode ADPs from a Hessian, and results on urea |
| [**Angular decomposition of electron density maps**](REPORT_ON_L_DECOMPOSITION_MAPS.md) | Local-moment maps from structure factors, the expansion about a centre, Hirshfeld atoms by angular character and their populations; keywords, code, checks, urea and gly-L-ala |
| [**A sum of Gaussians for 1/r: where it wins**](REPORT_ON_GAUSSIAN_1_ON_R_FIT.md) | What a Gaussian expansion of the Coulomb kernel buys and costs: analytic integrals, grids, tensor hypercontraction, COSX, periodic systems |

## Learning

| | |
|---|---|
| [**Workshop**](../workshop/WORKSHOP.md) | four worked exercises: HAR, bond indices, XCW fitting, deformation density |
| [**Workshop answers**](../workshop/WORKSHOP_ANSWERS.md) | answers to the questions in the workshop |
| [**`workshop/examples/`**](../workshop/examples) | the input files those exercises run |
| [**`workshop/`**](../workshop) | the workshop, its answers, and the slides and notes as PDFs |

## Developing

| | |
|---|---|
| [**Source and executable layout**](TONTO_LIBRARY_STRUCTURE.md) | what lives where, and the module structure |
| [**The Foo language**](FOO_GRAMMAR_DOCUMENTATION.md) | the language and its translation to Fortran |
| [**Foo compared with Fortran**](FOO_LANGUAGE_VS_FORTRAN.md) | for readers who know Fortran |
| [**Continuous integration**](TONTO_CONTINUOUS_INTEGRATION.md) | what each workflow runs, and what each badge means |
| [**Running the tests, and blessing**](TONTO_BLESSING_TESTS.md) | the loose criterion, why two machines differ in the last bits, and how to bless by hand |
| [**Call graphs**](TONTO_CALL_GRAPHS.md) | call/use graphs and dead-code elimination |
| [**Repository branches**](TONTO_REPOSITORY_BRANCHES.md) | what is live, what was archived, and how to recover it |
| [**Editing with vim**](TONTO_EDITING_WITH_VIM.md) | tags, folding, completion |

## Work in progress

These cover items still in flight. They are long on purpose — what was measured, what was
ruled out, what was decided — and **each is deleted when its item closes**, its durable
residue moving into the pages above.

| | |
|---|---|
| [**Tasks and history**](../TASKS_AND_HISTORY.md) | the live work: the handover, then every open issue by theme |
| [**Developer reference**](TONTO_DEVELOPER_INFO.md) | writing parallel (MPI) code in Foo, and build and test traps |
| [**Deblurring density maps with a benchmarked prior**](TASK_DEBLURRING_WITH_A_PRIOR.md) | a science idea filed for later: the arguments, the relation to density modification, the protein case, and the first test |
| [**Tonto and MPI**](TASK_MPI.md) | the parallel build's numerics and its defect register |
| [**Dispersion corrections**](TASK_DISPERSION_CORRECTIONS.md) | anomalous dispersion, Bijvoet pairs, and the residual density map |
| [**DFT standardisation**](TASK_DFT_STANDARDISATION.md) | the DFT machinery, its silent defects, and the libxc plan |
| [**Extinction correction**](TASK_EXTINCTION_CORRECTION.md) | why it has been dormant, its defect register, and the plan to bring it back |
| [**GoF², not chi2**](TASK_GOF2_NOT_CHI2.md) | the misnamed goodness of fit, and reporting GoF in place of its square |
| [**Nearest-neighbour HAR**](TASK_NN_HAR_REACTIVATION.md) | HAR on a covalent network solid, and where the sus go wrong |
| [**Bader basin analysis**](TASK_BADER_BASIN_PORT.md) | the `archive/Bader` port, and the two defects found by running it |
| [**MP2 teaching lab**](TEACHING_MP2.md) | the two non-default MP2 programs, and how they were validated |
| [**cctbx into Tonto**](TASK_CCTBX_INTO_TONTO.md) | the refinement capabilities Tonto lacks, and the plan to write them in Foo |
| [**gfortran-16 debug builds**](TASK_GFORTRAN16_PORT.md) | a compiler bug, and the build workaround |
| [**Project history**](PROJECT_HISTORY.md) | why the ANTLR4 translator exists, and what the milestones found |
