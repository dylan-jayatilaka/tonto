!=================================================================
!
! TYPES : used to deposit all derived types used in TONTO.
!
! A separate module is required so that two modules can "see"
! each others types, even if they can't use each others procedures
! via a "use" statement. In C++, the types are "friendly".
!
! Note that in Foo, the type attribute "readonly" can be added
! so that other routines can "see" but not directly modify the
! the readonly component of the type, except via a "set" routine
! to the module that type "belongs" to.
!
! Please follow the indenting style here to allow the Foo version 
! of vim, stored in .vim/foo.vim to  its work properly. See also
! the .vimrc file, and the .ctags file, for use in jumping
! to pieces of code via the "exuberant ctags" program.
!
! (c) dylan jayatilaka, university of western australia, 1998-2xxx
!=================================================================
module TYPES_MODULE

#  include "macros"

   include "types.use"

   ! External modules

#ifdef MPI
!  Note the capital USE, below which prevents the
!  preprocessor from making the module a dependent
   USE mpi, only: MPI_STATUS_SIZE
#endif

   implicit none

   include "types.int"


!  ========================
!  Encapsulated Array types
!  ========================
   type EVEC_BIN_TYPE
     VEC(BIN,:), allocatable :: element
     ! Encapsulated type
   end type
   type EVEC_INT_TYPE
     VEC(INT,:), allocatable :: element
     ! Encapsulated type
   end type
   type EVEC_STR_TYPE
     VEC(STR(len=STR_SIZE),:), allocatable :: element
     ! Encapsulated type
   end type
   type EVEC_REAL_TYPE
     VEC(REAL,:), allocatable :: element
     ! Encapsulated vec type
   end type
   type EVEC_EVEC_INT_TYPE
     VEC(type(EVEC_INT_TYPE),:), allocatable :: element
     ! Encapsulated encapsulated vector
   end type
   type EVEC_EVEC_REAL_TYPE
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: element
     ! Encapsulated encapsulated vector
   end type
   type EMAT_BIN_TYPE
     MAT(BIN,:,:), allocatable :: element
     ! Encapsulated mat type
   end type
   type EMAT_INT_TYPE
     MAT(INT,:,:), allocatable :: element
     ! Encapsulated mat type
   end type
   type EMAT_REAL_TYPE
     MAT(REAL,:,:), allocatable :: element
     ! Encapsulated mat type
   end type
   type EMAT3_REAL_TYPE
     MAT3(REAL,:,:,:), allocatable :: element
     ! Encapsulated mat3 type
   end type
   type EMAT4_REAL_TYPE
     MAT4(REAL,:,:,:,:), allocatable :: element
     ! Encapsulated mat4 type
   end type

!  =========
!  Operators
!  =========
   type OPVECTOR_TYPE
     INT :: n_bf
     ! No of spatial basis functions (i.e. AO's)
     VEC(REAL,:), allocatable :: r
     ! Restricted spinorbital
     VEC(REAL,:), allocatable :: a
     ! Unrestricted alpha spinorbital
     VEC(REAL,:), allocatable :: b
     ! Unrestricted beta spinorbital
     VEC(REAL,:), allocatable :: g
     ! General spinorbital; alpha part first, then beta part.
   end type
   type OPMATRIX_TYPE
     INT :: n_bf DEFAULT(0)
     ! No of real spatial basis functions (i.e. AO's)
     INT :: n_mo DEFAULT(0)
     ! No. of molecular orbitals - used for ELMO/ALO.
     MAT(REAL,:,:), allocatable :: r
     ! Restricted spinorbital matrix
     MAT(REAL,:,:), allocatable :: a
     ! Alpha spinorbital matrix 
     MAT(REAL,:,:), allocatable :: b
     ! Beta spinorbital matrix
     MAT(CPX,:,:), allocatable :: gc
     ! General complex alpha-beta matrix
     VEC(REAL,:), allocatable :: tri
     ! Space saving for symmetric matrices: lower triangle only
     VEC(REAL,:), allocatable :: sqr
     ! Space saving for hermitian matrices: lower triangle is real
     ! part, upper is imaginary
   end type

!  ==================
!  Associative arrays
!  ==================
   type MAP_INT_INT_TYPE
     INT :: n_keys DEFAULT(0)
     ! The number of keys
     INT :: n_size DEFAULT(0)
     ! The size of the keys and values arrays.
     BIN :: reverse_search DEFAULT(FALSE)
     ! If TRUE, the search for values is performed from the largest
     ! key to the smallest.
     VEC(INT,:), allocatable :: keys
     ! The array of keys.
     VEC(INT,:), allocatable :: values
     ! The array of values.
   end type
   type MAP_INT_STR_TYPE
     INT :: n_keys DEFAULT(0)
     ! The number of keys
     INT :: n_size DEFAULT(0)
     ! The size of the keys and values arrays.
     BIN :: reverse_search DEFAULT(FALSE)
     ! If TRUE, the search for values is performed from the largest
     ! key to the smallest.
     VEC(INT,:), allocatable :: keys
     ! The array of keys.
     VEC(STR(len=STR_SIZE),:), allocatable :: values
     ! The array of values.
   end type
   type MAP_STR_INT_TYPE
     INT :: n_keys DEFAULT(0)
     ! The number of keys
     INT :: n_size DEFAULT(0)
     ! The size of the keys and values arrays.
     BIN :: reverse_search DEFAULT(FALSE)
     ! If TRUE, the search for values is performed from the largest
     ! key to the smallest.
     VEC(STR(len=STR_SIZE),:), allocatable :: keys
     ! The array of keys.
     VEC(INT,:), allocatable :: values
     ! The array of values.
   end type
   type MAP_VEC_INT_VEC_INT_TYPE
     INT :: n_keys DEFAULT(0)
     ! The number of keys
     INT :: n_size DEFAULT(0)
     ! The size of the keys and values arrays.
     BIN :: reverse_search DEFAULT(FALSE)
     ! If TRUE, the search for values is performed from the largest
     ! key to the smallest.
     MAT(INT,:,:), allocatable :: keys
     ! The array of keys, stored contigously by columns.
     MAT(INT,:,:), allocatable :: values
     ! The array of values.
   end type

!  =====================
!  System-function types
!  =====================
   type TIME_TYPE
     VEC(INT,5) :: start_time5 DEFAULT(0)
     ! Contains real start time, in Julian day,h,m,s,ms
     VEC(INT,5) :: stop_time5 DEFAULT(0)
     ! Contains real stop time, in Julian day,h,m,s,ms
     REAL :: cpu_start_time DEFAULT(ZERO)
     ! Contains CPU start time, in seconds
     REAL :: cpu_stop_time DEFAULT(ZERO)
     ! Contains CPU stop time, in seconds
   end type
   type PARALLEL_TYPE
     BIN :: is_parallel DEFAULT(FALSE)
     ! Whether we are doing a parallel job, or not.
     STR(len=STR_SIZE) :: parallel_do_lock DEFAULT(" ")
     ! This is set to the name of the routine which calls the parallel
     ! do loop.  Only the routine with the same name may unlock the
     ! parallel loop. This implementation assumes that all routines
     ! with parallel do loops have distinct names (append with the
     ! module name, if not; or use a unique ID).  Further, recursive
     ! routines cannot have parallel do loops.
     INT :: master_rank DEFAULT(0)
     ! The rank of the master processor. It can be useful to change
     ! this, since only the master processor is allowed to so IO.
     INT :: processor_rank DEFAULT(0)
     ! The rank number of the this current processor
     INT :: n_processors DEFAULT(1)
     ! The number of processors
     INT :: p_loop_lbound DEFAULT(1)
     ! The minimum of the (shared) parallel loop index.
     INT :: p_loop_ubound DEFAULT(0)
     ! The maximum of the (shared) parallel loop index.
     INT :: p_loop_index DEFAULT(0)
     ! The actual index of the (shared) parallel loop.
     VEC(INT,:), allocatable :: p_loop_list
     ! A list of sorted indices so that parallel work can be
     ! distributed in an user-determined order.

#ifdef MPI
     INT :: mpi_comm DEFAULT(0)
     ! Communicator for all MPI routines.
     INT :: mpi_error DEFAULT(0)
     ! MPI error return code
     VEC(INT,MPI_STATUS_SIZE) :: mpi_status
     ! Status array for MPI routines.
     INT :: mpi_request DEFAULT(0)
     ! MPI request parameter for asynchornous routines.
     INT :: max_n_skip_proc DEFAULT(0)
     ! maximum number of processors that can be skipped when assigning a
     ! processor grid.
     VEC(INT,50) :: blacs_2d_context
     ! BLACS internal descriptor.
     INT :: proc_grid_nrow DEFAULT(0)
     ! Number of processors in a row of the processor grid
     ! nprows * npcols <= n_processors
     INT :: proc_grid_ncol DEFAULT(0)
     ! Number of processors in a column of the processor grid
     INT :: proc_grid_myrow DEFAULT(0)
     ! This processor's row of the processor grid
     INT :: proc_grid_mycol DEFAULT(0)
     ! This processor's column of the processor grid
#endif
   end type

#ifdef MPI
   type TEST_PARALLEL_TYPE
   end type
#endif
   type SYSTEM_TYPE

     ! ===================
     ! Errors and warnings
     ! ===================
     INT :: error_status DEFAULT(0)
     ! Set non-zero on error.
     BIN :: warnings_issued DEFAULT(FALSE)
     ! Set TRUE if warings were issued through the system
     INT :: stdin_unit DEFAULT(5)
     ! The standard file unit number for input.
     INT :: stdout_unit DEFAULT(6)
     ! The standard file unit number for output.
     INT :: std_err_unit DEFAULT(7)
     ! The standard file unit number for error messages.

     ! ==============
     ! Output control
     ! ==============
     BIN :: low_verbosity DEFAULT(FALSE)
     ! Whether low output verbosity is desired
     BIN :: keyword_echo DEFAULT(FALSE)
     ! Whether to echo use-input keywords
     STR(len=PATH_SIZE) :: io_file_name DEFAULT(" ")
     ! Name of the file last read from. Blank if none.
     INT :: io_file_record DEFAULT(0)
     ! Line number reached in that file.
     STR(len=BSTR_SIZE) :: io_file_line DEFAULT(" ")
     ! The line itself.
     INT :: io_file_cursor DEFAULT(0)
     ! How far along that line the parser had got.
     VEC(STR(len=STR_SIZE),:), allocatable :: known_keywords
     ! List of known keywords in the current case statement, if
     ! applicable This allows a nice error message to be returned
     ! saying what the allowed options in the case statement were.

     ! ==========================================================
     ! Parallel processor system options, inherited from PARALLEL
     ! ==========================================================
     BIN :: is_parallel DEFAULT(FALSE)
     ! Whether we are doing a parallel job, or not.
     BIN :: per_rank_IO_allowed DEFAULT(FALSE)
     ! Determines whether IO is allowed for all processors, or only
     ! the master. Default false.
     STR(len=STR_SIZE) :: parallel_do_lock DEFAULT(" ")
     ! This is set to the name of the routine which calls the parallel
     ! do loop.  Only the routine with the same name may unlock the
     ! parallel loop. This implementation assumes that all routines
     ! with parallel do loops have distinct names (append with the
     ! module name, if not; or use a unique ID).  Further, recursive
     ! routines cannot have parallel do loops.
     INT :: master_rank DEFAULT(0)
     ! The rank of the master processor. It can be useful to change
     ! this, since
     INT :: processor_rank DEFAULT(0)
     ! The rank number of the current processor
     INT :: n_processors DEFAULT(1)
     ! The number of processors
     INT :: p_loop_lbound DEFAULT(1)
     ! The minimum of the (shared) parallel loop index.
     INT :: p_loop_ubound DEFAULT(0)
     ! The maximum of the (shared) parallel loop index.
     INT :: p_loop_index DEFAULT(0)
     ! The actual index of the (shared) parallel loop.
     VEC(INT,:), allocatable :: p_loop_list
     ! A list of sorted indices so that work can be distributed in an
     ! asecending or descending manner.

#ifdef MPI
     INT :: mpi_comm DEFAULT(0)
     ! Communicator for all MPI routines.
     INT :: mpi_error DEFAULT(0)
     ! MPI error return code
     VEC(INT,MPI_STATUS_SIZE) :: mpi_status
     ! Status field used for all MPI routines.
     INT :: mpi_request DEFAULT(0)
     ! MPI request parameter for asynchornous routines.

#endif

     ! ================================================================
     ! Shell command execution, inherited from SYSTEM_COMMAND
     ! ================================================================
     ! These were the components of a separate SYSTEM_COMMAND type. They
     ! live here now because SYSTEM_COMMAND is a *virtual* module whose
     ! code is textually inherited into SYSTEM, exactly as PARALLEL is --
     ! so there is one command slot on the one global "tonto" object, and
     ! no new fortran dependency. They are prefixed "command_" because in
     ! that setting bare names like "status" and "message" would be far
     ! too generic (cf. .error_status just above).
     STR(len=STR_SIZE) :: command DEFAULT(" ")
     ! The shell command line to be executed by .execute_command.
     BIN :: command_wait DEFAULT(TRUE)
     ! Whether to wait for the command to finish (synchronous, the
     ! default) or to let it run in the background (asynchronous).
     INT :: command_exit_status DEFAULT(0)
     ! The system-dependent exit status of the executed command. Only
     ! meaningful for a synchronous command that could be executed at all.
     INT :: command_status DEFAULT(0)
     ! On exit, assigned:
     !  0  if no errors,
     ! -1  if command can't be executed
     ! -2  if asynchronous (wait=FALSE) execution is not supported
     ! >0  processor-dependent error condition
     STR(len=STR_SIZE) :: command_message DEFAULT(" ")
     ! Any explanatory message, set only when .command_status is non-zero.
   end type
   type COMMAND_LINE_TYPE
     INT :: n_items DEFAULT(0)
     ! Number of items on the command line.
     ! WARNING: do not confuse this with arguments.
     STR(len=PATH_SIZE) :: command DEFAULT(" ")
     ! The command name i.e. program name
     VEC(STR(len=PATH_SIZE),:), allocatable :: command_optarg
     ! The array of command options followed by command arguments
     VEC(STR(len=PATH_SIZE),:), allocatable :: item
     ! The complete list of command line items
     INT :: n_options DEFAULT(0)
     ! Number of optional arguments
     VEC(STR(len=PATH_SIZE),:), allocatable :: option
     ! List of options, dashes stripped, e.g. "output" for "--output name"
     VEC(STR(len=PATH_SIZE),:), allocatable :: option_value
     ! List of option values e.g. "name" for "--output name".
     ! A valueless flag e.g. "--help" has the value "none".
     INT :: n_args DEFAULT(0)
     ! Number of non-optional arguments.
     VEC(STR(len=PATH_SIZE),:), allocatable :: arg
     ! List of non-optional arguments.
   end type

!  =====
!  Files
!  =====
   type FILE_TYPE
     STR(len=PATH_SIZE) :: name DEFAULT("unknown")
     ! The name of the file
     INT :: unit DEFAULT(0)
     ! The unit number
     INT :: record DEFAULT(1)
     ! The current record of the output
     INT :: io_status DEFAULT(0)
     ! Set non-zero if there is an error condition
     STR(len=STR_SIZE) :: action DEFAULT("unknown")
     ! The type of action performed on this file
     STR(len=STR_SIZE) :: file_status DEFAULT("unknown")
     ! File status
   end type
   type BUFFER_TYPE
     STR(len=BSTR_SIZE) :: string DEFAULT(" ")
     ! This is the actual buffer-string
     BIN :: analyzed DEFAULT(FALSE)
     ! True, if the buffer-string has been analyzed
     INT :: n_items DEFAULT(0)
     ! The total number of items in the string
     INT :: item_index DEFAULT(0)
     ! The item number of the last item processed in the buffer string
     INT :: item_start DEFAULT(0)
     ! The position of the first character of the last item processed in
     ! the buffer-string
     INT :: item_end DEFAULT(0)
     ! The position of the last character of the last item processed in
     ! the buffer string
     STR(len=STR_SIZE) :: comment_chars DEFAULT(BUFFER_COMMENT_CHARS)
     ! The comment character symbols (concatenated) to be used in the
     ! buffer string
     STR(len=STR_SIZE) :: quote_chars DEFAULT(BUFFER_QUOTE_CHARS)
     ! The quote character symbols (concatenated) to be used in the
     ! buffer string
     BIN :: ignore_unmatched_quotes DEFAULT(FALSE)
     ! If TRUE then unmatched quote character on a sigle line are deemed an error.
     BIN :: eliminate_specials DEFAULT(TRUE)
     ! If TRUE then non-ascii characters are set to blank
     MAT(STR(len=STR_SIZE),:,:), allocatable :: replacement_list
     ! A list of replacement strings to apply to the buffer string, before
     ! analysis.
   end type
   type OUTPUT_STYLE_TYPE
     BIN :: using_fields DEFAULT(TRUE)
     ! If set TRUE, then items are output in set fields with certain
     ! field widths.  e.g. integers have withd int_width. If not TRUE
     ! then the minimum field width is used.
     INT :: n_fields_per_line DEFAULT(5)
     ! No. of fields per line (i.e. no of columns) to use for output
     ! in spaces.
     INT :: spacing DEFAULT(2)
     ! Spacing between fields, in spaces
     INT :: int_width DEFAULT(8)
     ! Width of an integer field in spaces
     INT :: real_width DEFAULT(16)
     ! Width of a real field in spaces
     INT :: real_precision DEFAULT(6)
     ! No. of decimal places to use for outputing a real variable
     ! in spaces
     STR(len=2) :: real_fortran_fmt DEFAULT("f")
     ! Fortran format string for a real variable.
     ! Must be one of: "f ", "d ", "e ", "es", "en", "g ".
     INT :: margin_width DEFAULT(0)
     ! Margin width in spaces
     BIN :: using_array_labels DEFAULT(TRUE)
     ! Whether to use column and row labels on vectors and matrices
     STR(len=STR_SIZE) :: default_units DEFAULT(" ")
     ! Each REAL number is assumed to have "default_units", and are
     ! converted into generic (internal) units, usually A.U.
     STR(len=STR_SIZE) :: comment_chars DEFAULT("!#")
     ! The single-character comment character symbols to be used in
     ! the textfile, as a concatenated string.
     STR(len=STR_SIZE) :: quote_chars DEFAULT('''"')
     ! The single-character quote character symbols to be used in the
     ! textfile, as a concatenated string. The open and close quotes
     ! are the same character. Note ' escape character.
     STR(len=STR_SIZE) :: list_delimiters DEFAULT("{}")
     ! The two list delimiters used to open and close variable-length
     ! lists *or* arrays.
     BIN :: ignore_unmatched_quotes DEFAULT(FALSE)
     ! If TRUE then unmatched quote character on a sigle line are deemed an error.
     BIN :: eliminate_specials DEFAULT(TRUE)
     ! If TRUE then non-ascii characters are set to blank
     MAT(STR(len=STR_SIZE),:,:), allocatable :: replacement_list
     ! A list of strings and their replacements, to be applied when every line is
     ! inputted, if required.
   end type
   type TEXTFILE_TYPE
     STR(len=PATH_SIZE) :: name DEFAULT("unknown")
     ! The name of the text file
     STR(len=STR_SIZE) :: action DEFAULT("unknown")
     ! The intended actions for the file: either "read" or "write".
     INT :: unit DEFAULT(0)
     ! The fortran file unit number.
     INT :: record DEFAULT(0)
     ! The current record (line number) of the cursor in the file.
     INT :: no_of_lines DEFAULT(-1)
     ! The number of lines in the file. Only set if EOF is hit.
     INT :: IO_status DEFAULT(0)
     ! Set non-zero if an IO error condition occured.
     BIN :: ignore_EOF DEFAULT(FALSE)
     ! Set TRUE if the end of file is not to be regarded as an error.
     type(BUFFER_TYPE) :: buffer
     ! Holds current record of file, to be read-in, or written out.
     VEC(STR(len=BSTR_SIZE),:), allocatable :: internal
     ! The entire file as a string. A line buffer, so BSTR_SIZE, not STR_SIZE.
     type(TEXTFILE_TYPE), allocatable :: saved
     ! The previous file, used for input redirection: a file stack.
     type(OUTPUT_STYLE_TYPE) :: style
     ! The output style options.
     type(OUTPUT_STYLE_TYPE) :: saved_style
     ! Previously used output style options; that can be restored.
   end type
   type ARCHIVE_TYPE
     STR(len=STR_SIZE) :: root_name DEFAULT("unknown")
     ! Archive root name (usually, the name of the job)
     STR(len=STR_SIZE) :: name DEFAULT("unknown")
     ! Archive name (usually, the name of the entity in the archive)
     STR(len=STR_SIZE) :: genre DEFAULT(" ")
     ! Archive basis genre for opmatrix objects, or a qualifier for name
     STR(len=STR_SIZE) :: format DEFAULT(" ")
     ! Archive format (blank for binary)
     BIN :: use_all_commas DEFAULT(FALSE)
     ! Whether to use all commas in the archive name
     type(TEXTFILE_TYPE), allocatable :: Afile
     ! ASCII (text) file part
     type(FILE_TYPE), allocatable :: Bfile
     ! Binary file part
   end type
   type CIF_TYPE
     STR(len=PATH_SIZE) :: file_name DEFAULT(" ")
     ! The name of the CIF file to open
     BIN :: is_mmCIF DEFAULT(FALSE)
     ! Set whether to use mmCIF input conventions
     BIN :: use_CIF2 DEFAULT(FALSE)
     ! Set whether to use CIF2 output conventions.
     ! In Tonto, CIF2 means cartesian.
     BIN :: use_fragments DEFAULT(FALSE)
     ! Set whether to make fragments to split protein up.
     type(TEXTFILE_TYPE), allocatable :: file
     ! The actual CIF file to be processed
     STR(len=STR_SIZE) :: data_block_name DEFAULT(" ")
     ! The name of the data block
     INT :: start_of_data DEFAULT(1)
     ! The starting line of the data block
     INT :: end_of_data DEFAULT(0)
     ! The end line of the data block
     BIN :: data_block_found DEFAULT(FALSE)
     ! Set TRUE if a data block has been sucessfully found
     REAL :: BH_bond_length DEFAULT(-CIF_BH_BOND_LENGTH*BOHR_PER_ANGSTROM)
     ! If greater than zero any BH bonds in the CIF file are set to this
     REAL :: CH_bond_length DEFAULT(-CIF_CH_BOND_LENGTH*BOHR_PER_ANGSTROM)
     ! If greater than zero any CH bonds in the CIF file are set to this
     REAL :: NH_bond_length DEFAULT(-CIF_NH_BOND_LENGTH*BOHR_PER_ANGSTROM)
     ! If greater than zero any NH bonds in the CIF file are set to this
     REAL :: OH_bond_length DEFAULT(-CIF_OH_BOND_LENGTH*BOHR_PER_ANGSTROM)
     ! If greater than zero any OH bonds in the CIF file are set to this
     BIN :: U_iso_is_B_iso DEFAULT(FALSE)
     ! If TRUE, then any U_iso values in the CIF file are interpreted as B_iso
     ! values. This is a hack that should not be used except when the CIF file is
     ! hand made using B_iso values.
     BIN :: U_is_B DEFAULT(FALSE)
     ! If TRUE, then any U values (i.e. ADP's) in the CIF file are interpreted as B
     ! values. This is a hack that should not be used except when the CIF file is
     ! hand made using B_iso values.
     BIN :: U_is_beta DEFAULT(FALSE)
     ! If TRUE, then any U values (i.e. ADP's) in the CIF file are interpreted as
     ! beta values. This is a hack that should not be used except when the CIF
     ! file is hand made using B_iso values.
     STR(len=STR_SIZE) :: GC_format DEFAULT(CIF_GC_FORMAT)
     ! This may be "g" for the dimensionless Gram-Charlier anharmonic
     ! constants, OR "u"  i.e. the constants are dimensioned in
     ! Angstrom^n, where n=3,4 is the order of the constants. This is
     ! like the second order cartesian ADPs, but is non-standard,
     ! see Trueblood (1996) Acta Cryst A 52 p. 770-81 (34)-(37), but
     ! in that paper "h" is our k/2\pi.
     REAL :: GC3_scale_factor DEFAULT(-ONE)
     ! The Gram-Charlier 3rd order constants are scaled by this factor.
     STR(len=STR_SIZE) :: GC3_label DEFAULT(CIF_GC3_LABEL)
     ! Label used for the 3rd order anharmonic Gram-Charlier constants
     REAL :: GC4_scale_factor DEFAULT(-ONE)
     ! The Gram-Charlier 4th order constants are scaled by this factor.
     STR(len=STR_SIZE) :: GC4_label DEFAULT(CIF_GC4_LABEL)
     ! Label used for the 3rd order anharmonic Gram-Charlier constants
   end type

!  ============
!  Table output
!  ============
   type TABLE_COLUMN_TYPE
     STR(len=STR_SIZE) :: heading DEFAULT(" ")
     ! Column heading
     STR(len=STR_SIZE) :: subheading DEFAULT(" ")
     ! Column subheading
     STR(len=STR_SIZE) :: sb3heading DEFAULT(" ")
     ! Column sub3 heading
     INT :: width DEFAULT(9)
     ! Column width
     BIN :: auto_width DEFAULT(TRUE)
     ! Set table width automatically from maximum column element,
     ! including any headings or subheadings.
     BIN :: width_set DEFAULT(FALSE)
     ! Was the width set?
     INT :: item_width DEFAULT(9)
     ! Either max real item width, or list item width
     BIN :: item_width_set DEFAULT(FALSE)
     ! Was the item width set?
     INT :: real_precision DEFAULT(6)
     ! Real precision
     STR(len=2) :: real_fortran_fmt DEFAULT("f")
     ! Fortran format string for a real variable.
     ! Must be one of: "f ", "d ", "e ", "es", "en", "g ".
     INT :: max_cp DEFAULT(6)
     ! Maximum closing brace position when outputting real numbers
     ! with errors as in 123.456(7) <-- position of last parens ")".
     ! Used by default, when e_under_r is FALSE.
     BIN :: e_under_r DEFAULT(FALSE)
     ! If set TRUE, the error is printed underneath the real number.
     ! Normally the error is placed in parens at the end, IUCr style.
     ! For example like 123.456(7).
     BIN :: two_digit_error DEFAULT(FALSE)
     ! Whether to always use two digit errors like 1.23(45) for extra 
     ! accuracy on errors - useful for testing and checking.
     INT :: column_spacing DEFAULT(2)
     ! Column spacing
     INT :: list_item_spacing DEFAULT(2)
     ! Column spacing between *list* items
     STR(len=STR_SIZE) :: spacer DEFAULT(" ")
     ! Column spacer, applied *before* the column; blanks means
     ! to use no spacer.
     BIN :: center DEFAULT(FALSE)
     ! Column centering
     BIN :: left_justify DEFAULT(FALSE)
     ! Column left-justify
     BIN :: list_as_column DEFAULT(FALSE)
     ! Put a X-list column as a column, one-per-line
     ! NOTE: this can only be set when the list is the last column.
     ! NOTE: Cannot use the e_under_r option.
     INT :: n_list_fields DEFAULT(10)
     ! The no. of fields in an X-list column
     ! NOTE: this can only be set when the list is the last column.
     ! NOTE: Cannot use the e_under_r option.
     BIN :: values_created DEFAULT(FALSE)
     ! Set true if the column was created
     VEC(BIN,:), allocatable :: b_values
     ! Binary column values
     VEC(INT,:), allocatable :: i_values
     ! Integer column values
     VEC(REAL,:), allocatable :: r_values
     ! Real column values
     VEC(REAL,:), allocatable :: e_values
     ! Real column *error* values
     VEC(STR(len=STR_SIZE),:), allocatable :: s_values
     ! String column values
     VEC(type(EVEC_INT_TYPE),:), allocatable :: iv_values
     ! Integer list column values
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: rv_values
     ! Integer list column values
     VEC(type(EVEC_STR_TYPE),:), allocatable :: sv_values
     ! String list column values
   end type

!  ==============================
!  Polynomials and PES expansions
!  ==============================
   type TERM_1D_TYPE
     STR(len=STR_SIZE) :: descriptor DEFAULT(" ")
     ! The character code for the term e.g. q(3)^2 or tpt(5)[-2.0,0.3].
     ! "q" or "tpt" is the "variable type", "3" and "5" is the "degree
     ! of freedom" or index of this variable, and "2" and "-2,0,0.3"
     ! are parameters associated with this monomial (the "2" is of
     ! course the power).
     type(MAP_STR_INT_TYPE), allocatable :: d1_codes
     ! Stores the 1D variable types and their associated integer
     ! "varcode". Always pointer assigned.
     BIN :: analyzed DEFAULT(FALSE)
     ! Set TRUE when the monomial is analyzed

     ! =================================
     ! Below are produced after analysis
     ! =================================
     STR(len=STR_SIZE) :: variable DEFAULT(" ")
     ! The variable type associated with the monomial
     INT :: variable_code DEFAULT(0)
     ! The variable code associated with the monomial: it is derived
     ! from d1_codes
     INT :: dof DEFAULT(0)
     ! The degree of freedom associated with the variable
     VEC(REAL,:), allocatable :: parameters
     ! The values of any parameters associated with the variable.
     ! You have to know the convention of what each parameter means.
   end type
   type TERM_TYPE
     REAL :: coefficient DEFAULT(ZERO)
     ! The coeficient associated with the term
     STR(len=STR_SIZE) :: descriptor DEFAULT(" ")
     ! The character code associated with this term e.g.
     ! q(3)^2.q(1)^4. This is the way a term is inputted.
     type(MAP_STR_INT_TYPE), allocatable :: d1_codes
     ! Stores the 1D variable types to be recognized and their
     ! associated "varcode" indices in a map table. Pointer assigned.
     BIN :: analyzed DEFAULT(FALSE)
     ! Set TRUE when the term is analyzed

     ! =================================
     ! Below are produced after analysis
     ! =================================
     INT :: n_monomials DEFAULT(0)
     ! The number of 1D monomials in the term
     VEC(type(TERM_1D_TYPE),:), allocatable :: monomial
     ! The array of 1D monomials into which the descriptor is decoded
     VEC(BIN,:), allocatable :: has_any_var
     ! Set TRUE if the variable with the given "varcode" is present as
     ! a 1D term, at any part position
     VEC(BIN,:), allocatable :: has_any_dof
     ! Set TRUE if the dof with the given "dof" index is present as
     ! a 1D term, at any part position
   end type
   type POLYNOMIAL_TYPE
     STR(len=STR_SIZE) :: name DEFAULT(" ")
     ! The name of this entity
     type(MAP_STR_INT_TYPE), allocatable :: d1_codes
     ! Stores the recognised 1D variable types and their associated
     ! indices in a map table.
     VEC(type(TERM_TYPE),:), allocatable :: series_expansion
     ! The polynomial provided as input by the user
   end type

!  =============
!  Interpolation
!  =============
   type INTERPOLATOR_TYPE
     STR(len=STR_SIZE) :: interpolation_method DEFAULT(INTERPOLATOR_INTERPOLATION_METHOD)
     ! The interpolation method used, usually "linear". Also allowed
     ! is "cubic_spline".
     BIN :: interpolation_is_spline DEFAULT(FALSE)
     ! Set TRUE if it is a spline interpolation
     STR(len=STR_SIZE) :: range_mapping DEFAULT(INTERPOLATOR_RANGE_MAPPING)
     ! How the function values are treated before being stored as
     ! interpolation table data_values, i.e. "none" or "logarithmic"
     BIN :: range_is_logarithmic DEFAULT(FALSE)
     ! Set TRUE if range has ben logged
     STR(len=STR_SIZE) :: domain_mapping DEFAULT(INTERPOLATOR_DOMAIN_MAPPING)
     ! How the data_points are treated before evaluation by the
     ! interpolation function, e.g. "none", "sqrt", "sqrt(x/(1-x))*scale".
     ! NOTE: when obtaining the tabulated values it is the users
     ! responsibility to use the inverse domain mapping function to
     ! obtain the actual table value x.
     REAL :: domain_scale_factor DEFAULT(ZERO)
     ! A factor which is used to rescale the domain mapping function
     BIN :: domain_is_sqrt DEFAULT(FALSE)
     ! Set TRUE if domain has been sqrted
     BIN :: domain_is_sqrt_x1x DEFAULT(FALSE)
     ! Set TRUE if domain maps to sqrt(x/(1-x))
     REAL :: table_origin DEFAULT(INTERPOLATOR_TABLE_ORIGIN)
     ! The origin of the table use i.e. where the table starts. Used for setting
     ! up even spaced data.
     REAL :: table_length DEFAULT(INTERPOLATOR_TABLE_LENGTH)
     ! The length of the table use; it starts from the origin. Used for setting up
     ! even spaced data.
     REAL :: table_spacing DEFAULT(INTERPOLATOR_TABLE_SPACING)
     ! The spacing between data points, if using an even spaced grid.
     ! (this is the case if spacing in non-zero)
     REAL :: table_eps DEFAULT(INTERPOLATOR_TABLE_EPS)
     ! If there are values smaller than this in the interpolation table, they are
     ! set to zero. The interpolation table may be reduced in size.
     INT :: n_data DEFAULT(0)
     ! The number of "data_points" and "values" in the table.
     VEC(REAL,:), allocatable :: data_point
     ! The list of data points, from smallest to largest.
     VEC(REAL,:), allocatable :: data_value
     ! The list of values corresponding to each data value.
     BIN :: finalized DEFAULT(FALSE)
     ! Set TRUE if the object is ready for use
     VEC(REAL,:), allocatable :: y2
     ! The tabulated values of the second derivatives at the data_point's. This is
     ! used only for cubic spline interpolation.
     REAL :: y1_initial DEFAULT(INTERPOLATOR_Y1_INITIAL)
     ! The first derivative at the initial data_point to use, when doing cubic
     ! spline interpolation.
     REAL :: y1_final DEFAULT(INTERPOLATOR_Y1_FINAL)
     ! The first derivative at the final data_point to use, when doing cubic
     ! spline interpolation.
   end type

!  ==========
!  Quadrature
!  ==========
   type RYS_TYPE
     INT :: n_roots DEFAULT(0)
     ! No. of rys roots
     VEC(REAL,16) :: weight
     ! The Rys weights, 1:n_roots used
     VEC(REAL,16) :: root
     ! The Rys roots, 1:n_roots used
   end type
   type ERI_SCRATCH_TYPE
     VEC(REAL,:), allocatable :: Ix
     ! Flat 2D x integrals, all primitive quartets and roots
     VEC(REAL,:), allocatable :: Iy
     ! Flat 2D y integrals
     VEC(REAL,:), allocatable :: Iz
     ! Flat 2D z integrals, Rys weights folded in
     VEC(REAL,:), allocatable :: esfs
     ! Flat (es|fs) integrals
     VEC(REAL,:), allocatable :: escd
     ! Flat (es|cd) integrals, after the cd transfer
     VEC(REAL,:), allocatable :: ints
     ! Flat (ab|cd) integrals, after the ab transfer
     VEC(REAL,:), allocatable :: ev
     ! (es| contracted J vector
     VEC(REAL,:), allocatable :: fv
     ! |fs) contracted J vector
     VEC(REAL,:), allocatable :: abcd
     ! Flat (ab|cd) output block of make_ERI, cartesian or spherical
   end type
   type ADAPTIVE_QUADRATURE_TYPE
     STR(len=STR_SIZE) :: algorithm DEFAULT(" ")
     ! Identifier for the kind of quadrature algorithm
     REAL :: accuracy DEFAULT(TOL(6))
     ! The accuracy required for adapative quadratures
     INT :: n_points DEFAULT(0)
     ! No. of quadrature points. For adaptive quadrature it is set to
     ! the number of function eveluations.
     REAL :: lower_limit DEFAULT(-ONE)
     ! Lower integration limit (if appropriate)
     REAL :: upper_limit DEFAULT(ONE)
     ! Upper integration limit (if appropriate)
     MAT(REAL,:,:), allocatable :: point
     ! The 3D quadrature points
     VEC(REAL,:), allocatable :: weight
     ! The quadrature weight
     VEC(REAL,:), allocatable :: embedded_weight
     ! The embedded set of quadrature weights==for embedded cubature.
   end type
   type QUADRATURE_TYPE
     STR(len=STR_SIZE) :: kind DEFAULT(" ")
     ! Identifier for the kind of quadrature
     INT :: n_pts DEFAULT(0)
     ! No of quadrature points, or the order of the quadrature
     BIN :: fold_wt_fn_in DEFAULT(TRUE)
     ! The quadrature of I = Int f(x) dx is I = Sum_i f(x_i) w_i, i.e. no weight
     ! function appears in the integrand; it has been folded into w_i.  If this
     ! switch is set FALSE then f(x) = F(x) W(x) and the quadrature is defined as
     ! I = Int F(x) W(x) dx = Sum_i F(x_i) W_i i.e. the function F(x) to be
     ! quadrated does *not* have the weight W(x) folded in, as does f(x). The
     ! weights are related by w_i = W(x_i) W_i. Normally the program calculated
     ! W_i but it delivers w_i.
     REAL :: lower_limit DEFAULT(-ONE)
     ! Lower integration limit (if appropriate)
     REAL :: upper_limit DEFAULT(ONE)
     ! Upper integration limit (if appropriate)
     REAL :: Gauss_Laguerre_alpha DEFAULT(ONE)
     ! Gauss-Laguerre alpha parameters
     REAL :: Gauss_Jacobi_alpha DEFAULT(ONE)
     ! Gauss-Jacobi alpha parameter
     REAL :: Gauss_Jacobi_beta DEFAULT(ONE)
     ! Gauss-Jacobi alpha parameter
     REAL :: eps DEFAULT(TOL(13))
     ! The relative accuracy of the roots when they are polished
     INT :: maxit DEFAULT(10)
     ! The maximum iterations to work solve for the roots.
     VEC(REAL,:), allocatable :: pts
     ! The quadrature point i.e. root of the appropriate orthogonal polynomial
     VEC(REAL,:), allocatable :: wts
     ! The quadrature weight
     BIN :: finalized DEFAULT(FALSE)
     ! Set TRUE when all initialisations are completed.
   end type
   type LEBEDEV_TYPE
     INT :: n_pts DEFAULT(0)
     ! No of quadrature points
     INT :: l DEFAULT(0)
     ! The angular momentum that this quadrature will integrate up to.
     INT :: n DEFAULT(0)
     ! Private counter to keep track of how many points have been
     ! added into the quadrature grid.
     INT :: s DEFAULT(0)
     ! Private counter to keep track of how many shells there are in
     ! the quadrature grid.
     MAT(REAL,:,:), allocatable :: pts
     ! Coordinates of the quadrature points
     VEC(REAL,:), allocatable :: wts
     ! The quadrature weights (normalized to unity)
     VEC(INT,:), allocatable :: n_pts_per_shell
     ! No of points per shell. Used to save work when calculating the
     ! atomic grids on each shell.
   end type
   type WOMERSLEY_TYPE
     INT :: n_pts DEFAULT(0)
     ! No of quadrature points
     INT :: t DEFAULT(0)
     ! The angular momentum that this quadrature will integrate up to.
     MAT(REAL,:,:), allocatable :: pts
     ! Coordinates of the quadrature points
   end type

!  ====================
!  DIIS & extrapolation
!  ====================
   type AITKEN_TYPE
     INT :: iteration DEFAULT(0)
     ! The iteration count for the AITKEN extrapolation.
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: param
     ! List of stored parameter vectors
     REAL :: error_length DEFAULT(1000d0)
     ! The length of the DIIS error vector
   end type
   type DIIS_TYPE
     INT :: iteration DEFAULT(0)
     ! The iteration count for the DIIS procedure. When it equals 0,
     ! the "initial guess" parameters param(0)[:] are specified.
     BIN :: use_errors DEFAULT(TRUE)
     ! Set TRUE if the extrapolation procedure supplies its own error
     ! vectors. If a corresponding error vector is supplied, the first
     ! nontrivial DIIS extrapolation occurs at iteration 2.  If no
     ! error vector is supplied, the error at iteration (i) is defined
     ! as error(i)[:] = param(i+1)[:] - param(i)[:] and the dimension
     ! of the DIIS problem is one less than the number of parameter
     ! vectors stored, so extrapolation occurs at actual iteration 3.
     INT :: start_iteration DEFAULT(DIIS_START_ITERATION)
     ! The iteration of the *parent iterative cycle* when DIIS is
     ! supposed to start e.g. 4 if it is supposed to start on the 4th
     ! iteration of the SCF cycle (or, whatever iterative method you
     ! are trying to extrapolate).
     INT :: save_iteration DEFAULT(DIIS_SAVE_ITERATION)
     ! The iteration of the *parent iterative cycle* when the DIIS
     ! is supposed to start saving vectors though not necessarily
     ! start extrapolating. This must be less than or equal to the
     ! .start_iteration. The "actual iterations" begin at this number.
     INT :: keep DEFAULT(DIIS_KEEP)
     ! No. of DIIS vectors to keep for use in extrapolation
     INT :: kept DEFAULT(0)
     ! The actual number of DIIS vectors actually kept.
     VEC(INT,:), allocatable :: oldest
     ! A list of the indices of the oldest vectors stored in the param
     ! vector. The index of oldest vector is stored as the *last*
     ! entry in this list, .oldest(.kept) i.e. this is a first-in
     ! first-out (FIFO) list.
     VEC(INT,:), allocatable :: iter_4
     ! A list of the actual *parent iteration indices* corresponding
     ! to the saved .param list. Note that in SCF, the DIIS operates
     ! before the SCF iteration is cycled, i.e. the DIIS iteration
     ! index may be one larger that the SCF iteration index.
     VEC(INT,:), allocatable :: itor_4
     ! The previous iteration's iter_4, required due to index lag.
     INT :: new DEFAULT(0)
     ! The index of the new (current) parameter vector in the list of
     ! saved vectors. It is equal to .iteration only if
     ! .iteration<=.keep; otherwise it is in the range [1,.keep] and
     ! it corresponds to either the parameter vector with the "worst"
     ! error or the "oldest" vector.
     REAL :: error_length DEFAULT(1000d0)
     ! The length of the DIIS error vector
     REAL :: convergence_tolerance DEFAULT(DIIS_CONVERGENCE_TOLERANCE)
     ! This tolerance specifies when the length of the error vector is
     ! such that the DIIS procedure is regarded as converged.
     REAL :: start_tolerance DEFAULT(DIIS_START_TOLERANCE)
     ! This tolerance is specifies the length of the error vector
     ! below which the DIIS procedure is supposed to start. It should
     ! override the .start_iteration.
     STR(len=STR_SIZE) :: replacement_method DEFAULT(DIIS_REPLACEMENT_METHOD)
     ! Specifies the method to be used when replacing the DIIS
     ! vectors, in the case when the DIIS space becomes saturated. The
     ! choices are "worst" item, or "oldest" item.
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: param
     ! List of stored parameter vectors
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: error
     ! List of stored error vectors
     MAT(REAL,:,:), allocatable :: lhs
     ! The LHS of the DIIS equations
     VEC(REAL,:), allocatable :: rhs
     ! The RHS of the DIIS equations
     VEC(REAL,:), allocatable :: coeff
     ! The DIIS coefficicents.
   end type

!   type FOCK_DIIS
!
!     genre :: STR  DEFAULT(DIIS_GENRE)
!     ! The kind of DIIS procedure to use
!
!     iteration :: INT  DEFAULT(0)
!     ! The iteration count for the DIIS procedure. It gets incremented every time
!     ! one of the extrapolators/interpolators are called.
!
!     start_iteration :: INT  DEFAULT(DIIS_START_ITERATION)
!     ! The iteration when the DIIS procedure is supposed to start accelerating the
!     ! requested object. The DIIS acceleration procedure works on the saved vectors,
!     ! and the procedure can start saving vectors before starting the acceleration
!     ! procedure.
!
!     save_iteration :: INT  DEFAULT(DIIS_SAVE_ITERATION)
!     ! The iteration when the DIIS object starts saving vectors. It must be equal
!     ! or smaller than the start_iteration. The previously saved vectors are used
!     ! in the acceleration procedure.
!
!     keep :: INT  DEFAULT(DIIS_KEEP)
!     ! No. of DIIS vectors to keep for use in extrapolation
!
!     kept :: INT  DEFAULT(0)
!     ! The actual number of DIIS vectors kept.
!
!     oldest :: VEC{INT}*  DEFAULT_NULL
!     ! A list of the indices of the oldest vectors.
!
!     new :: INT  DEFAULT(0)
!     ! The integer identifier for the new (current) parameter vector, usually the
!     ! Fock matrix.  This is equal to .iteration only if .iteration<=.keep;
!     ! otherwise it is an integer in the range [1,.keep] which corresponds to the
!     ! parameter vector with the worst error (DIIS) or the oldest vector (EDIIS).
!     ! In this way, the worst/oldest parameter and error vectors are pruned.
!
!     error_length :: REAL  DEFAULT(ZERO)
!     ! The length of the DIIS error vector
!
!     convergence_tolerance :: REAL  DEFAULT(DIIS_CONVERGENCE_TOLERANCE)
!     ! This tolerance specifies when the DIIS/EDIIS procedure is regarded to be
!     ! converged.
!
!     start_tolerance :: REAL  DEFAULT(DIIS_START_TOLERANCE)
!     ! In the case when the method is "EDIIS+DIIS", this tolerance specifies when
!     ! the EDIIS procedure finishes and when the DIIS procedure begins.
!
!     encore_iterations :: INT  DEFAULT(DIIS_ENCORE_ITERATIONS)
!     ! In the case when the method is "EDIIS+DIIS", this tolerance specifies how
!     ! many EDIIS iterations are to be performed again, assuming that EDIIS was
!     ! already performed before, and DIIS has started to diverge.
!
!     stage :: INT   DEFAULT(0)
!     ! The stage of the "EDIIS_DIIS" calculation. If 0, we have not yet started
!     ! the first DIIS. If 1, we have finished EDIIS and started DIIS. If 2, we
!     ! have reached stage 1, but we need encore EDIIS iterations.
!
!     encore :: INT   DEFAULT(0)
!     ! The iteration counter for encore iterations.
!
!     replacement_method :: STR  DEFAULT(DIIS_REPLACEMENT_METHOD)
!     ! This specifies the method to be used when replacing the DIIS vectors, in
!     ! the case when the DIIS space becomes saturated. The choices are "worst"
!     ! item, or "oldest" item.
!
!     archive :: ARCHIVE
!     ! Archive used to store the DIIS parameter and error vectors
!
!     par_tag :: STR  DEFAULT("fock_mx")
!     ! A string tag used to identify the parameter vectors stored on disk, which
!     ! are used to construct the next DIIS approximation.
!
!     err_tag :: STR  DEFAULT("pulay_error")
!     ! A string tag used to identify the error vectors stored on disk, which
!     ! are used to construct the next DIIS approximation.
!
!     lhs_tag :: STR  DEFAULT("DIIS_lhs")
!     ! A string tag used to identify the DIIS LHS matrix stored on disk.
!
!     lhs_tol :: REAL  DEFAULT(TOL(10))
!     ! Tolernace for solving the non-linear DDIIS  equations iteratively.
!
!     S :: MAT{REAL}*  DEFAULT_NULL
!     ! The overlap matrix. Used to calculate commutators.
!
!     F :: OPMATRIX@
!     ! The current fock matrix. It gets stored on disk, and then extrapolated or
!     ! interpolated by the DIIS routines.
!
!     F_tag :: STR  DEFAULT("fock_mx")
!     ! A string tag used to identify the Fock matrices stored on disk, which
!     ! are used to construct the next DIIS approximation.
!
!     D :: OPMATRIX@
!     ! The density matrix corresponding to the current fock matrix. It gets stored
!     ! on disk and is used to calculate the commutators.
!
!     D_tag :: STR  DEFAULT("density_mx")
!     ! A string tag used to identify the Density matrices stored on disk, which
!     ! are used to construct the next DIIS approximation.
!
!     J :: OPMATRIX@
!     ! The coulomb matrix corresponding to the current fock matrix. It gets stored
!     ! on disk and is used to calculate commutators in the UHF case only.
!
!     J_tag :: STR  DEFAULT("coulomb_mx")
!     ! A string tag used to identify the Coulomb J matrices stored on disk, which
!     ! are used to construct the next DIIS approximation in the UHF case.
!
!     energy :: VEC{REAL}*   DEFAULT_NULL
!     ! The current SCF electronic energy (or energies, in the UHF case), used in
!     ! energy DIIS.
!
!     e_tag :: STR  DEFAULT("energy")
!     ! A string tag used to identify the energy stored on disk, which
!     ! are used to construct the next DIIS approximation.
!
!     force_convexity :: BIN  DEFAULT(DIIS_FORCE_CONVEXITY)
!     ! If TRUE the DIIS coefficients are forced to be positive to ensure that any
!     ! extrapolated density matrix is a convex linear combination. This is TRUE by
!     ! default for the EDIIS method.
!
!     micro_iteration_method :: STR   DEFAULT(DIIS_MICRO_ITERATION_METHOD)
!     ! The name of the method used to solve the DDIIS equations iteratively.
!     ! These are called DDIIS micro iterations.
!
!     micro_starting_guess :: STR   DEFAULT(DIIS_MICRO_STARTING_GUESS)
!     ! The starting guess to use for the solving the DDIIS equations iteratively.
!     ! They can be started with the "last" density or "DIIS" coefficients.
!
!     micro_convergence_tol :: REAL  DEFAULT(DIIS_MICRO_CONVERGENCE_TOL)
!     ! The tolerace for solving the non-linear DDIIS  equations iteratively.
!
!     micro_max_iterations :: INT  DEFAULT(DIIS_MICRO_MAX_ITERATIONS)
!     ! This is the maximum number of micro iterations allowed when solving the
!     ! non-linear DDIIS equations by simple relaxation.
!
!     micro_damp_factor :: REAL  DEFAULT(DIIS_MICRO_DAMP_FACTOR)
!     ! The damp factor to use when solving the non-linear DDIIS equations by
!     ! simple relaxation.
!
!   end
   type L_BFGS_TYPE
     INT :: iteration DEFAULT(0)
     ! The iteration count for the DIIS procedure. When it equals 0,
     ! the "initial guess" parameters param(0)[:] are specified.
     INT :: start_iteration DEFAULT(DIIS_START_ITERATION)
     ! The iteration of the *parent iterative cycle* when DIIS is
     ! supposed to start e.g. 4 if it is supposed to start on the 4th
     ! iteration of the SCF cycle (or, whatever iterative method you
     ! are trying to extrapolate).
     INT :: save_iteration DEFAULT(DIIS_SAVE_ITERATION)
     ! The iteration of the *parent iterative cycle* when the DIIS
     ! is supposed to start saving vectors though not necessarily
     ! start extrapolating. This must be less than or equal to the
     ! .start_iteration. The "actual iterations" begin at this number.
     INT :: keep DEFAULT(DIIS_KEEP)
     ! No. of parameters/gradient to keep for use in extrapolation
     REAL :: grd_norm DEFAULT(1000d0)
     ! The length of the latest gradient vector
     REAL :: grd_norm_a DEFAULT(1000d0)
     ! The length of the latest alpha gradient vector
     REAL :: grd_norm_b DEFAULT(1000d0)
     ! The length of the latest beta gradient vector
     REAL :: convergence_tolerance DEFAULT(DIIS_CONVERGENCE_TOLERANCE)
     ! This tolerance specifies when the length of the error vector is
     ! such that the DIIS procedure is regarded as converged.
     REAL :: start_tolerance DEFAULT(DIIS_START_TOLERANCE)
     ! This tolerance is specifies the length of the error vector
     ! below which the DIIS procedure is supposed to start. It should
     ! override the .start_iteration.
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: par
     ! List of stored parameter vectors
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: par_a
     ! List of stored alpha parameter vectors
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: par_b
     ! List of stored beta parameter vectors
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: grd
     ! List of stored gradient vectors
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: grd_a
     ! List of stored alpha gradient vectors
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: grd_b
     ! List of stored beta gradient vectors
   end type

!  =========
!  Gaussians
!  =========
   type GAUSSIAN_TYPE
     INT :: l DEFAULT(0)
     ! l quantum number for the gaussian
     REAL :: exponent DEFAULT(ZERO)
     ! Exponent for the gaussian
     VEC(REAL,3) :: position DEFAULT(ZERO)
     ! Position of the gaussian
     INT :: n_comp DEFAULT(0)
     ! No. of cartesian components for the gaussian
     INT :: n_sph DEFAULT(0)
     ! No. of spherical components for the gaussian shell
     INT :: first_gaussian DEFAULT(0)
     ! Lexical index of the first gaussian. For lookups.
     INT :: last_gaussian DEFAULT(0)
     ! Lexical index of the last gaussian. For lookups.
   end type
   type GAUSSIAN2_TYPE
     type(GAUSSIAN_TYPE) :: a
     ! Shell "a" of the pair
     type(GAUSSIAN_TYPE) :: b
     ! Shell "b" of the pair
   end type
   type GAUSSIAN4_TYPE
     type(GAUSSIAN_TYPE) :: a
     ! Shell "a" of the quartet
     type(GAUSSIAN_TYPE) :: b
     ! Shell "b" of the quartet
     type(GAUSSIAN_TYPE) :: c
     ! Shell "c" of the quartet
     type(GAUSSIAN_TYPE) :: d
     ! Shell "d" of the quartet
   end type
   type RMS_INDICES_TYPE
     VEC(INT,:), allocatable :: xyz0
     ! list of the indices where nx and ny and nz are zero
     VEC(INT,:), allocatable :: yz0
     ! list of the indices where ny and nz are zero (not including any above)
     VEC(INT,:), allocatable :: xz0
     ! list of the indices where nx and nz are zero (not including any above)
     VEC(INT,:), allocatable :: xy0
     ! list of the indices where nx and ny are zero (not including any above)
     VEC(INT,:), allocatable :: z0
     ! list of the indices where nz is zero (not including any above)
     VEC(INT,:), allocatable :: y0
     ! list of the indices where ny is zero (not including any above)
     VEC(INT,:), allocatable :: x0
     ! list of the indices where nx is zero (not including any above)
     VEC(INT,:), allocatable :: n0
     ! list of the indices where none of nx, ny, nz is zero (not including any above)
     VEC(INT,:), allocatable :: a0
     ! list of the indices where any  of nx, ny, nz is zero i.e. complement of n0

!    xy_rms :: VEC{EVEC{INT}}@
!    ! list of the repeated nx and ny indices for use in the reduced
!    ! multiplication scheme for indices which are non-zero
     VEC(type(EVEC_INT_TYPE),:), allocatable :: xy_rms_n
     ! list of the repeated nx and ny indices for use in the reduced
     ! multiplication scheme for indices which are non-zero

!    xy_unique_n :: VEC{INT}@
!    ! list of the unique nx and ny indices for use in the reduced
!    ! multiplication scheme for indices which are non-zero
     VEC(type(EVEC_INT_TYPE),:), allocatable :: xy_rms_a
     ! list of the repeated nx and ny indices for use in the reduced
     ! multiplication scheme where at least one index is zero

!    xy_unique_a :: VEC{INT}@
!    ! list of the unique nx and ny indices for use in the reduced
!    ! multiplication scheme where at least one index is zero
     VEC(INT,:), allocatable :: yz0px
     ! px powers for the yz0 indices
     VEC(INT,:), allocatable :: xz0py
     ! py powers for the xz0 indices
     VEC(INT,:), allocatable :: xy0pz
     ! pz powers for the xz0 indices
     VEC(INT,:), allocatable :: z0px
     ! px and py powers for the z0 indices
     VEC(INT,:), allocatable :: z0py
     ! px and py powers for the z0 indices
     VEC(INT,:), allocatable :: y0px
     ! px and pz powers for the y0 indices
     VEC(INT,:), allocatable :: y0pz
     ! px and pz powers for the y0 indices
     VEC(INT,:), allocatable :: x0py
     ! py and pz powers for the x0 indices
     VEC(INT,:), allocatable :: x0pz
     ! py and pz powers for the x0 indices
     VEC(INT,:), allocatable :: n1
     ! Indices and non-zero powers for non-rms terms
     VEC(INT,:), allocatable :: n1px
     ! Indices and non-zero powers for non-rms terms
     VEC(INT,:), allocatable :: n1py
     ! Indices and non-zero powers for non-rms terms
     VEC(INT,:), allocatable :: n1pz
     ! Indices and non-zero powers for non-rms terms
     VEC(type(EVEC_INT_TYPE),:), allocatable :: n2
     ! Indices and non-zero powers for rms terms
     VEC(type(EVEC_INT_TYPE),:), allocatable :: n2px
     ! Indices and non-zero powers for rms terms
     VEC(type(EVEC_INT_TYPE),:), allocatable :: n2py
     ! Indices and non-zero powers for rms terms
     VEC(type(EVEC_INT_TYPE),:), allocatable :: n2pz
     ! Indices and non-zero powers for rms terms
   end type
   type GAUSSIAN_DATA_TYPE
     INT :: l_max DEFAULT(3)
     ! The maximum l value for which the data is defined.
     BIN :: CRYSTAL23_ordering DEFAULT(FALSE)
   end type

!  =========================
!  Segmented gaussian shells
!  =========================
   type SHELL_TYPE
     INT :: l DEFAULT(0)
     ! (Maximum) l quantum number for the gaussian shell
     STR(len=STR_SIZE) :: l_chr DEFAULT(" ")
     ! The character symbol for the shell e.g. "s", "p", "L", "d" ...
     BIN :: is_spherical DEFAULT(SHELL_IS_SPHERICAL)
     ! Set TRUE if the components are spherical harmonics, else cartesian
     INT :: n_comp DEFAULT(0)
     ! No. of cartesian components for the gaussian shell
     INT :: n_sph DEFAULT(0)
     ! No. of spherical components for the gaussian shell
     INT :: n_bf DEFAULT(0)
     ! No. of basis functions in the shell. This is equal to .n_sph is
     ! .is_spherical is set, otherwise it is equal to .n_comp.
     INT :: first_gaussian DEFAULT(0)
     ! Lexical index of the first gaussian in the shell. For lookups.
     INT :: last_gaussian DEFAULT(0)
     ! Lexical index of the last gaussian in the shell. For lookups.
     INT :: n_cc DEFAULT(0)
     ! No. of contractions for the gaussian shell
     VEC(REAL,:), allocatable :: exponent
     ! Exponent vector
     VEC(REAL,:), allocatable :: contraction
     ! Contraction coefficient vector
   end type
   type SHELL1_TYPE
     INT :: l DEFAULT(0)
     ! (Maximum) l quantum number of the shell
     STR(len=STR_SIZE) :: l_chr DEFAULT(" ")
     ! The character symbol for the shell e.g. "s", "p", "l", "d" ...
     BIN :: is_spherical DEFAULT(SHELL_IS_SPHERICAL)
     ! Set TRUE if the components are spherical harmonics, else cartesian
     INT :: n_comp DEFAULT(0)
     ! No. of components for the shell
     INT :: n_sph DEFAULT(0)
     ! No. of spherical components for the gaussian shell
     INT :: n_bf DEFAULT(0)
     ! No. of basis functions in the shell
     INT :: first_gaussian DEFAULT(0)
     ! Lexical index of the first gaussian in the shell. For lookups.
     INT :: last_gaussian DEFAULT(0)
     ! Lexical index of the last gaussian in the shell. For lookups.
     INT :: n_cc DEFAULT(0)
     ! No. of contraction coefficients for the shell
     VEC(REAL,3) :: position DEFAULT(ZERO)
     ! Position of the shell
     VEC(REAL,:), allocatable :: exponent
     ! Exponent vector
     VEC(REAL,:), allocatable :: contraction
     ! Contraction coefficient vector
   end type
   type SHELL2_TYPE
     type(SHELL1_TYPE) :: a
     ! Shell "a" of the pair
     type(SHELL1_TYPE) :: b
     ! Shell "b" of the pair
     BIN :: is_spherical DEFAULT(SHELL_IS_SPHERICAL)
     ! Set TRUE if the components are spherical harmonics, else cartesian
     INT :: n_gaussian_pairs DEFAULT(0)
     ! The number of gaussian pair products in the shell pair, .a.n_cc*.b.n_cc
     INT :: l_max DEFAULT(0)
     ! Maximum of the angular momenta on each shell, max(.a.l,.b.l)
     INT :: l_min DEFAULT(0)
     ! Minimum of the angular momenta on each shell, min(.a.l,.b.l)
     INT :: l_sum DEFAULT(0)
     ! Sum of the angular momenta on each shell, .a.l+.b.l
     INT :: first_gaussian DEFAULT(0)
     ! Lexical index of the first gaussian in the shell with momentum .l_max
     INT :: last_gaussian DEFAULT(0)
     ! Lexical index of the last gaussian in the shell with momentum .l_sum
     INT :: n_gaussian DEFAULT(0)
     ! The number of gaussians in the source integral list.

     ! kappa_max :: REAL
     ! Maximum of .a.cc * .b.cc * kappa_ab for the two electron integrals.
     BIN :: destroy_all DEFAULT(TRUE)
     ! Set TRUE or if all the pointer parts are to be destroyed. Sometimes you
     ! want this FALSE when pointers are assigned to save computation.
     VEC(REAL,:), allocatable :: exponent_sum
     ! The sum of all gaussian pair exponents as a flattened array with shell "a"
     ! exponents incrementing most rapidly, .a.ex(:)+.b.ex(:)
     VEC(REAL,:), allocatable :: exponent_inv
     ! The inverse of the sum of the gaussian pair exponents, as a flattened
     ! array, 1/(.a.ex(:)+.b.ex(:))
     VEC(REAL,:), allocatable :: contraction_prefactor
     ! The contraction coefficient product with an exponential part for each
     ! gaussian pair as a flattened array with shell "a" gaussian components
     ! incrementing most rapidly. Useful for integral evaluation.
     BIN :: same_center
     ! If TRUE, then the centers of a and b are the same.
     MAT(REAL,:,:), allocatable :: pair_center
     ! The center of the product gaussian of gaussians .a and .b.
     VEC(REAL,3) :: av_pair_center DEFAULT(ZERO)
     ! The contraction-weighted average of the pair_center's.
     MAT(REAL,:,:), allocatable :: center_diff
     ! The difference between pair_center and the position of the gaussian of
     ! higher angular momentum out of .a and .b.
   end type
   type SHELL1PAIR_TYPE
     type(SHELL1_TYPE) :: a
     ! Shell "a" of the pair
     type(SHELL1_TYPE) :: b
     ! Shell "b" of the pair
     INT :: n_gaussian_pairs
     ! The number of gaussian pair products in the shell pair, .a.n_cc*.b.n_cc
     INT :: n_comp_pairs
     ! The number of cartesian gaussian component pairs in the shell
     ! pair, i.e.  .a.n_comp*.b.n_comp
     INT :: n_sph_pairs
     ! The number of spherical gaussian component pairs in the shell
     ! pair, i.e.  .a.n_sph*.b.n_sph
     INT :: n_bf_pairs
     ! The number of basis function component pairs in the shell pair,
     INT :: l_max
     ! Maximum of the angular momenta on each shell, max(.a.l,.b.l)
     INT :: l_min
     ! Minimum of the angular momenta on each shell, min(.a.l,.b.l)
     INT :: l_sum
     ! Sum of the angular momenta on each shell
     INT :: first_gaussian
     ! Lexical index of the first gaussian in the shell with momentum .l_max
     INT :: last_gaussian
     ! Lexical index of the last gaussian in the shell with momentum .l_sum
     INT :: n_gaussian
     ! The number of gaussians in the source integral list.
     VEC(REAL,:), allocatable :: exponent_sum
     ! The sum of all gaussian pair exponents as a flattened array with shell "a"
     ! exponents incrementing most rapidly, .a.ex(:)+.b.ex(:)
     VEC(REAL,:), allocatable :: exponent_inv
     ! The inverse of the sum of the gaussian pair exponents, as a flattened
     ! array, 1/(.a.ex(:)+.b.ex(:))
     VEC(REAL,:), allocatable :: contraction_prefactor
     ! The contraction coefficient product with an exponential part for each
     ! gaussian pair as a flattened array with shell "a" gaussian components
     ! incrementing most rapidly. Useful for integral evaluation.
     MAT(REAL,:,:), allocatable :: pair_center
     ! The center of the product gaussian of gaussians .a and .b.
   end type
   type GAUSSIAN_PAIR_LIST_TYPE
     INT :: n_pairs DEFAULT(0)
     ! Number of primitive pairs kept
     INT :: n_classes DEFAULT(0)
     ! Number of (l_max,l_min) classes
     VEC(INT,:), allocatable :: class_first
     ! First pair of each class
     VEC(INT,:), allocatable :: class_last
     ! Last pair of each class
     VEC(INT,:), allocatable :: class_l_sum
     ! l_max+l_min of each class
     VEC(INT,:), allocatable :: class_first_gaussian
     ! GAUSSIAN_DATA index of the first (es| component of each class
     VEC(INT,:), allocatable :: class_n_gaussian
     ! Number of (es| components of each class
     VEC(INT,:), allocatable :: shell_pair
     ! Shell pair index of each primitive pair
     VEC(REAL,:), allocatable :: factor
     ! One half for a pair of one shell with itself, else one
     VEC(REAL,:), allocatable :: exponent_sum
     ! Exponent sum, zeta
     VEC(REAL,:), allocatable :: exponent_inv
     ! One over zeta
     VEC(REAL,:), allocatable :: prefactor
     ! Contraction prefactor times the gaussian product exponential
     MAT(REAL,:,:), allocatable :: center
     ! Product centre P, (3,n_pairs)
     MAT(REAL,:,:), allocatable :: center_diff
     ! P minus the centre of the higher-l shell, (3,n_pairs)
   end type
   type SHELL4_TYPE
     type(SHELL1_TYPE) :: a
     ! Shell "a" of the quartet
     type(SHELL1_TYPE) :: b
     ! Shell "b" of the quartet
     type(SHELL1_TYPE) :: c
     ! Shell "c" of the quartet
     type(SHELL1_TYPE) :: d
     ! Shell "d" of the quartet
     BIN :: is_spherical DEFAULT(SHELL_IS_SPHERICAL)
     ! Set TRUE if the integrals from this shell should be converted
     ! to spherical harmonics
   end type
   type SHELL1QUARTET_TYPE
     type(SHELL1_TYPE) :: a
     ! Shell "a" of the quartet
     type(SHELL1_TYPE) :: b
     ! Shell "b" of the quartet
     type(SHELL1_TYPE) :: c
     ! Shell "c" of the quartet
     type(SHELL1_TYPE) :: d
     ! Shell "d" of the quartet
     BIN :: is_spherical DEFAULT(SHELL_IS_SPHERICAL)
     ! Set TRUE if the integrals from this shell should be converted
     ! to spherical harmonics
     INT :: ab_n_gaussian_pairs DEFAULT(0)
     ! No. of (ab| gaussian pair products in the shell pair
     INT :: ab_n_comp_pairs DEFAULT(0)
     ! No. of (ab| cartesian basis function pairs in the shell pair
     INT :: ab_n_sph_pairs DEFAULT(0)
     ! No. of (ab| spherical basis function pairs in the shell pair
     INT :: ab_n_bf_pairs DEFAULT(0)
     ! No. of (ab| basis function pairs in the shell pair
     INT :: ab_l_max DEFAULT(0)
     ! Maximum of the angular momenta on each shell, max(.a.l,.b.l)
     INT :: ab_l_min DEFAULT(0)
     ! Minimum of the angular momenta on each shell, min(.a.l,.b.l)
     INT :: ab_l_tri DEFAULT(0)
     ! Triangular index for the ab angular momentum pair
     INT :: ab_l_sum DEFAULT(0)
     ! Sum of the angular momenta on each shell, .a.l+.b.l
     INT :: ab_first_gaussian DEFAULT(0)
     ! Index of first gaussian in pair ab
     INT :: ab_last_gaussian DEFAULT(0)
     ! Index of last gaussian in pair ab
     INT :: n_ab DEFAULT(0)
     ! The number of ab gaussian functions required for the source
     ! integrals.
     VEC(REAL,:), allocatable :: ab_exponent_sum
     ! The sum of all gaussian pair exponents as a flattened array
     ! with shell "a" exponents incrementing most rapidly,
     ! .a.ex(:)+.b.ex(:)
     VEC(REAL,:), allocatable :: ab_exponent_inv
     ! The inverse of ab_exponent_sum.
     VEC(REAL,:), allocatable :: ab_contraction_prefactor
     ! The contraction coefficient product with an exponential part
     ! for each gaussian pair as a flattened array with shell "a"
     ! gaussian components incrementing most rapidly. Useful for
     ! integral evaluation.
     BIN :: ab_same_center DEFAULT(FALSE)
     ! Whether centers of a and b are the same.
     MAT(REAL,:,:), allocatable :: ab_pair_center
     ! The center of the product gaussian of gaussians .a and .b.
     MAT(REAL,:,:), allocatable :: ab_center_diff
     ! The difference between ab_pair_center and the position of the
     ! gaussian of higher angular momentum out of .a and .b.
     INT :: cd_n_gaussian_pairs DEFAULT(0)
     ! No. of |cd) gaussian pair products in the shell pair
     INT :: cd_n_comp_pairs DEFAULT(0)
     ! No. of |cd) cartesian basis function pairs in the shell pair
     INT :: cd_n_sph_pairs DEFAULT(0)
     ! No. of |cd) spherical basis function pairs in the shell pair
     INT :: cd_n_bf_pairs DEFAULT(0)
     ! No. of |cd) basis function component pairs in the shell pair
     INT :: cd_l_max DEFAULT(0)
     ! Maximum of the angular momenta on each shell, max(.c.l,.d.l)
     INT :: cd_l_min DEFAULT(0)
     ! Minimum of the angular momenta on each shell, min(.c.l,.d.l)
     INT :: cd_l_tri DEFAULT(0)
     ! Triangular index for the cd angular momentum pair
     INT :: cd_l_sum DEFAULT(0)
     ! Sum of the angular momenta on each shell, .c.l+.d.l
     INT :: cd_first_gaussian DEFAULT(0)
     ! Index of first gaussian in pair cd
     INT :: cd_last_gaussian DEFAULT(0)
     ! Index of last gaussian in pair cd
     INT :: n_cd DEFAULT(0)
     ! The number of cd gaussian functions required for the source
     ! integrals.
     VEC(REAL,:), allocatable :: cd_exponent_sum
     ! The sum of all gaussian pair exponents as a flattened array
     ! with shell "c" exponents incrementing most rapidly,
     ! .c.ex(:)+.d.ex(:)
     VEC(REAL,:), allocatable :: cd_exponent_inv
     ! The inverse of ab_exponent_sum.
     VEC(REAL,:), allocatable :: cd_contraction_prefactor
     ! The contraction coefficient product with an exponential part
     ! for each gaussian pair as a flattened array with shell "c"
     ! gaussian components incrementing most rapidly. Useful for
     ! integral evaluation.
     BIN :: cd_same_center DEFAULT(FALSE)
     ! Whether centers of c and c are the same.
     MAT(REAL,:,:), allocatable :: cd_pair_center
     ! The center of the product gaussian of gaussians .c and .d.
     MAT(REAL,:,:), allocatable :: cd_center_diff
     ! The difference between cd_pair_center and the position of the
     ! gaussian of higher angular momentum out of .c and .d.
     VEC(INT,:), allocatable :: ab_form_3dints_x_indices
     ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
     ! angular momenta from .ab_l_max up to .a.l+.b.l.
     VEC(INT,:), allocatable :: ab_form_3dints_y_indices
     ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
     ! angular momenta from .ab_l_max up to .a.l+.b.l.
     VEC(INT,:), allocatable :: ab_form_3dints_z_indices
     ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
     ! angular momenta from .ab_l_max up to .a.l+.b.l.
     VEC(INT,:), allocatable :: cd_form_3dints_x_indices
     ! Which indices to use for combining the 2d ERIs to 3d ERIS, for
     ! the angular momenta from .cd_l_max up to .c.l+.d.l.
     VEC(INT,:), allocatable :: cd_form_3dints_y_indices
     ! Which indices to use for combining the 2d ERIs to 3d ERIS, for
     ! the angular momenta from .cd_l_max up to .c.l+.d.l.
     VEC(INT,:), allocatable :: cd_form_3dints_z_indices
     ! Which indices to use for combining the 2d ERIs to 3d ERIS, for
     ! the angular momenta from .cd_l_max up to .c.l+.d.l.
     REAL :: r2ab DEFAULT(ZERO)
     ! Distance between shells .a and .b.
     REAL :: r2cd DEFAULT(ZERO)
     ! Distance between shells .c and .d.
   end type

!  =====
!  Bases
!  =====
   type BASIS_TYPE
     STR(len=STR_SIZE) :: label DEFAULT(" ")
     ! Unique label for the basis set
     BIN :: is_spherical DEFAULT(FALSE)
     ! Set TRUE if using spherical harmonic basis
     INT :: n_shell DEFAULT(0)
     ! No. of shells in the basis, equal to size(shell)
     INT :: n_bf DEFAULT(0)
     ! No. of basis functions
     INT :: n_prim DEFAULT(0)
     ! No. of primitives
     VEC(type(SHELL_TYPE),:), allocatable :: shell
     ! The list of gaussian shells in the basis set
     INT :: atom DEFAULT(0)
     ! The index of the atom to which the basis belongs.
     ! Only used when the BASIS is sorted by L.
   end type
   type SLATERSHELL_TYPE
     INT :: l DEFAULT(0)
     ! l quantum number
     STR(len=STR_SIZE) :: l_chr DEFAULT(" ")
     ! The character symbol for the shell e.g. "s", "p", "l", "d" ...
     INT :: n_comp DEFAULT(0)
     ! The number of l-components, normally 2*l+1 for spherical type slater
     ! functions.
     INT :: n_orb DEFAULT(0)
     ! The number of generally contracted orbitals.
     INT :: n_prim DEFAULT(0)
     ! No. of exponents/contractions (i.e. the number of primitives for one
     ! *single* angular momentum shell component, e.g. p_x).
     VEC(INT,:), allocatable :: n
     ! The n quantum numbers.
     VEC(REAL,:), allocatable :: z
     ! The exponent zeta for each slater function
     MAT(REAL,:,:), allocatable :: c
     ! The contraction coefficient matrix == this is a generally contracted slater
     ! shell. The size of .dim1 is "n_cc". The size of .dim2 is "n_orb".
     VEC(STR(len=STR_SIZE),:), allocatable :: orb_kind
     ! The orbital kinds (1s, 2s, 2p, etc.), if available.
     ! It's length is "n_orb".
     VEC(INT,:), allocatable :: occupancy
     ! The occupany number of each contracted orbital, if available.
     ! It's length is "n_orb".
   end type
   type SLATERBASIS_TYPE
     STR(len=STR_SIZE) :: label DEFAULT(" ")
     ! Unique label for the basis set
     STR(len=STR_SIZE) :: configuration DEFAULT(" ")
     ! A STR representation of the configuration.
     INT :: n_shell DEFAULT(0)
     ! No. of shells in the basis, equal to size(shell)
     INT :: n_bf DEFAULT(0)
     ! No. of basis functions for the shell
     INT :: n_prim DEFAULT(0)
     ! No. of primitives for the shell
     VEC(type(SLATERSHELL_TYPE),:), allocatable :: shell
     ! The list of gaussian shells in the basis set
     BIN :: use_interpolator DEFAULT(TRUE)
     ! Whether to use interpolators
     type(INTERPOLATOR_TYPE), allocatable :: interpolator
     ! An interpolator object, used for calculating the atomic density at a
     ! certain radius, from the slater "shell" of orbitals
     type(INTERPOLATOR_TYPE), allocatable :: d1_interpolator
     ! The same as above, but for the 1st derivative of the density.
     type(INTERPOLATOR_TYPE), allocatable :: d2_interpolator
     ! The same as above, but for the 2nd derivative of the density.
   end type
   type COPPENSORBITAL_TYPE
     STR(len=STR_SIZE) :: orb_kind DEFAULT("?")
     ! The kind of the orbital (1s, 2s, 2p, etc.)
     INT :: occupancy DEFAULT(0)
     ! The number of electrons in the orbital.
     INT :: n_fun DEFAULT(0)
     ! The number of contracted fitting functions
     VEC(INT,:), allocatable :: n
     ! The n quantum numbers
     VEC(REAL,:), allocatable :: c
     ! The contraction coefficient
     VEC(REAL,:), allocatable :: z
     ! The exponent zeta for each slater function
   end type
   type COPPENSBASIS_TYPE
     STR(len=STR_SIZE) :: label DEFAULT(" ")
     ! The basis label
     INT :: n_orb DEFAULT(0)
     ! The number of orbitals in the basis
     INT :: n_prim DEFAULT(0)
     ! The number of primitive functions in the basis
     VEC(type(COPPENSORBITAL_TYPE),:), allocatable :: orbital
     ! The list of fitted Slater atomic orbitals, Coppens style.
     BIN :: use_interpolator DEFAULT(TRUE)
     ! Whether to use interpolators
     type(INTERPOLATOR_TYPE), allocatable :: interpolator
     ! An interpolator object, used for calculating the atomic density at a
     ! certain radius, from e.g. the coppensbasis
   end type
   type T_TENSOR_TYPE
     REAL :: scalar DEFAULT(ZERO)
     VEC(REAL,3) :: alpha DEFAULT(ZERO)
     MAT(REAL,3,3) :: alpha_beta DEFAULT(ZERO)
     MAT3(REAL,3,3,3) :: alpha_beta_gamma DEFAULT(ZERO)
     MAT4(REAL,3,3,3,3) :: alpha_beta_gamma_delta DEFAULT(ZERO)
     REAL :: x,y,z
     REAL :: xx,yy,zz,xy,xz,yz
     REAL :: xxx,yyy,zzz,xxy,xxz,yyx,yyz,zzx,zzy,xyz
   end type

!  =====
!  Atoms
!  =====
   type ATOM_TYPE
     STR(len=STR_SIZE) :: label DEFAULT("?")
     ! The label for the atom (not necessarily unique)
     STR(len=STR_SIZE) :: tag DEFAULT(" ")
     ! The label may have tag information, which is extracted into here.
     ! It may be reset in the VEC{ATOM} module
     INT :: atomic_number DEFAULT(0)
     ! The atomic number
     REAL :: nuclear_charge DEFAULT(ZERO)
     ! The nuclear charge, used for dummy atom charges
     INT :: oxidation_state DEFAULT(0)
     ! The atomic oxidation state
     INT :: n_core_electrons DEFAULT(0)
     ! The number of core elctrons eliminated by e.g. the
     ! use of effective core potentials.

     ! ====
     ! Mass
     ! ====
     INT :: mass_number DEFAULT(0)
     ! The atomic mass number i.e. no. of protons and neutrons
     REAL :: mass DEFAULT(ZERO)
     ! The abundamce-weighted mass of the atom (if mass_number=0) or
     ! the isotope mass in C12 mass units.

     ! ========
     ! Position
     ! ========
     VEC(REAL,3) :: position DEFAULT(ZERO)
     ! Atom position
     VEC(REAL,3) :: position_error DEFAULT(ZERO)
     ! Error in atom position error (for refinements only)

     ! =================
     ! Group & site data
     ! =================
     INT :: group DEFAULT(0)
     ! An integer which describes the group to which the atom belongs
     REAL :: site_occupancy DEFAULT(ONE)
     ! The crystallographic site occupancy.
     INT :: site_disorder_group DEFAULT(0)
     ! The crystallographic site disorder group.

     ! =====
     ! ADP's
     ! =====
     REAL :: U_iso DEFAULT(ZERO)
     ! The isotropic ADP value for the atom
     REAL :: U_iso_error DEFAULT(ZERO)
     ! The isotropic ADP error for the atom
     REAL :: M_ani DEFAULT(ZERO)
     ! The *square root* of the anisotropy of the ADP tensor as
     ! defined in e.g. Stone (2013) Theory of Intermolecular Forces,
     ! p. 27 eqn. (2.3.16).
     REAL :: M_ani_error DEFAULT(ZERO)
     ! The error in M_ani
     MAT(REAL,:,:), allocatable :: covariance_mx
     ! Covariance matrix between this atom's parameters.
     ! NOTE: always in the cartesian axis system.
     BIN :: pADP_covariances_derived DEFAULT(FALSE)
     ! TRUE if "covariance_mx" was derived from a CIF's standard
     ! uncertainties by assuming them uncorrelated, rather than
     ! measured by a refinement.
     MAT(REAL,3,3) :: ADP_tensor DEFAULT(ZERO)
     ! The ADP (thermal) tensor for the atom
     VEC(REAL,:), allocatable :: pADP_vector
     ! The position-ADP vector for the atom. Used in refinements only.
     ! The order is x, y, z, Uxx, Uyy, Uzz, Uxy, Uxz, Uyz, Uxxx, Uyyy,
     ! and thereafter exactly for gaussian basis functions. See the
     ! GAUSSIAN_DATA module.
     VEC(REAL,:), allocatable :: pADP_errors
     ! The errors in the parameters (positions and ADP's). Used in
     ! refinements only. See above.

     ! ===========
     ! Axis system
     ! ===========
     STR(len=STR_SIZE) :: pos_axis_system DEFAULT("cartesian")
     ! Specifies the position coordinate system
     STR(len=STR_SIZE) :: ADP_axis_system DEFAULT("cartesian")
     ! Specifies the ADP (thermal tensor) coordinate system
     STR(len=STR_SIZE) :: ADPn_axis_system DEFAULT("cartesian")
     ! Specifies the ADP3 coordinate system
     STR(len=STR_SIZE) :: ADPn_form DEFAULT(ATOM_ADPN_FORM)
     ! This may be "g" for the dimensionless Gram-Charlier anharmonic
     ! constants, OR "u"  i.e. the constants are dimensioned in
     ! Angstrom^n, where n=3,4 is the order of the constants. This is
     ! like the second order cartesian ADPs, but is non-standard,
     ! see Trueblood (1996) Acta Cryst A 52 p. 770-81 (34)-(37), but
     ! in that paper "h" is our k/2\pi.

     ! =======
     ! Charges
     ! =======
     REAL :: charge DEFAULT(ZERO)
     ! The charge on the atom
     VEC(REAL,3) :: dipole DEFAULT(ZERO)
     ! The dipole on the atom
     MAT(REAL,3,3) :: quadrupole DEFAULT(ZERO)
     ! The quadrupole on the atom
     REAL :: spin DEFAULT(ZERO)
     ! The value of unpaired spin on this atom
     VEC(REAL,3) :: spin_dipole DEFAULT(ZERO)
     ! The Sz spin dipole on the atom
     MAT(REAL,3,3) :: spin_quadrupole DEFAULT(ZERO)
     ! The Sz spin quadrupole on the atom
     VEC(REAL,3) :: charge_polarisability DEFAULT(ZERO)
     ! The charge polarisability on the atom
     MAT(REAL,3,3) :: dipole_polarisability DEFAULT(ZERO)
     ! The dipole polarisability on the atom; field is 2nd component.

     ! ========
     ! QM bases
     ! ========
     STR(len=STR_SIZE) :: basis_label DEFAULT(" ")
     ! The label of the basis set to match to. e.g. when looking in
     ! the library
     STR(len=STR_SIZE) :: basis_kind DEFAULT(" ")
     ! The kind of basis set ("gaussian", "slater" etc.) to use in
     ! calculations. Note that there may be more than one type of
     ! basis present for an atom.
     type(BASIS_TYPE), allocatable :: basis
     ! The basis for the atom
     STR(len=STR_SIZE) :: slaterbasis_label DEFAULT(" ")
     ! The label of the slater basis set to match to.
     type(SLATERBASIS_TYPE), allocatable :: slaterbasis
     ! The Slater function basis set for the atom. This includes
     ! occupancies and can be used for Hirshfeld surface plots and
     ! sum-of-spherical atoms densities.
     STR(len=STR_SIZE) :: coppensbasis_label DEFAULT(" ")
     ! The label of the coppens basis set to match to.
     type(COPPENSBASIS_TYPE), allocatable :: coppensbasis
     ! The Coppens-style fitted relativistic orbital basis for the
     ! atom. This is used for Hirshfeld surface plots and
     ! sum-of-spherical atoms densities.
     INT :: kind DEFAULT(0)
     ! The atom kind index ... atoms have the same kind if they have
     ! the same atomic number and the same basis sets.
     ! NOTE: this is used to test for finalization.
     BIN :: is_unique_kind DEFAULT(FALSE)
     ! Set true if this atom and its basis are unique.
     ! This is used for setting ANO data and bases.

     ! =======================
     ! ANO/spherical atom info
     ! =======================
     REAL :: energy
     ! The energy of the isolated atom
     type(OPMATRIX_TYPE), allocatable :: AOs
     ! The atomic orbitals for the atom
     type(OPMATRIX_TYPE), allocatable :: density_mx
     ! The density matrix for the atom
     type(OPMATRIX_TYPE), allocatable :: NOs
     ! The natural orbitals for the atom
     type(OPVECTOR_TYPE), allocatable :: NO_occ_nos
     ! The occupation numbers for the atom
     BIN :: atom_data_made DEFAULT(FALSE)
     ! Set TRUE if the ANO info has been made.  Used to destroy the
     ! unique atom info.
     MAT(REAL,:,:), allocatable :: NAOs
     ! The natural atomic orbitals for the atom
     VEC(REAL,:), allocatable :: NAO_occupations
     ! The NAO occupation numbers for the atom

     ! =============
     ! Interpolators
     ! =============
     BIN :: use_interpolator DEFAULT(TRUE)
     ! Whether to use interpolators for spherical densities
     type(INTERPOLATOR_TYPE), allocatable :: interpolator
     ! An interpolator object, used for calculating the atomic
     ! density at a certain radius. This is for gaussian bases only.

     ! =====================
     ! IAM structure factors
     ! =====================
     BIN :: use_IAM_ITC_FFs DEFAULT(TRUE)
     ! For IAM refinements only.
     ! If TRUE, use International Tables C neutral atom form factors.
     ! H atoms use Stewart-Davidson-Simpson (SDS) H form factors.
     ! If FALSE, use the gaussian atomic basis set.
     REAL :: f_r DEFAULT(ZERO)
     ! Real part of the anomalous scattering factor, f'
     REAL :: f_i DEFAULT(ZERO)
     ! Imaginary part of the anomalous scattering factor, f''

     ! ==========
     ! Shell data
     ! ==========
     VEC(INT,:), allocatable :: first_basis_fn_for_shell
     ! First basis function for a given shell
     VEC(INT,:), allocatable :: last_basis_fn_for_shell
     ! Last basis function for a given shell
     VEC(INT,:), allocatable :: angular_moment_for_shell
     ! Angular moment for a given shell
     MAT(type(SHELL1PAIR_TYPE),:,:), allocatable :: basis_shell1pair
     ! Precomputed stuff for shellpairs of the basis set, to speed up
     ! later calculations of gaussian integrals.

     ! =========
     ! Invarioms
     ! =========
     STR(len=STR_SIZE) :: invariom_label DEFAULT(" ")
     ! The label for the invariom

     ! ========
     ! Proteins
     ! ========

     ! The following names are very similar to the IUCr dictionary
     ! Blame them for silly names.
     INT :: atom_sequence_id DEFAULT(0)
     ! Atom sequence number in the protein. The dictionary says it is
     ! not necessary to be an integer but we make it so here.
     STR(len=STR_SIZE) :: compound_id DEFAULT(" ")
     ! Compound i.e. residue code that atom belongs to.
     INT :: compound_sequence_id DEFAULT(0)
     ! Sequential number for the compound i.e. residue that the atom belongs to.
     STR(len=STR_SIZE) :: asym_id DEFAULT(" ")
     ! Protein strand label that this atom belongs to.
     STR(len=STR_SIZE) :: entity_id DEFAULT(" ")
     ! Code for the entity that the atom & residue belong to
     BIN :: is_cap_atom DEFAULT(FALSE)
     ! Set true if this is a cap atom to terminate a fragment, so that
     ! structure factors are not calculated. NOTE: some cap atoms are
     ! in the backbond and overlap with other fragments.

     ! ==
     ! MM
     ! ==
     STR(len=STR_SIZE) :: mm_forcefield_name DEFAULT(" ")
     ! The name of the forcefield (e.g. amber, sybyl,...)
     STR(len=STR_SIZE) :: mm_atom_type DEFAULT(" ")
     ! The atom type, used to define the force field potential
     REAL :: mm_charge DEFAULT(ZERO)
     ! The atomic charge (relative to the force field!)
     VEC(REAL,3) :: restraining_position DEFAULT(ZERO)
     ! A position used for restrained geometry optimization
     REAL :: restraining_force_constant DEFAULT(ZERO)
     ! A force constant used for restrained geometry optimisations
   end type

!  ===========
!  Atom groups
!  ===========
   type ATOM_GROUP_TYPE
     STR(len=STR_SIZE) :: name DEFAULT(" ")
     ! The name given to the group
     INT :: n_atoms DEFAULT(0)
     ! The number of atoms in the group
     VEC(INT,:), allocatable :: atom_index
     ! The list of atom indices in the group
     ! Non-zero value indicates user set this value.
     INT :: charge DEFAULT(0)
     ! The list of charges on each group
     INT :: spin_multiplicity DEFAULT(1)
     ! Spin multiplicity of the atom group.
     BIN :: spin_multiplicity_set DEFAULT(FALSE)
     ! Set TRUE if the user entered a spin_multiplicity.
     ! Used to override the default.
     STR(len=PATH_SIZE) :: MO_file_name DEFAULT(" ")
     ! Name of the MO file for the group
     STR(len=PATH_SIZE) :: FChk_file_name DEFAULT(" ")
     ! Name of the gaussian .FChk file for molecule data
     STR(len=PATH_SIZE) :: molden_file_name DEFAULT(" ")
     ! Name of the .molden file for MO or NO data
     BIN :: transform_group DEFAULT(FALSE)
     ! Whether to symmetry-transform (rotate, translate) the group.
     ! fchk file groups are always rotated.
     MAT(REAL,3,3) :: rotation_mx DEFAULT(ZERO)
     ! Rotation matrix for ... ?
     VEC(REAL,3) :: shift DEFAULT(ZERO)
     ! Shift vector ... for ?
     INT :: n_MOs DEFAULT(0)
     ! The number of MO's to use when constructing the density matrix
     ! for the group. Only the lowest MO's up to this number are used.
     VEC(type(ATOM_TYPE),:), allocatable :: atom
     ! The list of atoms from which groups are defined, where
     ! atom_index(:) points to. *Pointer assigned*.

   ! mol :: MOLECULE*   DEFAULT_NULL
   ! ! The current submolecule
   end type

!  ===========
!  Atomic data
!  ===========

!   type SASAKI_FORM_FACTORS
!
!     Z_list :: VEC{INT}@
!     ! The list of atomic numbers for which form factors are required
!
!     wavelength :: REAL, readonly  DEFAULT(-ONE)
!     ! The wavelength for the data, in Angstrom
!
!     fr :: VEC{REAL}@
!     ! The real part of the anomalous form factor, f'
!
!     fi :: VEC{REAL}@
!     ! The imahinary part of the anomalous form factor, f''
!
!     mu_on_rho :: VEC{REAL}@
!     ! Mass photoelectric attenuation coefficient, units unknown
!
!     sigma_on_rho :: VEC{REAL}@
!     ! Total photon scatterin cross section, units unknown
!
!     finalized :: BIN  DEFAULT(FALSE)
!     ! Whetherthe object is finalised
!
!
!   end

!  ===================
!  DFT and Becke grids
!  ===================
   type DFT_FUNCTIONAL_TYPE
     STR(len=STR_SIZE) :: name DEFAULT(" ")
     ! The name of the functional to use
     REAL :: Xalpha DEFAULT(DFT_FUNCTIONAL_XALPHA)
     ! The valueof the alpha parameter in the Xalpha functional
     REAL :: rho_cutoff DEFAULT(DFT_FUNCTIONAL_RHO_CUTOFF)
     ! The value of the density below which the functional and the potential are
     ! set to zero.
   end type
   type XC_DENSITY_TYPE
     INT :: n_pt DEFAULT(0)
     ! The number of grid points
     INT :: n_spin DEFAULT(1)
     ! 1 for the total density, 2 for alpha and beta densities
     BIN :: has_gradient DEFAULT(FALSE)
     ! TRUE if the density gradient is held
     BIN :: has_tau DEFAULT(FALSE)
     ! TRUE if the kinetic energy density is held
     MAT(REAL,:,:), allocatable :: rho
     ! The density, (n_pt,n_spin)
     MAT3(REAL,:,:,:), allocatable :: grad
     ! The density gradient, (n_pt,3,n_spin)
     MAT(REAL,:,:), allocatable :: tau
     ! The kinetic energy density, (n_pt,n_spin)
   end type
   type XC_POTENTIAL_TYPE
     VEC(REAL,:), allocatable :: energy
     ! The energy per particle, (n_pt)
     MAT(REAL,:,:), allocatable :: v_rho
     ! The functional derivative with respect to the density, (n_pt,n_spin)
     MAT3(REAL,:,:,:), allocatable :: v_grad
     ! The gradient part of the potential, (n_pt,3,n_spin)
     MAT(REAL,:,:), allocatable :: v_tau
     ! The functional derivative with respect to the kinetic energy
     ! density, (n_pt,n_spin)
   end type
   type BECKE_GRID_TYPE
     STR(len=STR_SIZE) :: kind DEFAULT("mura_knowles")
     ! The exact type of Becke grid to use
     VEC(type(ATOM_TYPE),:), allocatable :: atom
     ! List of atoms for which DFT integration grids are to be made.
     ! If associated, used to generate the atom basis function (and
     ! nabla) grids for gaussian DFT calculations. *Pointer copied*.
     VEC(REAL,:), allocatable :: atom_zeta
     ! List of scaling factors used in the transformation to the
     ! radial integration interval.
     VEC(REAL,:), allocatable :: atom_min_distance
     ! For every atom, the minimum distance to the nearest atom.
     VEC(type(EVEC_INT_TYPE),:), allocatable :: Salvador_bonds_for
     ! The Salvador & Ramos-Cordoba (2013) neighbour atoms.
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: Salvador_radii_for
     ! The Salvador & Ramos-Cordoba (2013) radii for the neighbours of
     ! every atom. See JCP 139 p. 071103.
     MAT(REAL,:,:), allocatable :: Salvador_chi
     ! The Salvador & Ramos-Cordoba (2013) ratio of radii, otherwise
     ! known as "chi".
     INT :: n_unique_atoms
     ! No. of atoms of unique kind. An atom is the same as another if
     ! it has the same nuclear charge.
     VEC(INT,:), allocatable :: unique_atom
     ! unique_atom(k) is the index of the "k"-th unique atom.
     VEC(INT,:), allocatable :: unique_atom_for
     ! unique_atom_for(a) is the index of the unique atom equivalent to "a".
     REAL :: basis_fn_cutoff DEFAULT(SHELL_BASIS_FN_CUTOFF)
     ! Below this cutoff, *atomic* grid points whose corresponding
     ! basis function values (which are to be integrated) are assumed
     ! to be zero. In combination with the .atom_lowest_exponents this
     ! cutoff is used to prune *atomic* grids of unnecessary points.
     REAL :: basis_fn_pair_cutoff DEFAULT(SHELL_BASIS_FN_CUTOFF)
     ! *Atom pair* grid points where the *product* of the two basis
     ! function values is smaller than this cutoff are assumed to be
     ! zero. In combination with the .atom_lowest_exponents this
     ! cutoff is used to prune *atom pair* grids of unnecessary
     ! points.
     REAL :: prune_rho_cutoff DEFAULT(TOL(12))
     ! Drop DFT grid points where the promolecule density is below this.
     ! Zero keeps every point.
     INT :: XC_batch_max_pts DEFAULT(512)
     ! The most points in one batch of the molecular DFT grid.
     REAL :: XC_batch_box_edge DEFAULT(THREE)
     ! The edge, in bohr, of the cubes the molecular DFT grid is first cut into.
     REAL :: partition_cutoff DEFAULT(TOL(6))
     ! Below this cutoff, basis function values which are to be
     ! integrated are assumed to be zero. In combination with the
     ! .atom_lowest_exponents
     REAL :: rho_cutoff DEFAULT(TOL(10))
     ! The value of the density below which the functional and the
     ! potential are set to zero on this grid. This variable is passed
     ! to the DFT_FUNCTIONAL object (see MOLECULE.FOCK, which copies it
     ! onto the functional and so overrides that object's own, much
     ! smaller, DFT_FUNCTIONAL_RHO_CUTOFF default). It is also used to
     ! eliminate density evaluation at grid points, and it determines
     ! the overlapping_atoms array.
     !
     ! WAS TOL(6), AND THAT WAS A SYSTEMATIC ERROR, not a safety margin.
     ! Measured 2026-08-13 against g09 (BLYP/cc-pVDZ water, Cartesian d,
     ! both grid-converged): TOL(6) put Tonto 1.03e-5 Hartree away from
     ! g09; TOL(10) puts it 3.5e-8 away. A factor of 300, and the timing
     ! showed NO cost at any grid accuracy -- the cutoff was never buying
     ! speed, only losing accuracy, because the expensive work (basis
     ! functions and density on the grid, then the matrix contraction)
     ! happens whether or not a point is later discarded.
     !
     ! WHY IT BIT SO HARD, and why this gets WORSE further up Jacob's
     ! ladder. A cutoff on rho is a proxy for "this point cannot matter",
     ! and it is only a good proxy for LDA. In an exponential tail every
     ! derivative of rho is proportional to rho, but the reduced variables
     ! divide by a POWER of rho: Becke's x = |grad rho|/rho^(4/3) behaves
     ! as rho^(-1/3), so it GROWS exactly where the cutoff truncates. The
     ! LDA integrand dies as rho^(4/3); the B88 gradient correction decays
     ! only as rho/ln(1/rho) -- a factor rho^(-1/3), i.e. 100x more tail
     ! weight at 1e-6. That is the whole of the discrepancy above, and it
     ! is why LDA (slater) agreed with g09 to 5e-7 while B88 did not.
     !
     ! Each further derivative order costs another rho^(-1/3): a meta-GGA
     ! variable such as tau/tau_unif goes as rho^(-2/3). So if meta-GGAs
     ! are ever added, this default must be RE-MEASURED, not inherited --
     ! it could be ~100x more sensitive again. The real lesson is that
     ! the quantity which should be small before a point is discarded is
     ! its CONTRIBUTION TO THE INTEGRAL, not the density.
     !
     ! Full study, with the g09 cross-validation that isolated it (HF
     ! agrees to 1.2e-10, slater to 4.6e-7, B88 to only 9.9e-6):
     ! docs/TASK_DFT_STANDARDISATION.md section 3.
     !
     ! NOTE .partition_cutoff and .basis_fn_cutoff above prune by basis
     ! function rather than by density and were held fixed throughout
     ! that study. They may carry a bias of their own; unmeasured.
     INT :: n_radial_pts DEFAULT(20)
     ! No. of radial integration points desired for the H atom. The
     ! number of points used for other atoms is normally 5 more for
     ! every extra shell, according to Becke's original rule of thumb.
     INT :: n_extra_pts_per_shell DEFAULT(5)
     ! The number of extra points to use per n-shell over and above
     ! the default number .n_radial_points for the Hydrogen atom.
     STR(len=STR_SIZE) :: accuracy DEFAULT("medium")
     ! A keyword describing the desired accuracy of the integration
     ! grid. "medium" with the Becke partition is 5e-8 from a converged
     ! reference on water; see docs/TASK_DFT_STANDARDISATION.md section 6b.
     INT :: l_angular_grid DEFAULT(0)
     ! Angular momentum which is to be integrated exactly by the
     ! angular grid.
     INT :: l_H_angular_grid DEFAULT(0)
     ! Angular momentum which is to be integrated exactly by the
     ! angular grid for the Hydrogen/Helium atoms only. For other
     ! atoms .l_angular_grid is used.
     INT :: l_bonding_angular_grid DEFAULT(0)
     ! Lebedev order in the bonding zone of non-H atoms; "adaptive" pruning only.
     STR(len=STR_SIZE) :: pruning_scheme DEFAULT("treutler_ahlrichs")
     ! The method to reduce the angular grids: "none", "treutler_ahlrichs",
     ! "jayatilaka0", "jayatilaka1" or "adaptive".

   ! reduce_core_angular_grid :: BIN  DEFAULT(TRUE)
   ! ! If set TRUE the order of the angular grid near the core of the atom is
   ! ! reduced to make smaller meshes. So far the scheme of Treutler and Ahlrichs
   ! ! is used.
     BIN :: reduce_H_angular_grid DEFAULT(TRUE)
     ! If set TRUE the order of the angular grid for Hydrogen and
     ! Helium are reduced to .l_H_angular_grid.
     BIN :: scale_atomic_grids DEFAULT(TRUE)
     ! If set TRUE, the atomic grids are scaled according to the
     ! authors recommended scaling factors. This does *not* mean that
     ! the partition function is also scaled to account for varying
     ! atomic sizes.
     INT :: partition_power DEFAULT(3)
     ! Used in smoothing the partition boundary
     REAL :: stratmann_scuseria_A DEFAULT(0.64d0)
     ! The Stratmann-Scuseria "a" parameter, used in defining their
     ! piecewise partition function. See CPL 257 p. 213 (1996).
     VEC(INT,:), allocatable :: atom_n1_SS
     ! The number of unit-weight SS grid weights, for every atom
     STR(len=STR_SIZE) :: partition_scheme DEFAULT("becke")
     ! The scheme which partitions a density-like function into atomic
     ! contributions: "becke" (smoothed Voronoi cells, the default) or
     ! "stratmann_scuseria" (a sharper cell function with screening,
     ! cheaper on large systems but ten times less accurate on the same
     ! grid). "delley" was removed 2026-09-11: it did not converge.
     STR(len=STR_SIZE) :: partition_scaling_scheme DEFAULT("becke")
     ! The atomic size adjustment for a "becke" partition: "becke" (the
     ! ratio of Bragg-Slater radii, with H doubled), "treutler_ahlrichs"
     ! (the square root of that ratio) or "none". Stratmann-Scuseria
     ! takes "none".
     BIN :: finalized DEFAULT(FALSE)
     ! Set to true if the grid has been "set" using set_grid_data
     VEC(INT,:), allocatable :: n_pts_for_row
     ! The number of points for a given row of the periodic table, for
     ! the current grid settings.
     VEC(type(QUADRATURE_TYPE),:), allocatable :: radial_grid
     ! Radial quadrature points, for every row of the periodic table.
     type(LEBEDEV_TYPE), allocatable :: lebedev_grid
     ! The default angular quadrature points.
     type(LEBEDEV_TYPE), allocatable :: lebedev_H_grid
     ! The angular quadrature points for the Hydrogen atom.
     type(LEBEDEV_TYPE), allocatable :: lebedev_L3_grid
     ! Angular quadrature points for L=3, used close to the atom.
     type(LEBEDEV_TYPE), allocatable :: lebedev_L5_grid
     ! Angular quadrature points for L=5, used close to the atom.
     type(LEBEDEV_TYPE), allocatable :: lebedev_L7_grid
     ! Angular quadrature points for L=7, used close to the atom.
     type(LEBEDEV_TYPE), allocatable :: lebedev_L9_grid
     ! Angular quadrature points for L=9, used close to the atom.
     type(LEBEDEV_TYPE), allocatable :: lebedev_L11_grid
     ! Angular quadrature points for L=11, used close to the atom.
     INT :: n_unscaled_pts DEFAULT(0)
     ! No. of quadrature points in the reference atom.
     MAT(REAL,:,:), allocatable :: unscaled_pts
     ! The unscaled spherical quadrature grid points centered at the
     ! origin, for a reference atom.
     VEC(REAL,:), allocatable :: unscaled_wts
     ! The unscaled spherical quadrature grid weights for a reference
     ! atom.
     INT :: n_points DEFAULT(0)
     ! No. of points in quadrature grid for the whole molecule.
     VEC(type(EMAT_REAL_TYPE),:), allocatable :: atom_pts
     ! The complete list of grid points for the unique atoms
     ! assuming the atom is centered at the origin
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: atom_wts
     ! The complete list of grid weights for the unique atoms in
     ! .atom_Z.
     VEC(type(EMAT_REAL_TYPE),:), allocatable :: atom_grid0
     ! List of atomic density grids for the unique atoms
     VEC(type(EMAT3_REAL_TYPE),:), allocatable :: atom_grid1
     ! List of atomic gradient density grids for the unique atoms
     VEC(BIN,:), allocatable :: wt_is_0
     ! Set to TRUE if the weight for a particular point is zero. Used
     ! to eliminate grid points from product grids.
   end type

!  ========
!  Symmetry
!  ========
   type IRREP_TYPE
     STR(len=4) :: label DEFAULT("?")
     ! Irrep label
     INT :: dim DEFAULT(0)
     ! Irrep dimension
     VEC(REAL,:), allocatable :: chi
     ! Characters for the irrep
     MAT3(REAL,:,:,:), allocatable :: mx
     ! Representation matrices for the irrep
   end type
   type POINTGROUP_TYPE
     STR(len=4) :: symbol DEFAULT(" ")
     ! symmetry symbol
     STR(len=4) :: ID_symbol DEFAULT(" ")
     ! group id symbol
     INT :: ID_number DEFAULT(0)
     ! group id number
     INT :: axis_order DEFAULT(0)
     ! principal axis order
     INT :: order DEFAULT(0)
     ! order of the group
     INT :: n_irrep DEFAULT(0)
     ! no of irreducible representations
     INT :: n_gen DEFAULT(0)
     ! No. of generators
     BIN :: has_complex_irreps DEFAULT(FALSE)
     ! true if the group has complex irreps
     MAT(INT,:,:), allocatable :: table
     ! group multiplication table
     MAT3(REAL,:,:,:), allocatable :: mx
     ! 3x3 representation matrices
     BIN :: use_spherical_basis DEFAULT(FALSE)
     ! Set true if you want to use spherical harmonic basis set
     ! Necessary for the correct rep matrices
     MAT3(REAL,:,:,:), allocatable :: ptr
     ! 3x3 representation matrices for p functions, same as mat
     MAT3(REAL,:,:,:), allocatable :: dtr
     ! 6x6 representation matrices for d functions
     MAT3(REAL,:,:,:), allocatable :: ftr
     ! 10x10 representation matrices for f functions
     MAT3(REAL,:,:,:), allocatable :: gtr
     ! 15x15 representation matrices for g functions
     VEC(INT,:), allocatable :: inverse
     ! Indices of inverse group elements
     VEC(type(IRREP_TYPE),:), allocatable :: irrep
     ! List of irrrducible representations
   end type
   type SPACEGROUP_TYPE

     ! ==========
     ! Input data
     ! ==========
     STR(len=STR_SIZE) :: IT_symbol DEFAULT("?")
     ! International Table (Hermann-Maguin) symmetry symbol
     INT :: IT_group_number DEFAULT(0)
     ! International tables group number
     STR(len=STR_SIZE) :: Hall_symbol DEFAULT("?")
     ! Hall notation symbol
     STR(len=STR_SIZE) :: HM_symbol DEFAULT("?")
     ! Hermann-Mauguin notation symbol
     STR(len=STR_SIZE) :: Schoenflies_symbol DEFAULT("?")
     ! Hermann-Mauguin notation symbol

     ! ============
     ! Parsed input
     ! ============
     STR(len=1) :: lattice_symbol DEFAULT("?")
     ! Hall lattice symmetry symbol
     INT :: lattice_symbol_index DEFAULT(0)
     ! Hall lattice symbol index number
     STR(len=STR_SIZE) :: lattice_type DEFAULT("?")
     ! Lattice type e.g. "cubic"
     BIN :: centrosymmetric DEFAULT(FALSE)
     ! True if center of symmetry present
     VEC(INT,4) :: axis_order DEFAULT(0)
     ! Order of each symop generator
     VEC(STR(len=1),4) :: axis_direction_symbol DEFAULT(" ")
     ! Superscript rotation axis direction symbol for each generator
     VEC(INT,4) :: axis_direction_index DEFAULT(0)
     ! Indicates the numberical index corresponding to the rotation
     ! axis symbol for each generator
     VEC(BIN,4) :: has_axis_bar DEFAULT(FALSE)
     ! True if bar for this generator
     MAT(STR(len=1),3,4) :: translation_symbol DEFAULT(" ")
     ! Subscript translation symbols for each generator
     VEC(INT,3) :: origin_shift DEFAULT(0)
     ! Subscript translation symbols for each axis

     ! =============
     ! Analyzed data
     ! =============
     INT :: nL DEFAULT(0)
     ! No. of translational lattice generators
     INT :: nR DEFAULT(0)
     ! No. of rotation generators
     INT :: nG DEFAULT(0)
     ! No. of generators
     INT :: n_seitz DEFAULT(0)
     ! No. of Seitz matrices
     MAT3(REAL,:,:,:), allocatable :: seitz
     ! The spacegroup Seitz matrices in the fractional crystal system.
     MAT(INT,:,:), allocatable :: product_of_symops
     ! The spacegroup multiplication table; product_of_symop(a,b)
     ! corresponds to matmul(seitz(:,:,a),seitz(:,:,b))
     VEC(INT,:), allocatable :: inverse_of_symops
     ! The inverse of spacegroup symops
     INT :: n_inverted_symops DEFAULT(0)
     ! Number of symmetry operations related by inversion
     VEC(INT,:), allocatable :: inverted_symop
     ! inverted_symop(i) is the index of a symop strictly less than i,
     ! which is related to symop i by an inversion.  Used to save work
     ! in SF calculations.
     VEC(INT,:), allocatable :: translated_symop
     ! translated_symop(i) is the index of a symop, less than or
     ! equal to i, which is related to symop i by translation but
     ! *not* inversion.  This array is used to save work in SF calculations.
     INT :: n_unique_SF_symops DEFAULT(0)
     ! Number of unique symmetry operations not related by translation or
     ! inversion, T&I. This is used to save work in structure factor (SF)
     ! calculations.
     VEC(INT,:), allocatable :: unique_SF_symop
     ! Indices of the unique symops not related by inversion or translation.
     ! This is used to save work in structure factor (SF) calculations.
     BIN :: analyzed DEFAULT(FALSE)
     ! Set TRUE if the spacegroup symbol has been succesfully analyzed
   end type

!  ============================
!  CSFs and Salter determinants
!  ============================
   type CSF_TO_SD_TYPE
     INT :: n_e DEFAULT(0)
     ! No. of electrons
     INT :: n_up DEFAULT(0)
     ! No. of up-couplings
     MAT(INT,:,:), allocatable :: CSF_graph
     ! For getting lexical order of the CSFs
     MAT(INT,:,:), allocatable :: SD_graph
     ! For getting lexical order of the SDs
   end type

!  ===========
!  Diffraction
!  ===========
   type REFLECTION_TYPE
     INT :: h1 DEFAULT(0)
     ! Miller h or h1 index for the reflection
     INT :: h2 DEFAULT(0)
     ! Miller k or h2 index for the reflection
     INT :: h3 DEFAULT(0)
     ! Miller l or h3 index for the reflection
     REAL :: d DEFAULT(ZERO)
     ! The interplanar spacing (in Bohr)
     REAL :: stl DEFAULT(ZERO)
     ! Sin (theta)/lambda (in Bohr)
     REAL :: theta DEFAULT(ZERO)
     ! The Bragg angle theta
     INT :: equivalence_factor DEFAULT(1)
     ! The number of symmetry-equivalent reflections
     BIN :: F_exp_set DEFAULT(FALSE)
     ! Whether F_exp & F_sigma are inputted
     REAL :: F_exp DEFAULT(ZERO)
     ! Experimental structure factor
     REAL :: F_corr DEFAULT(ZERO)
     ! Experimental structure factor corrected for anomalous
     ! dispersion. *These are used in the LSQ*, not F_exp.
     ! (Although, without removal of dispersion from F_exp
     ! it will be the same as F_exp).
     REAL :: F_sigma DEFAULT(ZERO)
     ! Experimental error in the structure factor
     CPX :: F_calc DEFAULT(ZERO)
     ! Calculated complex structure factor on the absolute scale
     ! without extinction or scale factor *with* dispersion *if used*
     CPX :: F_disp DEFAULT(ZERO)
     ! The anomalous dispersion correction, if stored.
     REAL :: F_pred DEFAULT(ZERO)
     ! Calculated structure factor including scale and extinction corrections
     ! Predicted structure factor, which is |F_calc| times the scale
     ! and extinction corrections, if any. In crystallography, the
     ! tradition is to scale and correct the calculated quantities to
     ! match the experimental data, which are usually not measured on
     ! an absolute scale.
     BIN :: I_exp_set DEFAULT(FALSE)
     ! Whether I_exp & I_sigma are inputted
     REAL :: I_exp DEFAULT(ZERO)
     ! Experimental intensity
     REAL :: I_sigma DEFAULT(ZERO)
     ! Experimental error in the intensity
     REAL :: I_calc DEFAULT(ZERO)
     ! Calculated intensity (F_pred==2) without scale and extinction corrections
     REAL :: I_pred DEFAULT(ZERO)
     ! Calculated intensity (F_pred==2) including scale and extinction corrections
     INT :: group DEFAULT(0)
     ! Specifies the group that this reflection belongs to. Reflections in
     ! different groups may get treated differently e.g. be optimised with
     ! different scale factors.
   end type
   type LEAST_SQUARES_TYPE

     ! The refined parameters p of a least-squares fit and how they make the
     ! model vector X = X_offset + jacobian p. See least_squares.foo.
     INT :: n_X DEFAULT(0)
     ! Length of the model vector X
     INT :: n_param DEFAULT(0)
     ! Number of refined parameters p
     MAT(REAL,:,:), allocatable :: jacobian
     ! J = dX/dp, n_X x n_param; absent means p is X itself
     VEC(REAL,:), allocatable :: X_offset
     ! The part of X the parameters do not move
     VEC(REAL,:), allocatable :: param
     ! The refined parameters p
     VEC(STR(len=STR_SIZE),:), allocatable :: param_label
     ! Their labels
     VEC(REAL,:), allocatable :: param_esd
     ! Their esds, from the diagonal of param_covariance
     MAT(REAL,:,:), allocatable :: param_covariance
     ! Their covariance C = (A+W)^-1, before scaling by GoF^2
     VEC(REAL,:), allocatable :: dp
     ! The last shift in p
     MAT(REAL,:,:), allocatable :: restraint_W
     ! Restraint weights W in chi^2 + (p - p0)^T W (p - p0); absent: none
     VEC(REAL,:), allocatable :: restraint_shift
     ! The restraint targets p0 minus p
     REAL :: n_param_eff DEFAULT(ZERO)
     ! Effective number of parameters, tr[(A+W)^-1 A]
     INT :: n_0 DEFAULT(0)
     ! Number of zero eigenvalues of the normal matrix
     INT :: near_0 DEFAULT(0)
     ! Number of near-zero eigenvalues filtered out
     VEC(REAL,:), allocatable :: near_0_evals
     ! Those eigenvalues
     MAT(REAL,:,:), allocatable :: near_0_evecs
     ! Their eigenvectors, as columns over p
     VEC(REAL,:), allocatable :: near_0_IPR
     ! Inverse participation ratio of each, 1/sum v_i^4: how many
     ! parameters the direction involves. See DIFFRACTION_DATA.near_0_tol.
     VEC(REAL,:), allocatable :: eigenvalues
     ! All eigenvalues of the last normal matrix
   end type
   type DIFFRACTION_DATA_TYPE
     STR(len=STR_SIZE) :: data_kind DEFAULT("x-ray")
     ! Tells whether this is X-ray, neutron, or whatever data
     STR(len=PATH_SIZE) :: fcf_file_name DEFAULT(" ")
     ! Name of the .fcf file containing the unique reflections which
     ! are to be read in.
     VEC(type(REFLECTION_TYPE),:), allocatable :: reflections
     ! The structure factor information. NOTE: this can stand for either X-ray or
     ! PND reflections depending on the case.
     VEC(type(REFLECTION_TYPE),:), allocatable :: reflection0
     ! The original input structure factors before pruning.
     VEC(type(EVEC_INT_TYPE),:), allocatable :: hkl_range
     ! The range of the Miller indices (h k l)
     INT :: n_data_sets DEFAULT(1)
     ! The number of experimental data sets corresponding to the input
     ! reflections, in the case of a multiple-data-set refinement.
     ! NOTE: it is assumed that a unique reflection occurs n_data_sets
     ! times, once for each data set. (I think --dylan)

     ! =================
     ! Experimental data
     ! =================
     REAL :: wavelength DEFAULT(-ONE)
     ! Experimental wavelength
     REAL :: temperature DEFAULT(ZERO)
     ! Experimental temperature, for NKA
     INT :: NKA_T_sequence_no DEFAULT(0)
     ! Sequence no. for NKA output file

     ! =================================
     ! Reflection data treatment options
     ! =================================
     BIN :: use_equivalents DEFAULT(FALSE)
     ! If TRUE, then equivalents are *not* pruned from the data.
     ! Normally equivalent reflections are pruned out.
     REAL :: F_sigma_noise DEFAULT(ZERO)
     ! The value of the "alpha" noise to apply to the F_exp and
     ! F_sigma in units of F_sigma. Noise is normally distributed.
     REAL :: F_sigma_cutoff DEFAULT(-TWO)
     ! Cutoff for F_exp/F_sigma, for prune_weak_reflections
     REAL :: I_sigma_cutoff DEFAULT(-FOUR)
     ! Cutoff for I_exp/I_sigma, for prune_weak_reflection data
     REAL :: F_z_cutoff DEFAULT(-ONE)
     ! Cutoff for F_z, for prune_bad_F_reflections
     REAL :: I_z_cutoff DEFAULT(-ONE)
     ! Cutoff for I_z, for prune_bad_I_reflections
     REAL :: F_calc_cutoff DEFAULT(TOL(3))
     ! Cutoff for F_calc, for prune_zero_F_calcs
     REAL :: F_pred_cutoff DEFAULT(-ONE)
     ! Cutoff for F_pred, for prune_zero_F_preds
     REAL :: I_pred_cutoff DEFAULT(-ONE)
     ! Cutoff for I_pred, for prune_zero_I_preds
     REAL :: stl_high_cutoff DEFAULT(-ONE)
     ! Cutoff for sin_theta_on_lamba, for prune_high_angle_reflections
     REAL :: stl_low_cutoff DEFAULT(-ONE)
     ! Cutoff for sin_theta_on_lamba, for prune_low_angle_reflections
     REAL :: stl_limit DEFAULT(-ONE)
     ! Sin-theta on lambda limit for generating (h k l) reflections
     INT :: max_prune_iterations DEFAULT(-1)
     ! If the refinement iteration does not exceed this maximum,
     ! reflection pruning will occur i.e. the last ref. iteration
     ! that pruning will occur.
     BIN :: show_rejects DEFAULT(FALSE)
     ! Set TRUE if you want to see rejected reflections when pruning
     BIN :: synthesize_sigma_I DEFAULT(FALSE)
     ! True if artificial sigma(I) errors are to be generated from poisson
     ! statistics and used in calculating agreement statistics
     REAL :: SF_mixing_parameter DEFAULT(ZERO)
     ! The SF mixing parameter which sets the proportion of the
     ! experimental structure factors added into the predicted
     ! structure factors. This may help convergence in constrained HF
     ! problems. Dangerous and not recommended.

     ! =======================================
     ! Structure factor calculation parameters
     ! =======================================
     STR(len=STR_SIZE) :: partition_model DEFAULT(" ")
     ! Model for partitioning the electron density of the fragment
     ! wavefunction into (possibly unique, possibly oversampled) bits

   ! temperature_factor_model :: STR, readonly   DEFAULT(" ")
   ! ! Thermal smearing model for the Fourier transform
     INT :: RI_l_max DEFAULT(7)
     ! Highest l of the fitting functions for the oc-ri partition model
     REAL :: RI_exponent_ratio DEFAULT(1.5d0)
     ! Ratio of successive exponents of the oc-ri fitting functions
     REAL :: expHAR_power DEFAULT(2d0)
     ! Power n of the free-atom densities in the oc-exphar weight
     REAL :: FF_fit_s_max DEFAULT(2d0)
     ! Range [0,s_max] of the Gaussian fit of spherical form factors, 1/Angstrom
     INT :: FF_fit_n_gaussians DEFAULT(4)
     ! Number of Gaussians in that fit
     INT :: FF_fit_n_points DEFAULT(201)
     ! Number of s points the form factor is fitted on
     STR(len=STR_SIZE) :: FF_fit_format DEFAULT("shelx")
     ! Format the fitted coefficients are written in: shelx or table
     BIN :: use_Voronoi_atoms DEFAULT(FALSE)
     ! Set TRUE if fuzzy Voronoi atoms (in the Stratmann-Scuseria
     ! scheme) are to be used for calculating the atomic form factors,
     ! instead of Hirshfeld atoms
     BIN :: use_KM_atoms DEFAULT(FALSE)
     ! Set TRUE if Koritsanszky-Michael atoms are to be used in the
     ! density in the refinement.
     BIN :: use_disk_SFs DEFAULT(FALSE)
     ! Set TRUE if writing the asphrical atomic form factors to disk
     ! to save memory.
     BIN :: use_text_SFs DEFAULT(FALSE)
     ! Set TRUE if the aspherical atomic on disk are in ASCII format.
     ! Setting this will set use_disk_SFs TRUE also.

     ! =====================
     ! IAM structure factors
     ! =====================
     BIN :: is_IAM_refinement DEFAULT(FALSE)
     ! Set TRUE if this is an IAM refinement.
     BIN :: use_IAM_ITC_FFs DEFAULT(TRUE)
     ! Set TRUE if using International Tables Vol. C IAM exponential
     ! fit to the atomic form factors. This only makes sense for IAM
     ! refinement. NOTE: for H, we use Stewart-Davidson-Simpson (SDS).
     ! If set FALSE the gaussian IAM promolecule is used - atomic
     ! densities will be calculated using the specified QM method.
     BIN :: use_zero_overlap DEFAULT(FALSE)
     ! Set TRUE if you want to have zero overlap density in the
     ! Koritsanszky-Michael density in the refinement.

     ! ====================
     ! Dittrich's invarioms
     ! ====================
     STR(len=STR_SIZE) :: invariom_database_directory DEFAULT(" ")
     ! Location of the invariom database
     BIN :: invariom_refinement DEFAULT(FALSE)
     ! Logical to switch on invariom refinement
     INT :: NN_level DEFAULT(1)
     ! The number of shells of nearest neighbours to include, by atom
     ! bonding, when making the fragment around each asymmetric unit
     ! atom of a periodic network solid. Everything beyond this shell
     ! is replaced by a capping hydrogen atom. Default is 1.

     ! ========================
     ! Experimental corrections
     ! ========================
     BIN :: refine_scale DEFAULT(TRUE)
     ! True if an overall scaler factor is to be optimised, for use in
     ! calculating F_pred
     REAL :: scale_factor DEFAULT(ONE)
     ! Scale factor to apply to the calculated structure factor
     ! magntidues |F_calc|. This places F_calc on a non-absolute
     ! scale, to match F_exp. Note that extinction corrections and
     ! anomalous dispersion corrections may also be applied.
     VEC(REAL,:), allocatable :: scale_factors
     ! A set of scale factors to apply to structure factors from
     ! different groups.
     REAL :: exp_scale_factor DEFAULT(-ONE)
     ! Fixed scale factor to apply to the experimental structure factors
     BIN :: refine_extinction DEFAULT(FALSE)
     ! True if extinction is to be optimised.
     REAL :: extinction_factor DEFAULT(ZERO)
     ! Secondary extinction factor. Uses the SHELXL empirical model.
     REAL :: extinction_factor_esd DEFAULT(-ONE)
     ! Error in the secondary extinction factor. Negative if unavailable.
     INT :: n_param_structure DEFAULT(0)
     ! No. of structural (pADP) fitting parameters, excluding the scale
     ! and extinction factors. See DIFFRACTION_DATA.SET:update_n_param.
     BIN :: add_dispersion_to_F_calc DEFAULT(FALSE)
     ! If TRUE dispersion is to be corrected, by adding the anomalous 
     ! contribution into the complex F_calc.
     BIN :: remove_dispersion_from_F_exp DEFAULT(FALSE)
     ! If TRUE dispersion is to be removed from F_exp during refinement.
     ! This method is used by XD, Shelx, ...
     BIN :: stop_after_dispersion DEFAULT(FALSE)
     ! Stop the program immediately after dispersion corrections
     ! arre applied to the SF's in the zeroth iteration of a
     ! refinement and printo out the results. For testing only.

     ! ====================
     ! Agreement statistics
     ! ====================
     INT :: n_param DEFAULT(0)
     ! No. of fitting parameters used.
     BIN :: n_param_manually_set DEFAULT(FALSE)
     ! If TRUE n_param was manually set e.g. by reading a CIF,
     ! or it was set after a refinement and is not to be changed
     INT :: n_obs DEFAULT(0)
     ! No. of observations *including* any pseudo observations
     ! associated with restraints like those for H U_iso.
     REAL :: GoF2 DEFAULT(0)
     ! The current iteration's value of GoF2

     ! =======================
     ! Refinement/fit switches
     ! =======================
     BIN :: fit_structure DEFAULT(FALSE)
     ! Set TRUE if a structure is being fitted
     BIN :: refine_structure DEFAULT(FALSE)
     ! Set TRUE if a structure is being refined (implies fitting)
     BIN :: refine_fragments DEFAULT(FALSE)
     BIN :: use_Ryde_capping DEFAULT(FALSE)
     ! Use Ulf Ryde's capping method to split and cap polypeptides
     ! into fragment to make wavefunction and atomic structure factor
     ! and HAR calculations for proteins quicker.
     ! Internal variable to know if we are doing a fragHAR
     BIN :: use_NN DEFAULT(FALSE)
     ! Use nearest-neighbour capping to make an atom-centred fragment
     ! for every asymmetric unit atom, so that a fragHAR can be done on
     ! a periodic network solid, where no molecule can be isolated.
     ! Internal variable to know if we are doing a fragHAR
     BIN :: use_NN_capping DEFAULT(TRUE)
     ! Set FALSE to leave the nearest-neighbour fragments uncapped,
     ! rather than replacing the outermost shell with hydrogen atoms.
     ! Only meaningful when "use_NN" is set.
     BIN :: refine_F DEFAULT(TRUE)
     ! Set TRUE if you want to refine F's *not* F2's
     BIN :: refine_positions_only DEFAULT(FALSE)
     ! Set TRUE if you want to refine *only* the coordinates
     BIN :: refine_ADPs_only DEFAULT(FALSE)
     ! Set TRUE if you want to refine *only* the ADP's
     BIN :: refine_H_positions DEFAULT(TRUE)
     ! Set TRUE if you want to refine *only* the hydrogen positions
     BIN :: refine_H_ADPs DEFAULT(TRUE)
     ! Set TRUE if you want to refine *all* hydrogen ADP's
     ! NOTE: this includes the H_U_iso case i.e. it includes
     ! just the isotropic displacement parameter.
     BIN :: refine_H_U_iso DEFAULT(FALSE)
     STR(len=STR_SIZE) :: ADP_model DEFAULT("free")
     ! "free": each atom's ADP refined; "tls": a rigid-body T, L, S model
     INT :: n_soft_modes DEFAULT(0)
     ! Number of soft internal modes whose amplitudes a TLS model refines
     REAL :: soft_mode_restraint DEFAULT(HALF)
     ! sigma of a soft-mode amplitude's restraint, as a fraction of its harmonic value
     BIN :: use_soft_mode_correlations DEFAULT(FALSE)
     ! Refine the soft modes' correlations with the rigid body and each other
     REAL :: correlation_restraint DEFAULT(HALF)
     ! sigma of a TLS correlation coefficient's restraint toward zero
     BIN :: use_positive_TLS_sigma DEFAULT(TRUE)
     ! Hold the TLS model's covariance positive semidefinite
     ! Set TRUE if you want to refine U_iso for *all* H atoms
     VEC(STR(len=STR_SIZE),:), allocatable :: refine_U_iso_for_atom
     ! List of unique atom tags for which U_iso is refined
     VEC(STR(len=STR_SIZE),:), allocatable :: refine_nothing_for_atom
     ! List of unique atom tags for which *nothing* (niether positions
     ! nor ADPs) is refined
     BIN :: refine_anharmonicity DEFAULT(FALSE)
     ! Set TRUE if at least some anharmonic parameters are refined
     VEC(STR(len=STR_SIZE),:), allocatable :: refine_3rd_order_for_atom
     ! List of unique atom tags for which third-order anharmonicity
     ! parameters are to be refined.
     VEC(STR(len=STR_SIZE),:), allocatable :: refine_4th_order_for_atom
     ! List of unique atom tags for which fourth-order anharmonicity
     ! parameters are to be refined.
     BIN :: refine_4th_order_only DEFAULT(FALSE)
     ! Set TRUE if you want to refine only 4th order
     MAT(INT,:,:), allocatable :: refine_atom_parameters
     ! NOT IMPLEMENTED YET ...
     ! The list of atom parameters which should be refined. This is an
     ! N x 9 list. The first nine columns hold 0's and 1's indicating
     ! whether certain parameters should be refined. The first three
     ! are for the coordinates of the atom and the last six are for
     ! the ADP's in the order xx, yy, zz, xy, xz, yz.
     BIN :: restart_HAR_with_promolecule DEFAULT(FALSE)
     ! If set TRUE this will restart the SCF procedure with
     ! promolecule guess, followed by the orginal choice of guess.
     ! This may stop convergence problems.
     BIN :: XCW DEFAULT(FALSE)
     ! Set TRUE if doing an XCW

     ! ====================================
     ! Refinement/fit iteration termination
     ! ====================================
     INT :: min_iterations DEFAULT(2)
     ! The minimum number of iterations for the normal equations
     INT :: max_iterations DEFAULT(100)
     ! The maximum number of iterations for the normal equations
     REAL :: max_allowed_param_shift DEFAULT(25.0)
     ! The maximum allowed shift when solving the normal equations:
     ! If any one of the parameters shifts obtained from solving the
     ! normal equations is greater than this value, they are all
     ! rescaled so that the largest is this value.

     ! =====================
     ! Least squares options
     ! =====================
     REAL :: tol_for_shift_on_esd DEFAULT(TOL(2))
     ! The convergence tolerance for the shift in a parameter
     ! divided by its esd

     ! =============================
     ! Refinement/fit output control
     ! =============================
     BIN :: show_refinement_output DEFAULT(TRUE)
     ! Show refinement output, including header, options and results.
     BIN :: show_refinement_results DEFAULT(TRUE)
     ! Show only refinement results statistics, but no options or parameters.
     BIN :: show_fit_output DEFAULT(FALSE)
     ! Show LS fit output, including header, options and results.
     ! NOTE: A fit is one cycle of LS with fixed form factors.
     BIN :: show_fit_results DEFAULT(FALSE)
     ! Show only LS fit results, but no header, options and results.
     ! NOTE: A fit is one cycle of LS with fixed form factors.
     BIN :: show_IAM_output DEFAULT(TRUE)
     ! Show IAM output, including header, options and results.
     ! NOTE: A fit is one cycle of LS with fixed form factors.
     BIN :: show_IAM_results DEFAULT(TRUE)
     ! Show only IAM results, but no header, options and results.
     ! NOTE: A fit is one cycle of LS with fixed form factors.
     BIN :: do_residual_cube DEFAULT(TRUE)
     ! Do residual density as a gaussian.cube after a fit/refinement.
     BIN :: do_minmax_atoms DEFAULT(FALSE)
     ! Print out atoms nearest to the positions of the minimum 
     ! and maximum in the residual density. These normally occur 
     ! between atoms, at high symmetry points, and so printing is
     ! supressed by default to produce consistent test results.
     BIN :: do_standard_plots DEFAULT(TRUE)
     ! Do standard plots after a fit/refinement
     BIN :: show_near_0_eigenvectors DEFAULT(FALSE)
     ! Whether to display the near zero eigenvectors
     BIN :: show_correlation_mx DEFAULT(FALSE)
     ! Whether to display the correlation matrix or not
     REAL :: min_correlation DEFAULT(0.60)
     ! The minimum value for correlation matrix elements before 
     ! they are printed out in the output.
     INT :: n_worst_reflections DEFAULT(-1)
     ! Number worst reflections to show; when set, is +ve.

     ! =================
     ! Fit output tables
     ! =================
     VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: fit_table
     ! The current fit table
     VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: ref_table
     ! The current refinement table

     ! ==================
     ! Fit iteration data
     ! ==================
     INT :: fit_iteration DEFAULT(0)
     ! The iteration counter for a rigid-atom structure fit
     REAL :: GoF2_fit DEFAULT(0)
     ! The fit-iteration's value of GoF2
     REAL :: GoF2_fit0 DEFAULT(0)
     ! The previous fit-iteration's value of GoF2
     REAL :: max_shift DEFAULT(ZERO)
     ! The shift for the parameter which had the maximum shift on ESD
     ! during a HAR least-squares cycle.
     REAL :: max_esd DEFAULT(ZERO)
     ! The ESD for the parameter which had the maximum shift on ESD.
     ! during a HAR least-squares cycle.
     REAL :: max_shift_on_esd DEFAULT(ZERO)
     ! The maximum shift in a parameter divided by its esd, during
     ! the current refinement step.
     INT :: max_shift_on_esd_ind DEFAULT(DIFFRACTION_DATA_INDMAX_INIT)
     ! The index of the maximum parameter shift on esd, during the
     ! current refinement step.
     STR(len=STR_SIZE) :: max_shift_on_esd_par DEFAULT(" ")
     ! The parameter label for the index of the maximum parameter
     ! shift on esd, during the current refinement step.
     BIN :: fit_finished DEFAULT(FALSE)
     ! TRUE if the fit has finished for a good or bad reason
     BIN :: fit_converged DEFAULT(FALSE)
     ! TRUE if the fit has converged
     BIN :: GoF2_increased DEFAULT(FALSE)
     ! TRUE if GoF2 increased
     BIN :: too_many_fit_iterations DEFAULT(FALSE)
     ! TRUE if the maximum fit iterations was exceeded

     ! =========================
     ! Refinement iteration data
     ! =========================
     INT :: ref_iteration DEFAULT(0)
     ! The iteration counter for a non-rigid atom structure refinement
     ! (includes multiple SCF/fit cycles)
     REAL :: GoF2_ref DEFAULT(0)
     ! For a given refine step, the final fit-iteration's value of GoF2
     REAL :: GoF2_ref0 DEFAULT(0)
     ! For a given refine step, the initial fit-iteration's value of GoF2
     REAL :: max_refshift DEFAULT(ZERO)
     ! The shift in the parameter which had the maximum shift on ESD.
     ! during a HAR refinement cycle.
     REAL :: max_refesd DEFAULT(ZERO)
     ! The ESD for the parameter which had the maximum shift on ESD
     ! during a HAR refinement cycle.
     REAL :: max_refshift_on_esd DEFAULT(ZERO)
     ! The maximum shift in a parameter divided by its esd, during
     ! the current refinement step.
     REAL :: ave_refshift_on_esd DEFAULT(ZERO)
     ! The *mean* *absolute* shift in a parameter divided by its esd,
     ! during the current refinement step.
     INT :: max_refshift_on_esd_ind DEFAULT(DIFFRACTION_DATA_INDMAX_INIT)
     ! The index of the maximum parameter shift on esd, during the
     ! current refinement step.
     STR(len=STR_SIZE) :: max_refshift_on_esd_par DEFAULT(" ")
     ! The parameter label for the index of the maximum parameter
     ! shift on esd, during the current refinement step.
     BIN :: refinement_finished DEFAULT(FALSE)
     ! TRUE if the refinement has finished for a good or bad reason.
     BIN :: refinement_converged DEFAULT(FALSE)
     ! TRUE if the refinement has converged
     BIN :: too_many_ref_iterations DEFAULT(FALSE)
     ! TRUE if the maximum ref iterations was exceeded
     REAL :: max_residual DEFAULT(ZERO)
     ! Maximum residual in the unit cell
     REAL :: min_residual DEFAULT(ZERO)
     ! Minimum residual in the unit cell
     REAL :: rms_residual DEFAULT(ZERO)
     ! RMS residual in the unit cell
     REAL :: max_atom_residual DEFAULT(ZERO)
     ! Maximum residual on at atom
     STR(len=STR_SIZE) :: max_atom_residual_tag DEFAULT(" ")
     ! Maximum residual on at atom
     REAL :: min_atom_residual DEFAULT(ZERO)
     ! Minimum residual on at atom
     STR(len=STR_SIZE) :: min_atom_residual_tag DEFAULT(" ")
     ! Minimum residual on at atom
     REAL :: rms_atom_residual DEFAULT(ZERO)
     ! RMS residual on at atom

     ! ===========================
     ! Parameter values and shifts
     ! ===========================
     VEC(REAL,:), allocatable :: dX
     ! These are are the parameter shifts
     VEC(REAL,:), allocatable :: X_fit
     ! These are are the refinement parameter values
     VEC(REAL,:), allocatable :: X_fit0
     ! These are are the previous fit-iteration's parameter values
     VEC(REAL,:), allocatable :: X_ref
     ! These are are the refinement parameter values
     VEC(REAL,:), allocatable :: X_ref0
     ! These are are the previous refine-iteration's parameter values

     ! ==========================================
     ! Near zero eigenvalues in the least-squares
     ! ==========================================
     REAL :: near_0_tol DEFAULT(TOL(3))
     ! The tolerance used to detect near zero eigenvalues in the
     ! normal equations
     type(LEAST_SQUARES_TYPE), allocatable :: ls
     ! The refined parameters, their Jacobian onto X_fit, restraints,
     ! covariance, and what the eigenvalue filter found

     ! ==========================================
     ! Errors and covariances in parameter values
     ! ==========================================
     VEC(REAL,:), allocatable :: esd
     !  Uncertainties in the parameters; alias for esd_fit or esd_ref
     MAT(REAL,:,:), allocatable :: covariance_mx
     ! The covariance matrix for the asymmetric-unit parameters i.e.
     ! the inverse of the LHS of the least squares matrix.
     MAT(REAL,:,:), allocatable :: correlation_mx
     ! The correlation coefficient matrix for the parameters
     MAT(REAL,:,:), allocatable :: fragment_covariance_mx
     ! The covariance matrix for the parameters in the full fragment
   end type

!  ===================
!  Crystals & clusters
!  ===================
   type UNIT_CELL_TYPE
     VEC(REAL,3) :: angle DEFAULT(HALF_PI)
     ! The cell angles  (in radians)
     VEC(REAL,3) :: angle_error DEFAULT(ZERO)
     ! The cell angle errors (in radians)
     VEC(REAL,3) :: length DEFAULT(ONE)
     ! The cell lengths (in bohr)
     VEC(REAL,3) :: length_error DEFAULT(ZERO)
     ! The cell length errors (in bohr)
     REAL :: volume DEFAULT(-ONE)
     ! The cell volumes (bohr^3)
     REAL :: volume_error DEFAULT(ZERO)
     ! The cell volume error (bohr^3)
     MAT(REAL,3,3) :: direct_mx DEFAULT(ZERO)
     ! Direct lattice cell matrix (bohr). The *columns* are vectors
     ! of the three cell axes a, b, c, in the cartesian system.
     MAT(REAL,3,3) :: inverse_mx DEFAULT(ZERO)
     ! Inverse direct lattice cell matrix (bohr^{-1}). The columns
     ! are the coordinates of a*, b*, c* in the cartesian system.
     MAT(REAL,3,3) :: reciprocal_mx DEFAULT(ZERO)
     ! Reciprocal lattice cell matrix (bohr^{-1}).
     MAT(REAL,3,3) :: direct_U_mx DEFAULT(ZERO)
     ! Converts ADPs from crystal to cartesian systems.
     MAT(REAL,3,3) :: reciprocal_U_mx DEFAULT(ZERO)
     ! Converts ADPs from cartesian to crystal systems.
     BIN :: info_made DEFAULT(FALSE)
     ! Set TRUE if all the above information is consistent.
   end type
   type CRYSTAL23_TYPE
     STR(len=PATH_SIZE) :: xml_file_name DEFAULT(" ")
     ! The name of the C23 XML file to read the density mx from
     type(UNIT_CELL_TYPE) :: Primitive_unit_cell
     ! The primitive unit cell from Crystal23's XML file.
     ! Upon this unit cell, the density matrix is defined.
     ! This unit cell is used for the electron density calculation.
     VEC(INT,:), allocatable :: xml_atom_for_cif_atom
     ! Contains the XML atom corresponding to the asymmetric unit
     ! atoms in the CIF file (since they may be different in some
     ! cases)
     VEC(INT,:), allocatable :: cif_atom_for_xml_atom
     ! Inverse of above array: The asymmetric unit CIF atom
     ! corresponding to the C23 unit atoms in the XML file.
     ! Equals zero if n ot assigned.
     INT :: n_bf DEFAULT(0)
     ! No. of basis functions in the unit cell
     INT :: n_g DEFAULT(0)
     ! No. of integer-vector direct lattic (IVDL) vectors
     MAT(INT,:,:), allocatable :: g_latt
     ! The lattice translational vectors "g" corresponding to each 
     ! overlap matrix or density matrix. Ask Kanghyun for info!
     MAT3(REAL,:,:,:), allocatable :: density_mx
     ! The segments of the density matrices separated the g unit cell
     ! vectors.

   ! overlap_mx :: MAT3{REAL}@
     ! The overlap matrix like above.
   end type
   type CELL_MAP_TYPE
     STR(len=STR_SIZE) :: kind DEFAULT("residual")
     ! Which density of the cell: "residual", "f_exp" or "f_calc"
     INT :: l_value DEFAULT(-1)
     ! For local-moment maps, the angular momentum l; -1 for the plain density
     INT :: m_value DEFAULT(-99)
     ! For local-moment maps, one component m; -99 for the norm over m
     REAL :: window_width DEFAULT(0.25*BOHR_PER_ANGSTROM)
     ! The width of the Gaussian window of a local-moment map
     REAL :: sharpen_U DEFAULT(ZERO)
     ! A mean ADP divided out of a local-moment map; must be below window_width^2
     INT :: sharpen_order DEFAULT(0)
     ! The order of the Taylor polynomial replacing the exact sharpening factor; 0 is exact
     INT :: laplacian_order DEFAULT(0)
     ! Apply (-sigma^2 q^2)^k to a local-moment map: its k-th Laplacian, scaled by sigma^2k
     BIN :: wiener_weight DEFAULT(FALSE)
     ! Weight each observed coefficient by F^2/(F^2 + sigma_F^2)
     INT :: l_max DEFAULT(2)
     ! The highest l of the radial functions about a centre
     VEC(REAL,3) :: centre DEFAULT(ZERO)
     ! The centre of the radial functions, cartesian
     INT :: n_radii DEFAULT(40)
     ! The number of radii of the radial functions
     REAL :: radius_max DEFAULT(2*BOHR_PER_ANGSTROM)
     ! The largest radius of the radial functions
     MAT(REAL,3,3) :: direct_mx DEFAULT(ZERO)
     ! The direct cell matrix, columns the cell vectors
     STR(len=STR_SIZE) :: density_source DEFAULT("wavefunction")
     ! For the Hirshfeld-atom decomposition: "wavefunction" or "cell_map"
     STR(len=STR_SIZE) :: atom_weight DEFAULT("hirshfeld")
     ! For the Hirshfeld-atom decomposition: "hirshfeld" or "none"
     VEC(STR(len=STR_SIZE),:), allocatable :: atom_tags
     ! The atoms of a rebuilt-atoms map, as indices or tags; none means all
     VEC(INT,:), allocatable :: h1,h2,h3
     ! Miller indices of the Fourier coefficients, over the whole sphere
     VEC(CPX,:), allocatable :: coeff
     ! The Fourier coefficients, on absolute scale, divided by the cell volume
     VEC(CPX,:), allocatable :: F_pro
     ! The promolecule structure factors of the unique reflections, for the deformation kinds
     MAT(REAL,3,3) :: reciprocal_mx DEFAULT(ZERO)
     ! The reciprocal cell matrix, columns the reciprocal cell vectors
     REAL :: volume DEFAULT(ZERO)
     ! The cell volume
   end type
   type CRYSTAL_TYPE
     STR(len=STR_SIZE) :: CIF_chemical_formula DEFAULT("?")
     ! The chemical formula, as found in a CIF file.
     type(SPACEGROUP_TYPE) :: spacegroup
     ! The crystal spacegroup
     type(UNIT_CELL_TYPE) :: unit_cell
     ! The crystal unit cell

     ! ===========
     ! QM fragment
     ! ===========
     INT :: n_fragment_atoms DEFAULT(0)
     ! No. of atoms in the inputted cell fragment, used to calculate
     ! structure factors, or do wavefunction fitting.
     VEC(type(ATOM_TYPE),:), allocatable :: fragment_atom
     ! The set of fragment atoms. NOTE: this is normally pointer
     ! copied, except when a cluster object makes a new list.
     MAT3(REAL,:,:,:), allocatable :: fragment_U_high
     ! U of each fragment atom from its stiff internal modes, (3,3,n), bohr^2;
     ! the fixed part of a TLS model. Absent means zero.
     MAT(REAL,:,:), allocatable :: TLS_basis
     ! Columns: the site-symmetric 6x6 Sigma = [[T,S^T],[S,L]] of each TLS
     ! parameter, as its 21 upper-triangle elements
     INT :: TLS_first_param DEFAULT(0)
     ! Index of the first TLS parameter in .xray_data.ls
     VEC(REAL,3) :: TLS_origin DEFAULT(ZERO)
     ! Origin of the TLS model, the fragment's centre of mass (bohr)
     MAT(REAL,:,:), allocatable :: TLS_unique_response
     ! d U / d TLS parameter for the unique fragment atoms, (6 x n_unique, k)
     MAT(REAL,:,:), allocatable :: soft_mode_vectors
     ! Displacement of each fragment atom per unit normal coordinate, l/sqrt(m),
     ! for each soft mode: (3 x n_fragment_atoms, n_soft)
     VEC(REAL,:), allocatable :: soft_mode_harmonic
     ! Harmonic <Q^2> of each soft mode; negative for an imaginary mode
     VEC(REAL,:), allocatable :: soft_mode_frequency
     ! Harmonic frequency of each soft mode, cm^-1; negative for imaginary
     REAL :: soft_mode_temperature DEFAULT(ZERO)
     ! Temperature (Kelvin) the harmonic amplitudes are for
     INT :: n_TLS_held_at_zero DEFAULT(0)
     ! No. of directions of the TLS covariance held at zero to keep it positive
     ! See below.
     BIN :: destroy_fragment_atom DEFAULT(TRUE)
     ! Set TRUE if the fragment_atoms need to be destroyed
     MAT(REAL,:,:), allocatable :: fragment_geometry
     ! The geometry for a molecular fragment in the crystal (in
     ! fractional coordinates) used to calculate structure factors or
     ! do wavefunction fitting. NOTE: this should not to be confused
     ! with the "asymmetric_unit_geometry"; it may include symmetry
     ! non-unique atoms.
     VEC(INT,3) :: fragment_offset DEFAULT(0)
     ! The center point of the fragment_geometry, to the nearest unit cell

     ! =====================
     ! Asymmetric unit atoms
     ! =====================
     STR(len=STR_SIZE) :: asymmetric_unit_source DEFAULT("?")
     ! Where did the asymmetric unit come from?
     INT :: n_asymmetric_unit_atoms DEFAULT(0)
     ! No. of atoms in the asymmetric unit of the unit cell.
     VEC(type(ATOM_TYPE),:), allocatable :: asymmetric_unit_atom
     ! The set of asymmetric unit atoms.
     VEC(REAL,:), allocatable :: asymmetric_FF_symmetrization_rss
     ! residual sum of squares (RSS) from the symmetrization of atomic Form Factor
     !  = Sum_k |F(k)-F_sym(k)|^2 | 
     ! Check out .crystal.symmetrize_FFs(ff,ff_diffs)
     ! For the asymmetric atoms of site symmetry factor = 1, it is zero.
     ! The larger residual square sum, the more initial symmetry violation in
     !  the electron density.
     ! The Becke grid not following the local symmetry may give a rise to
     !  non-zero value.
     VEC(REAL,:), allocatable :: asymmetric_FF_symmetrization_rmax
     ! The largest single asymmetry, Max_k( |F(k)-F_sym(k)| ), for each
     ! asymmetric unit atom. The RSS above says how asymmetric the form
     ! factors are on average; this says how bad the worst one is.
     MAT(INT,:,:), allocatable :: asymmetric_FF_symmetrization_rmax_hkl
     ! The reflection (h,k,l) at which ..._symmetrization_rmax occurs,
     ! one row per asymmetric unit atom.
     MAT(REAL,:,:), allocatable :: asymmetric_unit_geometry
     ! The asymmetric unit geometry. Usually inputted from a CIF file.
     ! This may or may not be the same as fragment_geometry. It is NOT
     ! used for structure factor calculations, but for cluster
     ! generation.
     BIN :: prune_asymmetric_unit DEFAULT(TRUE)
     ! Set TRUE if you want to prune the asymmetric unit. Sometimes
     ! the asymmetric unit may not be asymmetric i.e. it may have
     ! symmetry-equivalent atoms repeated. This could be because of an
     ! input or CIF file error, or it could be genuinely correct
     ! because the repeated atom corresponds to a disordered position.

     ! ===============
     ! Unit cell atoms
     ! ===============
     INT :: n_unit_cell_atoms DEFAULT(0)
     ! Total no. of unit cell atoms.
     VEC(type(ATOM_TYPE),:), allocatable :: unit_cell_atom
     ! The set of unit cell atoms. Needed for dispersion correction.
     MAT(REAL,:,:), allocatable :: unit_cell_geometry
     ! The full unit cell geometry, in the crystal coordinate system,
     ! generated from the asymmetric_unit_geometry.
     VEC(INT,:), allocatable :: unique_unit_cell_atom
     ! The list of unique (asymmetric unit) atoms in the
     ! unit_cell_geometry above
     VEC(type(EVEC_INT_TYPE),:), allocatable :: unique_uc_atom_offset
     ! The list of (h1 h2 h3) offsets which need to be applied to the
     ! ".asymmetric_unit_geometry" to get the corresponding unit cell
     ! atom in the ".unit_cell_geometry".
     VEC(BIN,:), allocatable :: is_asym_unit_cell_atom
     ! Is TRUE if the the atom index corresponds to an asymmetric unit
     ! atom in unit_cell_geometry

     ! ========================================================
     ! Arrays relating fragment, unit cell, and asymmetric cell
     ! ========================================================
     REAL :: same_atom_tol DEFAULT(TOL(3))
     ! Tolerance to compare if two atom coordinates are the same
     ! in fractional coordinates
     REAL :: unit_cell_tol DEFAULT(TOL(6))
     ! Tolerance to compare if fractional position is close enough to
     ! integer to be made into an integer.
     BIN :: fragment_info_made DEFAULT(FALSE)
     ! Set TRUE if the fragment related information has been made
     VEC(INT,:), allocatable :: asym_atom_for_unit_cell_atom
     ! ".asym_atom_for_unit_cell_atom(u)" is the index of the unique
     ! unit cell atom (in ".asymmetric_unit_geometry") which generates
     ! ".unit_cell_geometry(:,u)".
     VEC(INT,:), allocatable :: asym_symop_for_unit_cell_atom
     ! "asym_symop_for_unit_cell_atom(u)" is the index of any
     ! spacegroup symmetry operation that generates atom position
     ! "unit_cell_geometry(:,u)" from the atom with index
     ! ".asym_atom_for_unit_cell_atom(u)" (and with position in
     ! ".asymmetric_unit_geometry").
     MAT(INT,:,:), allocatable :: asym_shift_for_unit_cell_atom
     ! "asym_shift_for_unit_cell_atom(u)" is the shift that was
     ! applied to get position "unit_cell_geometry(:,u)" after it was
     ! got from position of atom ".asym_atom_for_unit_cell_atom(u)"
     ! with the symop ".asym_symop_for_unit_cell_atom(u)" to get it
     ! into the unit cell.
     VEC(INT,:), allocatable :: frag_atom_for_unit_cell_atom
     ! ".frag_atom_for_unit_cell_atom(u)" is the index of the
     ! fragment atom (in ".fragment_geometry") which is equivalent to
     ! ".unit_cell_geometry(:,u)" *by cell translation*.  The index may
     ! be zero i.e. there is no fragment atom for unit cell atom u.
     VEC(INT,:), allocatable :: frag_parent_for_cell_atom
     ! ".frag_parent_for_unit_cell_atom(u)" is the index of the
     ! fragment atom in ".fragment_geometry" which is equivalent to
     ! .unit_cell_geometry(:,u) *by any crystal symop*. It may have
     ! zero elements if the fragment does not enhcompass the whole
     ! unit cell. Compare with ".frag_atom_for_unit_cell_atom".
     VEC(INT,:), allocatable :: unit_cell_atom_for_frag_atom
     ! ".unit_cell_atom_for_frag_atom(f)" is the index of the unit
     ! cell atom in ".unit_cell_geometry" which is equivalent to
     ! ".fragment_geometry(:,f)".  *by cell translation*. It is the
     ! =inverse= of the array above.
     MAT(INT,:,:), allocatable :: unit_cell_shft_for_frag_atom
     ! ".unit_cell_shft_for_frag_atom(f)" is the shift for position
     ! ".fragment_geometry(:,f)" needed to make it the same as unit
     ! cell atom in ".unit_cell_atom_for_frag_atom(f)" above.
     VEC(INT,:), allocatable :: frag_atom_for_asym_atom
     ! ".frag_atom_for_asym_atom(a)" is the index of the earliest
     ! fragment atom (in ".fragment_geometry") which is generated from
     ! the asymmetric unit atom position ".asymmeric_unit_geometry(:,a)"
     ! *by any crystal symop*.  This index may be zero meaning that
     ! there is no fragment atom for asymmetric atom "a".
     VEC(INT,:), allocatable :: asym_atom_for_frag_atom
     ! ".asym_atom_for_frag_atom(f)" is the index of the asymmetric
     ! atom (in ".asymmetric_unit_geometry") which is equivalent to
     ! ".fragment_geometry(:,f)" *by any crystal symop*.  This array
     ! is used to extract the atom list of the asymmetric unit from a
     ! given fragment for cluster generation. This is the =inverse=
     ! of the array above.
     INT :: n_unique_frag_atoms DEFAULT(0)
     ! No. of unique fragment atoms.
     VEC(INT,:), allocatable :: unique_frag_atom
     ! The list of unique fragment atoms, determined as the first
     ! atom in the fragment which is generated by a particular
     ! asymmetric unit atom.
     VEC(INT,:), allocatable :: unique_index_for_frag_atom
     ! The unique fragment atom index for a given frag atom.
     ! It is zero if the atom is not unqie.
     VEC(INT,:), allocatable :: unique_atom_for_frag_atom
     ! ".unique_atom_for_frag_atom(f)" is the index of the
     ! symmetry-unique atom position in ".fragment_geometry" which
     ! generates ".fragment_geometry(:,f)" by the symop with index
     ! .unique_symop_for_frag_atom(u) *except for a possible
     ! translational shift*.  The unique atom is determined as the
     ! first atom in the ".fragment_geometry" which is generated by a
     ! particular asymmetric unit atom.
     VEC(INT,:), allocatable :: unique_symop_for_frag_atom
     ! ".unique_symop_for_frag_atom(f)" is the index of the symop
     ! which generates ".fragment_geometry(:,f)" from
     ! ".unique_atom_for_frag_atom(f)" *except for a possible
     ! translational shift*.  The symop corresponds to the first atom
     ! in the ".fragment_geometry" which is generated by a particular
     ! asymmetric unit atom. WARNING: UNTESTED.
     VEC(INT,:), allocatable :: asym_symop_for_frag_atom
     ! ".asym_symop_for_frag_atom(f)" is the index of the symop which
     ! maps an asymmetric_unit_atom into the unit cell atom which is
     ! equivalent to fragment atom "f" *by translation*. See below.
     MAT(REAL,:,:), allocatable :: asym_shift_for_frag_atom
     ! "asym_shift_for_frag_atom(f)" is the shift required to map an
     ! asymmetric_unit_atom into the fragment atom after applying the
     ! 3x3 seitz operator with index ".asym_symop_for_frag_atom(f)".
     VEC(type(EVEC_INT_TYPE),:), allocatable :: asym_symops_for_frag_atom
     ! ".asym_symop_for_frag_atom(f)" are the index of all symops which
     ! map an asymmetric_unit_atom into the unit cell atom "u" which is
     ! equivalent to fragment atom "f" *up to translation*.

   ! asym_pair_for_frag_atom_pair :: VEC{INT}@
   ! ! The triangle number for the asymmetric unit atom pair that is
   ! ! symmetry-equivalent  to the frag atom pair in the fragment
   ! ! atom list.

   ! frag_pair_for_asym_atom_pair :: VEC{EVEC{INT}}@
   ! ! The triangle number for the fragment atom *pairs* that are
   ! ! symmetry-equivalent to the asymmetric atom pair in the fragment
   ! ! atom list. There may be more than one set of pairs!

     ! ==================
     ! Repetition factors
     ! ==================
     REAL :: Z DEFAULT(ZERO)
     ! The Z factor for the molecular cell fragment in the unit cell
     BIN :: use_manual_repetition_factors DEFAULT(FALSE)
     ! If this is set TRUE, then the user should manually enter the
     ! desired repetition factors. For experts only!
     VEC(REAL,:), allocatable :: repetition_factor
     ! The number of times *any* fragment atom is mapped into itself
     ! by the spacegroup. Used in structure factor calcs from a
     ! fragment geometry or a portion of it; in the latter case it can
     ! be reset manually.
     INT :: n_stabilizer_max DEFAULT(0)
     ! The maximum number of stabilizer operations for an atom
     VEC(INT,:), allocatable :: frag_atom_sym_factor
     ! The number of times an asymmetric unit atom is mapped into
     ! itself by the spacegroup i.e. the "site symmetry factor"
     ! however this array is for the atoms in the QM fragment.
     ! For structure factor calcs based on the asymmetric unit.
     VEC(type(EVEC_INT_TYPE),:), allocatable :: frag_atom_stabilizer
     ! The list of indices of seitz symop's which stabilize
     ! a given atoms position. See above.
     VEC(INT,:), allocatable :: asym_atom_sym_factor
     ! The number of times the position of an asymmetric unit atom is
     ! mapped into itself by the spacegroup i.e. the "site symmetry
     ! factor". For atomic form-factor calculations based on the
     ! asymmetric unit.
     VEC(type(EVEC_INT_TYPE),:), allocatable :: asym_atom_stabilizer
     ! The list of indices of seitz symop's which stabilize
     ! a given atoms position. See above.

     ! ==========================
     ! Unit cell connection table
     ! ==========================
     VEC(type(EVEC_EVEC_INT_TYPE),:), allocatable :: unit_cell_connection
     ! List of which unit cell atoms are connected to which other unit
     ! cell atoms, and in what cell e.g. "connection(i).element(k)"
     ! represents the "k"-th atom connected to atom "i" in the
     ! ".crystal.unit_cell_geometry", and it is comprised of a
     ! 4-vector, the first three elements being the cell of the "k"-th
     ! connected atom, and the last element being the index of the
     ! actual unit cell atom connected to unit cell atom "i".
     VEC(type(EVEC_EVEC_INT_TYPE),:), allocatable :: uc_vdw_connection
     ! As above, for vdw connections.
     VEC(INT,:), allocatable :: unit_cell_mol_for_atom
     ! "unit_cell_mol_for_atom(i)" is the index of the molecule to
     ! which a particular unit cell atom "i" belongs. See comments for
     ! "unit_cell_mol".
     INT :: n_unit_cell_mols DEFAULT(0)
     ! The number of molecules in the unit cell. Fragments of
     ! molecules are counted as separate molecules.

     ! ===============
     ! Stablizer group
     ! ===============
     INT :: n_stabilizer_symops DEFAULT(0)
     ! No. of symops needed to make the unit_cell_geometry from
     ! fragment_geometry
     VEC(INT,:), allocatable :: stabilizer_symop
     ! Indices of the symops in the spacegroup seitz list which map
     ! .fragment_geometry into itself.
     MAT3(REAL,:,:,:), allocatable :: str
     ! Representation matrices for s-type gaussian functions of the
     ! stabilizer symops.
     MAT3(REAL,:,:,:), allocatable :: ptr
     ! Representation matrices for p-type gaussian functions of the
     ! stabilizer symops.
     MAT3(REAL,:,:,:), allocatable :: dtr
     ! Representation matrices for d-type gaussian functions of the
     ! stabilizer symops.
     MAT3(REAL,:,:,:), allocatable :: ftr
     ! Representation matrices for f-type gaussian functions of the
     ! stabilizer symops.
     MAT3(REAL,:,:,:), allocatable :: gtr
     ! Representation matrices for g-type gaussian functions of the
     ! stabilizer symops.

     ! ================
     ! Diffraction data
     ! ================

   ! data_kind :: STR  DEFAULT("?")
     ! Kind of diffraction experiment used.
     type(DIFFRACTION_DATA_TYPE), allocatable :: xray_data
     ! The X-ray structure factor and fitting data

   ! pnd_data :: DIFFRACTION_DATA@ 
     ! The PND scalar structure factor and fitting data
     INT :: r_free_percentage DEFAULT(0)
     ! A percentage which tells how much of the data is to be reserved
     ! and not fitted in the model
     type(DIFFRACTION_DATA_TYPE), allocatable :: xray_r_free_data
     ! This is the excluded X-ray data used to calculate R free

   ! pnd_r_free_data :: DIFFRACTION_DATA@ 
     ! This is the excluded PND data used to calculate R free
     BIN :: finalize DEFAULT(TRUE)
     ! Finalize/update non-cluster crystal data

     ! ===============
     ! Cluster-related
     ! ===============
     REAL :: radius DEFAULT(ZERO)
     ! The radius of the cluster. This number determines the maximum
     ! acceptable distance between an atom in fragment_geometry and
     ! any atom in the crystal lattice. It defines the size of the
     ! cluster in the case that "generation_method" component (see
     ! below) takes the value "within_radius".
     REAL :: atom_density_cutoff DEFAULT(TOL(10))
     ! This number is used to define a cluster radius (see above). The
     ! radius is defined from it, as the *largest* distance from any
     ! atom in the asymmetric_cell_atom list where the atomic electron
     ! density becomes smaller than this number. It requires that
     ! slater or coppens bases are defined in asymmetric_unit_atom's,
     ! and it requires that the generation method be set to
     ! "for_hirshfeld_surface". It is primarily used for clusters
     ! generated from a fragment which will correctly produce the
     ! Hirshfeld surface.
     STR(len=STR_SIZE) :: generation_method DEFAULT("fragment")
     ! This option tell how to make the cluster: whether to add atoms
     ! within a certain distance (radius) of the starting fragment,
     ! whether to make a cluster specifically for generating a
     ! hirshfeld surface, whether to just make the unit cell, or the
     ! unit cell and the starting fragment.
     BIN :: defragment DEFAULT(FALSE)
     ! If TRUE, the cluster ends are defragmented, i.e. any atoms
     ! which are "bonded" at the ends of the cluster are included into
     ! the cluster. This isn't a good idea for network covalent
     ! solids or zeolites: the defragmentation will end when all
     ! bonded entites with the 3x3 cube of units cells near the origin
     ! have been constructed.
     BIN :: reorder_in_mols DEFAULT(TRUE)
     ! If TRUE reorder the generated cluster so that consecutive atoms
     ! in the occupation_list are connected together (CCDC crieria).
     INT :: n_atoms
     ! The number of atoms in the cluster. (crystal).n_atoms = (crystal).occupation_list.dim
     MAT(REAL,:,:), allocatable :: geometry
     ! The (3 x .n_atoms) sized array of cluster atom positions, in
     ! fractional coordiantes
     VEC(REAL,3) :: unit_cell_offset DEFAULT(ZERO)
     ! Unit cell offset to use when generating an offset unit cell.
     VEC(INT,3) :: h_min
     ! The minimum values of the cells to search over in a within_radius cluster
     VEC(INT,3) :: h_max
     ! The maximum values of the cells to search over in a within_radius cluster
     INT :: n_shift DEFAULT(0)
     ! The number of distinct translational shifts used to generate the atoms in
     ! the cluster.
     MAT(INT,:,:), allocatable :: shift
     ! The (3 x .n_shift) sized list of translational shifts used to generate the
     ! cluster from the crystal unit cell atoms. See ".shift_for_atom".
     ! NOT CURRENTLY USED
     VEC(INT,:), allocatable :: shift_for_atom
     ! ".shift_for_atom(a)" is the index of the translational shift in array
     ! ".shift", that shift being added to the position of the crystal unit cell
     ! atom whose index is given by ".occupation_list(a)/1000" in order to
     ! generate the cluster atom "a" whose position is given by ".geometry(a)".
     ! NOT CURRENTLY USED
     VEC(BIN,:), allocatable :: is_fragment_atom
     ! is_fragment_atom(a) is TRUE if .geometry(:,a) is the position of a fragment
     ! atom, i.e. if "a" is the index of a fragment atom.
     VEC(INT,:), allocatable :: new_frag_atom_ind
     ! new_frag_atm_ind(a) is the new index of the original fragment
     ! atom, before it was reordered into molecules.
     VEC(INT,:), allocatable :: occupation_list
     ! The list of atoms in the cluster which are occupied. This is a
     ! coded integer representing the triple of integers (h1,h2,h3),
     ! the cell where the atom resides, as well as the integer index i
     ! for the unit cell atom. The cell integers are limited to be in
     ! the range [-4,4] and at most 10^6 atoms in the cluster.
     INT :: n_molecules DEFAULT(0)
     ! The number of molecules in the cluster
     VEC(INT,:), allocatable :: molecule_for_atom
     ! An integer which tells which molecule the cluster atoms belongs to.
     VEC(type(EVEC_INT_TYPE),:), allocatable :: molecule
     ! The list of atom indices corresponding to each molecule
     MAT(REAL,:,:), allocatable :: molecule_centroid
     ! The list of molecule centroids in fractionals
     ! NOT CURRENTLY USED
     INT :: n_unique_molecules DEFAULT(0)
     ! The number of unique molecules in a defragmented cluster
     VEC(INT,:), allocatable :: unique_molecule_for
     ! The unique molecules; unique_molecule_for(mi) is the molecule
     ! earlier in the list which is symmetry equivalent by the symop
     ! given in unique_symcode_for.
     ! NOT CURRENTLY USED
     VEC(INT,:), allocatable :: unique_symcode_for
     ! The list of symops which maps unique_molecule_for(mi) to the
     ! non-unique molecule mi. See above.
     ! NOT CURRENTLY USED
     VEC(type(EVEC_INT_TYPE),:), allocatable :: atom_connection
     ! atom_connection(i)[:] are those atoms which are connected to atom "i".
     VEC(INT,:), allocatable :: unit_cell_atoms_to_suppress
     ! A list of indices into unit cell geometry. Used
     ! to suppress those atoms in cluster generation.
     BIN :: cluster_info_made DEFAULT(FALSE)
     ! Set to true if the routine make_info has been called
   end type
   type CLUSTER_TYPE
     REAL :: radius DEFAULT(ZERO)
     ! The radius of the cluster. This number determines the maximum
     ! acceptable distance between an atom in fragment_geometry and
     ! any atom in the crystal lattice. It defines the size of the
     ! cluster in the case that "generation_method" component (see
     ! below) takes the value "within_radius".
     REAL :: atom_density_cutoff DEFAULT(TOL(10))
     ! This number is used to define a cluster radius (see above). The
     ! radius is defined from it, as the *largest* distance from any
     ! atom in the asymmetric_cell_atom list where the atomic electron
     ! density becomes smaller than this number. It requires that
     ! slater or coppens bases are defined for the
     ! asymmetric_cell_atom's, and it requires that the generation
     ! method be set to "for_hirshfeld_surface". It is primarily used
     ! for clusters generated from a fragment which will correctly
     ! produce the Hirshfeld surface.
     STR(len=STR_SIZE) :: generation_method DEFAULT("fragment")
     ! This option tell how to make the cluster: whether to add atoms
     ! within a certain distance (radius) of the starting fragment,
     ! whether to make a cluster specifically for generating a
     ! hirshfeld surface, whether to just make the unit cell, or the
     ! unit cell and the starting fragment.
     BIN :: defragment DEFAULT(FALSE)
     ! If TRUE, the cluster ends are defragmented, i.e. any atoms
     ! which are "bonded" at the ends of the cluster are included into
     ! the cluster. This isn't a good idea for network covalent
     ! solids or zeolites: the defragmentation will end when all
     ! bonded entites with the 3x3 cube of units cells near the origin
     ! have been constructed.
     INT :: n_atoms
     ! The number of atoms in the cluster
     MAT(REAL,:,:), allocatable :: geometry
     ! The (3 x .n_atoms) sized array of cluster atom positions, in crystal
     ! coordiantes
     type(CRYSTAL_TYPE), allocatable :: crystal
     ! The crystal information used to generate the cluster. The most important
     ! part used is the asymmetric unit cell geometry.
     VEC(type(ATOM_TYPE),:), allocatable :: asymmetric_unit_atom
     ! The atom list data associated with the *asymmetric* unit cell_geometry (see
     ! below) used to generate the cluster atom positions. It must be consistent
     ! with the crystal asymmetric unit.
     INT :: n_fragment_atoms
     ! The number of fragment atoms used to build the cluster
     MAT(REAL,:,:), allocatable :: fragment_geometry
     ! The (3 x .n_fragment_atoms) sized array of fragment atom positions used to
     ! generate the cluster: the cluster will usually be arranged as the set of
     ! atoms within a certain radius of this fragment geometry. It is in the
     ! crystal coordinate system. Note that the fragment geometry might not
     ! include an asymmetric cell unit.
     VEC(REAL,3) :: fragment_width DEFAULT(ZERO)
     ! The width of the crystal fragment, to the nearest unit cell
     VEC(INT,3) :: fragment_offset DEFAULT(0)
     ! The center point of the fragment_geometry, to the nearest unit cell
     VEC(REAL,3) :: unit_cell_offset DEFAULT(ZERO)
     ! Unit cell offset to use when generating an offset unit cell.
     VEC(INT,3) :: h_min DEFAULT(0)
     ! The minimum values of the cells to search over in a within_radius cluster
     VEC(INT,3) :: h_max DEFAULT(0)
     ! The maximum values of the cells to search over in a within_radius cluster
     INT :: n_shift DEFAULT(0)
     ! The number of distinct translational shifts used to generate
     ! the atoms in the cluster.
     MAT(INT,:,:), allocatable :: shift
     ! The (3 x .n_shift) sized list of translational shifts used to generate the
     ! cluster from the crystal unit cell atoms. See ".shift_for_atom".
     ! NOT CURRENTLY USED
     VEC(INT,:), allocatable :: shift_for_atom
     ! ".shift_for_atom(a)" is the index of the translational shift in array
     ! ".shift", that shift being added to the position of the crystal unit cell
     ! atom whose index is given by ".occupation_list(a)/1000" in order to
     ! generate the cluster atom "a" whose position is given by ".geometry(a)".
     ! NOT CURRENTLY USED
     VEC(BIN,:), allocatable :: is_fragment_atom
     ! is_fragment_atom(a) is TRUE if .geometry(:,a) is the position of a fragment
     ! atom, i.e. if "a" is the index of a fragm,ent atom.
     VEC(INT,:), allocatable :: occupation_list
     ! The list of atoms in the cluster which are occupied. This is a coded
     ! integer representing the triple of integers (h1,h2,h3), the cell where the
     ! atom resides, as well as the integer index i for the unit cell atom. The
     ! cell integers are limited to be in the range [-4,4] and at most 10^6 atoms
     ! in the cluster.
     INT :: n_molecules DEFAULT(0)
     ! The number of molecules in the cluster
     VEC(INT,:), allocatable :: molecule_for_atom
     ! An integer which tells which molecule the cluster atoms belongs to.
     VEC(type(EVEC_INT_TYPE),:), allocatable :: molecule
     ! The list of atom indices corresponding to each molecule
     MAT(REAL,:,:), allocatable :: molecule_centroid
     ! The list of molecule centroids in fractionals
     ! NOT CURRENTLY USED
     INT :: n_unique_molecules DEFAULT(0)
     ! The number of unique molecules in a defragmented cluster
     VEC(INT,:), allocatable :: unique_molecule_for
     ! The unique molecules; unique_molecule_for(mi) is the molecule
     ! earlier in the list which is symmetry equivalent by the symop
     ! given in unique_symcode_for.
     ! NOT CURRENTLY USED
     VEC(INT,:), allocatable :: unique_symcode_for
     ! The list of symops which maps unique_molecule_for(mi) to the
     ! non-unique molecule mi. See above.
     ! NOT CURRENTLY USED
     VEC(type(EVEC_INT_TYPE),:), allocatable :: atom_connection
     ! atom_connection(i)[:] are those atoms which are connected to atom "i".
     BIN :: info_made DEFAULT(FALSE)
     ! Set to true if the routine make_info has been called
     VEC(INT,:), allocatable :: unit_cell_atoms_to_suppress
     ! A list of indices into unit cell geometry. Used
     ! to suppress those atoms in cluster generation.
   end type
   type MULTI_T_ADP_TYPE

     ! ===================
     ! Calculation options
     ! ===================
     STR(len=STR_SIZE) :: fit_method DEFAULT("unknown")
     ! The method used to refine the ADP's
     VEC(STR(len=STR_SIZE),:), allocatable :: cif_file_name
     ! A list of the CIF files names that contain the multi
     ! temperature ADP data. Normally ordered from lowest
     ! to highest temperature.
     INT :: V_expansion_order DEFAULT(2)
     ! Order to expand the volumes as a polynomial series
     BIN :: use_V_linear_term DEFAULT(TRUE)
     ! Whether to use the linear term in the expansion of the volume
     BIN :: use_translations DEFAULT(TRUE)
     ! Set TRUE if you want to refine translational modes
     BIN :: use_librations DEFAULT(TRUE)
     ! Set TRUE if you want to refine librational modes
     BIN :: use_fixed_H_modes DEFAULT(FALSE)
     ! Set TRUE if you want to use fixed high frequency local H modes
     ! aka Buergi and coworkers "epsilon" tensors.
     BIN :: use_fixed_non_H_modes DEFAULT(FALSE)
     ! Set TRUE if you want to use fixed high frequency local non-H
     ! modes aka Buergi and coworkers "epsilon" tensors.
     INT :: n_internal DEFAULT(0)
     ! No. of internal modes to refine
     BIN :: project_onto_initial_modes DEFAULT(FALSE)
     ! Project the mode shifts onto the starting modes.
     ! Normally used for translations and rotations.
     INT :: n_project DEFAULT(-1)
     ! No. of initial modes (starting from the first) to use in
     ! projection
     VEC(type(EVEC_INT_TYPE),:), allocatable :: equality_constraints
     ! A list of parameter indices which are to be made equal
     ! Indices should be ordered least to greatest. These are
     ! normally applied to mode indices and Grueneisen's.
     VEC(type(EVEC_INT_TYPE),:), allocatable :: decoupled_parameters
     ! A list of parameters indices which are decoupled
     ! from the others
     INT :: max_iterations DEFAULT(MULTI_T_ADP_MAX_ITER)
     ! The maximum number of iterations for the normal equations
     INT :: min_iterations DEFAULT(MULTI_T_ADP_MIN_ITER)
     ! The minumum number of iterations for the normal equations
     REAL :: tol_shift_convergence DEFAULT(MULTI_T_ADP_TOL_SHIFT_CONV)
     ! Tolerance for convergence of the parameter shifts divided by
     ! the esd's, used when solving the normal equations
     REAL :: max_allowed_shift DEFAULT(MULTI_T_ADP_MAX_SHIFT)
     ! The maximum allowed shift when solving the normal equations:
     ! If any one of the parameters shifts obtained from solving the
     ! normal equations is greater than this value, they are all
     ! rescaled so that the largest is this value.
     BIN :: show_correlations DEFAULT(FALSE)
     ! Whether to display the correlation matrix or not
     REAL :: min_correlation DEFAULT(MULTI_T_ADP_MIN_CORRELATION)
     ! The minimum value for correlation matrix elements before they
     ! are printed out in the output.
     BIN :: show_near_0_eigenvectors DEFAULT(FALSE)
     ! Whether to display the near zero eigenvectors
     REAL :: tol_near_0 DEFAULT(MULTI_T_ADP_TOL_NEAR_0)
     ! The tolerance used to detect near zero eigenvalues in the
     ! normal equations

     ! ================
     ! Private switches
     ! ================
     BIN :: cif_files_read DEFAULT(FALSE)
     ! Set TRUE if the cif files have been read
     BIN :: initialized DEFAULT(FALSE)
     ! Set up starting info

     ! =================
     ! Experimental data
     ! =================
     INT :: n_T DEFAULT(0)
     ! The number of temperatures
     INT :: n_atom DEFAULT(0)
     ! The number of atoms in the file
     INT :: n3N DEFAULT(0)
     ! Three times the number of atoms
     INT :: n6N DEFAULT(0)
     ! 6 times the number of atoms
     INT :: n9N DEFAULT(0)
     ! 9 times the number of atoms
     VEC(type(ATOM_TYPE),:), PTR :: atom DEFAULT_NULL
     ! List of atoms for the first CIF file used to construct
     ! the starting rigid modes L^rigid. *Pointer copied*.
     VEC(REAL,:), allocatable :: temperature
     ! The different experimental temperatures
     VEC(REAL,:), allocatable :: temperature_sigma
     ! The uncertainty in the experimental temperatures
     VEC(type(UNIT_CELL_TYPE),:), allocatable :: unit_cell
     ! The list of unit cells at the different temperatures
     VEC(type(UNIT_CELL_TYPE),:), allocatable :: unit_cell_sigma
     ! The list of unit cell errors at the different temperatures
     VEC(type(EMAT_REAL_TYPE),:), allocatable :: ADP
     ! A list of the X-ray ADP's at the different temperatures
     ! n_T x 6 in size
     VEC(type(EMAT_REAL_TYPE),:), allocatable :: ADP_cov
     ! For every temperature, the covariance matrix between
     ! the ADP's. Must be extracted from full covariance matrix.
     VEC(type(EMAT_REAL_TYPE),:), allocatable :: ADP_wt
     ! For every temperature, the weights to use in the least squares.
     ! Normally the inverse of the covariance matrix between ADP's.

     ! ==========
     ! Model info
     ! ==========
     VEC(REAL,:), allocatable :: V_coeff
     ! List of polynomial coefficients to expand the volume
     VEC(REAL,:), allocatable :: Delta_V_on_V
     ! The relative changes in unit cell volumes
     VEC(STR(len=STR_SIZE),:), allocatable :: parameter_description
     ! A string description of each parameter
     INT :: n_modes DEFAULT(0)
     ! The number of modes in the model
     MAT(REAL,:,:), allocatable :: L
     ! The list of normal modes
     VEC(REAL,:), allocatable :: lambda
     ! Lagrange multipliers for mode orthogonality
     VEC(REAL,:), allocatable :: omega
     ! The list of harmonic frequencies
     MAT(REAL,:,:), allocatable :: amplitude
     ! The list of amplitudes for each mode omega bar each temperature
     VEC(REAL,:), allocatable :: gamma_G
     ! The list of Grueneisen constants

     ! ===============================
     ! Structure refinement parameters
     ! ===============================
     INT :: iteration
     ! The iterations coiunter for structure refinement
     REAL :: chi2
     ! The current iteration's value of chi2
     REAL :: chi2_old
     ! The previous iteration's value of chi2
     REAL :: max_shift_on_esd
     ! The maximum shift in the parameter vector divided by the esd
     ! of that parameter during a refinement step.
     REAL :: max_shift
     ! The maximum shift in the parameter vector
     INT :: n_ind_params
     ! No of independent parameters taking into account zero
     ! eigenvalues
     INT :: n_0
     ! The number of zero eigenvalues detected when solving the
     ! normal equations
     INT :: n_near_0
     ! The number near zero eigenvalues detected when solving the
     ! normal equations
     VEC(REAL,:), allocatable :: near_0_eigenvalues
     ! The near zero eigenvalues obtained when solving the normal
     ! equations. Useful to see parameter correlations.
     MAT(REAL,:,:), allocatable :: near_0_eigenvectors
     ! The columns of eigenvectors with near zero eigenvalue obtained
     ! when solving the normal equations. Useful to see parameter
     ! correlations.
     VEC(REAL,:), allocatable :: delta_p
     ! Uncertainties in the parameters
     MAT(REAL,:,:), allocatable :: covariance_mx
     ! The covariance matrix for the parameters i.e.
     ! the inverse of the LHS of the least squares matrix.
     MAT(REAL,:,:), allocatable :: correlation_mx
     ! The correlation coefficient matrix for the parameters
   end type

!  =========
!  Spherical
!  =========
   type SPHERICAL_HARMONIC_TYPE

     ! This module is essentially just methods
     ! Spherical harmonics?
     INT :: L DEFAULT(0)
     ! The L angular momentum value
     INT :: M DEFAULT(0)
     ! The z-component of angular momentum value
     INT :: n_points DEFAULT(0)
     INT :: nfact DEFAULT(1)
     VEC(REAL,:), allocatable :: factorials
     BIN :: recur
   end type

!  ======
!  BIGINT
!  ======

!    type BIGINT
!        n :: INT DEFAULT(0)
!        dig :: VEC{INT}* DEFAULT_NULL
!        base :: INT DEFAULT(10)
!    end

!  ====
!  JSON
!  ====
   type JSON_TYPE
     type(TEXTFILE_TYPE), allocatable, public :: out
     ! Output texctfile
     STR(len=STR_SIZE) :: name DEFAULT("unknown")
     ! ?
     INT :: margin DEFAULT(0)
     ! Margin size for the JSON file
   end type

!  ======
!  Colour
!  ======
   type COLOUR_TYPE
     STR(len=STR_SIZE) :: name DEFAULT(" ")
     ! The standard colour name for this colour.
     VEC(INT,3) :: RGB255 DEFAULT(0)
     ! The RGB triple for this colour as a triple of integers between 0 and 255.
   end type
   type COLOUR_FUNCTION_TYPE
     INT :: n_data
     ! The number of data values (and their associated colours) used to make
     ! the colour function
     VEC(REAL,:), allocatable :: data
     ! The list of data values, from smallest to largest
     MAT(REAL,:,:), allocatable :: RGB
     ! The RGB values corresponding to each data value. Their norm should be
     ! between zero and 3.
     BIN :: finalized DEFAULT(FALSE)
     ! Set TRUE if the object is ready for use
   end type

!  =====================
!  Plots and isosurfaces
!  =====================
   type PLOT_GRID_TYPE
     STR(len=STR_SIZE) :: plot_kind DEFAULT(" ")
     ! The type of plot calculation wanted
     VEC(STR(len=STR_SIZE),:), allocatable :: plot_kinds
     ! The type of plot kinds (multiple) wanted
     STR(len=STR_SIZE) :: plot_label DEFAULT(" ")
     ! An optional label to describe the plot, used on archives.
     VEC(REAL,3) :: projection_direction DEFAULT(ZERO)
     ! For vector fields, the vectors at each point in the grid are
     ! projected onto the (normalized) direction of this vector.
     type(INTERPOLATOR_TYPE), allocatable :: interpolator
     ! This is used for setting defaults for isosurfaces which make
     ! use of spherical atomic densities.
     INT :: n_x DEFAULT(PLOT_GRID_NX)
     ! The number of points on the x-axis
     INT :: n_y DEFAULT(PLOT_GRID_NY)
     ! The number of points on the y-axis
     INT :: n_z DEFAULT(PLOT_GRID_NZ)
     ! The number of points on the z-axis
     INT :: n_pt DEFAULT(0)
     ! The total no. of points in the plot
     INT :: min_batch_size DEFAULT(4096)
     ! The minimum no. of points in a rectilinear batch
     VEC(type(ATOM_TYPE),:), allocatable :: atom
     ! List of atoms used to define grid axes and positions.
     ! *Pointer copied*.
     INT :: center_atom DEFAULT(0)
     ! Use this atom as the center of the plot
     VEC(INT,:), allocatable :: center_atoms
     ! List of center atoms
     INT :: x_atom_1 DEFAULT(0)
     ! These atoms define the x-axis of the plot
     INT :: x_atom_2 DEFAULT(0)
     ! These atoms define the x-axis of the plot
     INT :: y_atom_1 DEFAULT(0)
     ! These atoms define the y-axis of the plot 
     ! (made orthogonal to the x-axis)
     INT :: y_atom_2 DEFAULT(0)
     ! These atoms define the y-axis of the plot
     ! (made orthogonal to the x-axis)
     INT :: z_atom_1 DEFAULT(0)
     ! These atoms define the z-axis of the plot.
     ! (made orthogonal to the x-axis and y-axis)
     INT :: z_atom_2 DEFAULT(0)
     ! These atoms define the z-axis of the plot.
     ! (made orthogonal to the x-axis and y-axis)
     REAL :: del_x DEFAULT(ONE)
     ! The distance between axis points in the plot
     REAL :: del_y DEFAULT(ONE)
     ! The distance between axis points in the plot
     REAL :: del_z DEFAULT(ONE)
     ! The distance between axis points in the plot
     REAL :: desired_separation DEFAULT(ZERO)
     ! The desired separation between grid points.
     VEC(INT,3) :: grid_points
     ! number of grid points as requested by the user
     VEC(REAL,3) :: center DEFAULT(ZERO)
     ! Center of the plot
     VEC(REAL,3) :: origin DEFAULT(ZERO)
     ! Bottom left front point of the plot, unless .origin_atom is
     ! set, in which case it is the same as the .center.
     VEC(REAL,3) :: x_axis DEFAULT(PLOT_GRID_X_AXIS)
     ! x-axis of the plot
     VEC(REAL,3) :: y_axis DEFAULT(PLOT_GRID_Y_AXIS)
     ! y-axis of the plot
     VEC(REAL,3) :: z_axis DEFAULT(PLOT_GRID_Z_AXIS)
     ! z-axis of the plot
     VEC(REAL,3) :: width DEFAULT(PLOT_GRID_WIDTH)
     ! The widths (in a.u.) of each axis of the plot
     VEC(REAL,3) :: offset DEFAULT(ZERO)
     ! The center of the plot is offset by this amount
     BIN :: origin_atom DEFAULT(FALSE)
     ! The bounding box will start at the center atom / center vector
     BIN :: x_width_set DEFAULT(TRUE)
     ! If False the x_width, i.e. width(1), is to be calculated from
     ! the size of the inputted x_axis vector; or if a width has *not*
     ! been inputted.
     BIN :: y_width_set DEFAULT(TRUE)
     ! If False the y_width, i.e. width(2), is to be calculated from
     ! the size of the inputted y_axis vector; or if a width has *not*
     ! been inputted.
     BIN :: z_width_set DEFAULT(TRUE)
     ! If False the z_width, i.e. width(3), is to be calculated from
     ! the size of the inputted z_axis vector; or if a width has *not*
     ! been inputted.
     BIN :: x_axis_defined DEFAULT(FALSE)
     ! Flag set true if the user has defined the x_axis in the input
     BIN :: y_axis_defined DEFAULT(FALSE)
     ! Flag set true if the user has defined the y_axis in the input
     BIN :: z_axis_defined DEFAULT(FALSE)
     ! Flag set true if the user has defined the z_axis in the input

     ! =================
     ! Bounding box data
     ! =================
     BIN :: use_bounding_box DEFAULT(FALSE)
     ! IF set TRUE the bounding box has been set.
     VEC(REAL,3) :: box_center DEFAULT(ZERO)
     ! The center of the bounding box, defined as the center of an optional
     ! inputted atom list.
     VEC(REAL,3) :: box_widths
     ! The bounding box widths for the molecule, defined from an
     ! optional atom list.
     REAL :: box_scale_factor DEFAULT(ONE)
     ! Scale factor for the bounding box.  Usually leave it at 1.
     MAT(REAL,3,3) :: box_axes
     ! The axes of the bounding box, defined as the principal axes of
     ! the shape tensor (essentially the unit weighted moment of
     ! inertia tensor). Defined from an optionally inputted atom list.
     BIN :: use_unit_cell_as_bbox DEFAULT(FALSE)
     ! IF set TRUE the bounding box is supposed to be the unit cell.
     ! Then the grid must be finalized outside the module.

     ! =============================
     ! Plot units, format and limits
     ! =============================
     STR(len=STR_SIZE) :: plot_units DEFAULT(" ")
     ! Default units for the plot: the value is used to convert the grid values
     VEC(STR(len=STR_SIZE),:), allocatable :: plot_kinds_units
     ! Different units for each of the plot_kinds.
     STR(len=STR_SIZE) :: plot_format DEFAULT(" ")
     ! Default format for the plot: if blank, all known formats are produced.
     STR(len=STR_SIZE) :: points_filename DEFAULT("no_made-up_points")
     ! filename for file containing a made-up grid of 3D points to read in
     BIN :: use_plot_max DEFAULT(FALSE)
     ! Whether to reset the plot maximum
     BIN :: use_plot_min DEFAULT(FALSE)
     ! Whether to reset the plot minimum
     REAL :: plot_max DEFAULT(ZERO)
     ! Whether to reset the plot maximum
     REAL :: plot_min DEFAULT(ZERO)
     ! Whether to reset the plot minimum

     ! =======================
     ! Plot-kind specific data
     ! =======================
     INT :: orbital DEFAULT(0)
     ! The orbital to plot (if any)
     INT :: HOMO_orbital DEFAULT(0)
     ! The HOMO orbital to plot. A value "n" indicates the HOMO+"n".
     INT :: LUMO_orbital DEFAULT(0)
     ! The LUMO orbital to plot. A value "n" indicates the LUMO+"n".
     VEC(INT,:), allocatable :: bond_density_atom
     ! Pair of atoms indices whose bond density should be plotted.
     ! Triple indicates an atom within a bond.
     STR(len=STR_SIZE) :: electric_potential_method DEFAULT("exact")
     ! Method to suse when calculating electric potential on the isosurface
     INT :: group DEFAULT(0)
     ! Molecular group to use as central element in fragment_plot

     ! ============
     ! Contour data
     ! ============
     STR(len=STR_SIZE) :: contour_scale DEFAULT("log")
     ! Whether to use linear, log, positive- or negative-log on the
     ! range scale.
     REAL :: contour_min_value DEFAULT(ZERO)
     ! Minimum value for contour
     REAL :: contour_max_value DEFAULT(ZERO)
     ! Maximum value for contour
     REAL :: contour_increment DEFAULT(ZERO)
     ! Value to increment contors by
     type(COLOUR_TYPE) :: contour_min_color
     ! Color for minimum-value contour
     type(COLOUR_TYPE) :: contour_mid_color
     ! Color for mid-range-contour (or the zero contour)
     type(COLOUR_TYPE) :: contour_max_color
     ! Color for maximum-value contour
     STR(len=STR_SIZE) :: contour_color_scale DEFAULT(" ")
     ! Explicit color scale to use (gnuplot format)
     REAL :: contour_line_width DEFAULT(1.25)
     ! Width for cotour lines.
     REAL :: contour_fade_factor DEFAULT(0.8)
     ! Factor to fade the contour-colors by, for the palette
     INT :: contour_tic_dps DEFAULT(2)
     ! No of decimal places on the colorbox tics
     INT :: contour_font_size DEFAULT(9)
     ! Font size for contour scale
     BIN :: use_ruler_scale DEFAULT(TRUE)
     ! Whether to use a ruler scale on contour plot
     BIN :: use_colorbox_scale DEFAULT(TRUE)
     ! Whether to plot a colorbox scale onb contour plot
   end type
   type CAPPING_SQUARE_TYPE
     INT :: case DEFAULT(-1)
     ! A coded bit string to describe the particular case for capping
     ! triangles. These are essentially the same as for marching squares.
     INT :: face_number DEFAULT(0)
     ! Integer representing which face the of the cube the capping
     ! square belongs to.  i.e. it's orientation. if face_number=1
     ! then it's the z=0 face.
     REAL :: iso_value DEFAULT(ONE)
     ! The isovalue to be used for the isosurface
     INT :: skip_bit_string DEFAULT(0)
     ! A bit string which tells whether to skip processing certain
     ! edges. Can be used when certain edge vertex points are already
     ! done.
     MAT(REAL,3,0:3) :: vertex_positions DEFAULT(ZERO)
     ! A (3 x 0:3) dimensioned list of the actual coordinates of each
     ! square vertex.  The second index is the standard capping square
     ! vertex number.
     MAT(REAL,3,0:3) :: edge_positions DEFAULT(ZERO)
     ! A (3 x 0:3) dimensioned list of the interpolated coordinates of
     ! the edge vertices that are shared between the marching cube and
     ! the capping square.  The second index is the standard capping
     ! square vertex number.
     VEC(REAL,0:3) :: vertex_fn_value DEFAULT(ZERO)
     ! A (0:3) dimensioned list of the values of the function at each
     ! square vertex.
     VEC(INT,0:7) :: edge_vertex_index DEFAULT(0)
     ! A (0:7) dimensioned list of the  triangle vertex *indices* for
     ! each "edge" of the capping square.  Because the vertices of the
     ! square are involved in triangulation that are encoded as edges:
     ! v0 -> e4, v1 -> e5, v2 -> e6, v4 -> e7.
     ! Not all these edge are new since they include the edge vertices
     ! taken from the marching cube
     MAT(REAL,3,0:7) :: edge_vertex_positions DEFAULT(ZERO)
     ! A (3 x 0:7) dimensioned list of the interpolated triangle vertex
     ! coordinates on each edge of the cube (if that edge is needed).
     VEC(BIN,0:3) :: cap_vertex_flag DEFAULT(FALSE)
     ! Used to flag vertices of capping square which belong to caps
     ! and therefore need to be triangulated.
     INT :: n_triangle DEFAULT(0)
     ! The number of triangles formed on this capping square.
     INT :: n_pt DEFAULT(0)
     ! The number of points up until this square was analyzed. Used to
     ! keep a list of unique indices to define triangle vertices and
     ! points.
     INT :: no_new_vertex DEFAULT(0)
     ! A capping square contains previously determined edge vertices
     ! (from marching cube) and new edge vertices. This contains the
     ! number of new edge vertices.
     MAT(INT,3,4) :: triangle_edge_index DEFAULT(0)
     ! The (3 x .n_triangle) dimensioned list of 3-edges on the
     ! capping square used to form triangles. This is just the
     ! appropriate row from the triangle_table. The maximum number of
     ! triangles is 4.
     MAT(INT,3,4) :: triangle_vertex_index DEFAULT(0)
     ! The (3,.n_triangle) dimensioned list of groups of 3 *unique*
     ! triangle vertex *indices*. These indices point to a unique list
     ! of points. The maximum number of triangles is 4.
     VEC(INT,6) :: new_vertex_index DEFAULT(0)
     ! A list of the edge indices for each new triangle vertex index.
     ! Only the vertices of the square become new edge vertices hence
     ! only can have four new vertex indices.
     MAT(REAL,3,0:7) :: edge_vertex_gradient DEFAULT(ZERO)
     ! A (3 x 0:7) dimensioned list of the triangle vertex normals on
     ! each edge of the capping square (if that edge is needed).
     VEC(REAL,0:7) :: edge_mean_curvature DEFAULT(ZERO)
     ! A (0:7) dimensioned list of the mean curvatures on each edge of
     ! the capping square that crosses the isosurface (if that edge is
     ! needed).
     VEC(REAL,0:7) :: edge_gaussian_curvature DEFAULT(ZERO)
     ! A (0:7) dimensioned list of the gaussian curvatures on each
     ! edge of the capping square that crosses the isosurface (if that
     ! edge is needed).
   end type
   type MARCHINGCUBE_TYPE
     MAT(REAL,3,0:7) :: vertex_positions DEFAULT(ZERO)
     ! A (3 x 0:7) dimensioned list of the actual coordinates of each
     ! cube vertex. Second index is the marching cubes vertex number.
     VEC(REAL,0:7) :: vertex_fn_value DEFAULT(ZERO)
     ! A (0:7) dimensioned list of the values of the function at each
     ! cube vertex.
     MAT(REAL,3,0:7) :: vertex_gradient DEFAULT(ZERO)
     ! A (3 x 0:7) dimensioned list of the vertex gradients at each
     ! cube vertex.  Second index is the marching cubes vertex number.
     MAT3(REAL,3,3,0:7) :: vertex_hessian DEFAULT(ZERO)
     ! A (3 x 3 x 0:7) dimensioned list of the hessian at each cube
     ! vertex. Third index is the marching cubes vertex number.
     REAL :: x_side_length DEFAULT(ZERO)
     ! The length of the x side of the cube
     REAL :: y_side_length DEFAULT(ZERO)
     ! The length of the y side of the cube
     REAL :: z_side_length DEFAULT(ZERO)
     ! The length of the z side of the cube
     MAT(REAL,3,3) :: axes DEFAULT(ZERO)
     ! The axes of the marching cube, if not usual x,y,z
     MAT(REAL,3,3) :: inverse_axes DEFAULT(ZERO)
     ! The inverse (transpose) of the axes of the marching cube
     REAL :: iso_value DEFAULT(ZERO)
     ! The isovalue to be used for the isosurface
     REAL :: accuracy DEFAULT(TOL(6))
     ! The function accuracy to which each isosurface point is
     ! determined
     INT :: case DEFAULT(-1)
     ! The case number of this marching cube, for lookup in edge_table
     ! and triangle_table
     INT :: interior_case DEFAULT(-1)
     ! The case number corresponding to a cube being wholly within a
     ! surface.  Normally this is when the interior of the surface has
     ! larger values than the exterior, and in this case the default is 0.
     INT :: exterior_case DEFAULT(-1)
     ! The case number corresponding to a cube being wholly outside a
     ! surface.  Normally this is when the interior of the surface has
     ! smaller values than the exterior, and in this case the default
     ! is 255.
     INT :: edge_bit_string DEFAULT(0)
     ! The edge bit string for the marching cube. This is just the
     ! appropriate element of the edge_table i.e. if the bit is set,
     ! then that edge crosses the isosurface and must be interpolated.
     INT :: skip_bit_string DEFAULT(0)
     ! A bit string which tells whether to skip processing certain cube
     ! edges.  Can be used when certain edge vertex points are already
     ! done.
     INT :: cube_bit_string DEFAULT(0)
     ! The grad bit string for the marching cube. This tells which
     ! *vertices* of the cube cross the isosurface and must be
     ! interpolated.
     INT :: n_edge DEFAULT(0)
     ! The number of marching cube edges that need to be considered on
     ! this marching cube
     INT :: n_active_edge DEFAULT(0)
     ! The number of active marching cube edges that need to be
     ! considered on this marching cube, taking into account the
     ! skip_bit_string
     INT :: n_triangle DEFAULT(0)
     ! The number of triangles formed on this marching cube
     INT :: n_pt DEFAULT(0)
     ! The number of points up until this cube was analyzed. Used to
     ! keep a list of unique indices to define triangle vertices and
     ! points.
     MAT(INT,3,5) :: triangle_edge_index DEFAULT(0)
     ! The (3 x .n_triangle) dimensioned list of 3-edges on the
     ! marching cube used to form triangles. This is just the
     ! appropriate row
     ! from the triangle_table. The maximum number of triangles is 5.
     MAT(INT,3,5) :: triangle_vertex_index DEFAULT(0)
     ! The (3,.n_triangle) dimensioned list of groups of 3 *unique*
     ! triangle vertex *indices*. These indices point to a unique list
     ! of points. The maximum number of triangles is 5.
     MAT(REAL,3,0:11) :: edge_vertex_positions DEFAULT(ZERO)
     ! A (3 x 0:11) dimensioned list of the interpolated triangle vertex
     ! coordinates on each edge of the cube (if that edge is needed).
     VEC(REAL,0:11) :: edge_vertex_value DEFAULT(ZERO)
     ! A (0:11) dimensioned list of the interpolated vertex values
     ! normals on each edge of the cube (if that edge is needed).
     MAT(REAL,3,0:11) :: edge_vertex_gradient DEFAULT(ZERO)
     ! A (3 x 0:11) dimensioned list of the interpolated triangle vertex
     ! normals on each edge of the cube (if that edge is needed).
     MAT3(REAL,3,3,0:11) :: edge_vertex_hessian DEFAULT(ZERO)
     ! A (3 x 3 x 0:11) dimensioned list of the interpolated triangle
     ! vertex hessians on each edge of the cube (if that edge is
     ! needed).
     VEC(REAL,0:11) :: edge_mean_curvature DEFAULT(ZERO)
     ! A (0:11) dimensioned list of the (interpolated) mean curvatures
     ! on each edge of the cube that crosses the isosurface (if that
     ! edge is needed).
     VEC(REAL,0:11) :: edge_gaussian_curvature DEFAULT(ZERO)
     ! A (0:11) dimensioned list of the (interpolated) gaussian
     ! curvatures on each edge of the cube that crosses the isosurface
     ! (if that edge is needed).
     VEC(INT,0:12) :: edge_vertex_index DEFAULT(0)
     ! A (0:11) dimensioned list of the *unique* triangle vertex
     ! *indices* for each edge of the marching cube (if that edge is
     ! needed). NOTE: The 12-th component is used for re-use counting.
     VEC(INT,12) :: vertex_edge_index DEFAULT(0)
     ! A list of the edge indices for each unique triangle vertex index.
     ! Essentially, this is the reverse mapping of edge_vertex_index.
   end type
   type ISOSURFACE_TYPE
     STR(len=STR_SIZE) :: property DEFAULT(" ")
     ! The kind of isosurface plot, if known. This helps in deciding
     ! which way the normals of the isosurface should point.
     STR(len=STR_SIZE) :: triangulation_method DEFAULT(" ")
     ! The method used to triangulate the isosurface.
     REAL :: iso_value DEFAULT(ZERO)
     ! The isovalue to be used for the isosurface
     VEC(REAL,:), allocatable :: iso_values
     ! List of *multiple* isovalues to be used for the isosurfaces
     type(PLOT_GRID_TYPE) :: plot_grid
     ! The isosurface plotgrid
     INT :: n_pt DEFAULT(0)
     ! The number of isosurface points
     MAT(REAL,:,:), allocatable :: point
     ! A (3 x n_pt) list of points on the isosurface
     INT :: n_face DEFAULT(0)
     ! The number of triangulated faces on the isosurface
     MAT(INT,:,:), allocatable :: face
     ! A (3 x n_face) list of the triangular faces of the surface.
     ! Each face is represented by three integers which move in an
     ! anticlockwise direction when viewed from the outside.
     VEC(type(EVEC_INT_TYPE),:), allocatable :: neighbour
     ! The linked list of neighbour vertices for each vertex.
     INT :: n_cap DEFAULT(0)
     ! The number of triangulated faces making up the caps
     MAT(INT,:,:), allocatable :: cap
     ! A (3 x n_cap) list of the triangular faces of the capping
     ! region. Each face is represented by three integers which move
     ! in an anticlockwise direction when viewed from the outside.
     MAT(REAL,:,:), allocatable :: point_gradient
     ! A (3 x n_pt) list of the function gradient vectors for every
     ! point on the isosurface
     VEC(REAL,:), allocatable :: point_mean_curvature
     ! A (3 x n_pt) list of the mean surface curvatures for every
     ! point on the isosurface
     VEC(REAL,:), allocatable :: point_gaussian_curvature
     ! A (3 x n_pt) list of the gaussian surface curvatures for every
     ! point on the isosurface
     REAL :: area DEFAULT(ZERO)
     ! The area of the isosurface, calculated from the triangulate
     ! isosurface
     REAL :: volume DEFAULT(ZERO)
     ! The volume of the isosurface, calculated from the triangulate
     ! isosurface.
     REAL :: an_volume DEFAULT(ZERO)
     ! The volume of the isosurface, calculated from the triangulate
     ! isosurface by using the average face normal for each triangle
     ! face. The average face normal is calculated from the average of
     ! the norms of the point_gradient's at each surface point.
     REAL :: volume_min DEFAULT(ZERO)
     ! A lower bound to the interior volume of the isosurface,
     ! calculated by counting the number of cubes inside the surface.
     REAL :: volume_max DEFAULT(ZERO)
     ! An upper bound to the interior volume of the isosurface,
     ! calculated by counting the number of cubes inside and on the
     ! surface.
     INT :: n_skip DEFAULT(0)
     ! The number of function evaluations skipped (recursive method
     ! only) This is in fact worked out by counting the total number
     ! of evaluations.
     INT :: level DEFAULT(0)
     ! The current level to which the initial box has been divided, in
     ! the recursive marching cube algorithm.
     INT :: final_level DEFAULT(0)
     ! The final level to which the initial box must be divided, in the
     ! recursive marching cube algorithm.
     INT :: scan_level DEFAULT(0)
     ! The level to which the initial box must be divided, in the
     ! recursive marching cube algorithm, to achieve resolution of all
     ! important features.  It is determined by the
     ! .minimum_scan_division setting; or if this is smaller than the
     ! .minimum_scan_level, that value is used instead.
     INT :: minimum_scan_level DEFAULT(ISOSURFACE_MINIMUM_SCAN_LEVEL)
     ! The smallest acceptable scan level used in the recursive
     ! marching cubes algorithm.
     REAL :: minimum_scan_division DEFAULT(ZERO)
     ! The minimum acceptable voxel side length to which the initial
     ! box must be divided, in the recursive marching cube algorithm,
     ! to achieve resolution of all important features. The
     ! minimum_scan_level (above) may override this setting.
     REAL :: voxel_proximity_factor DEFAULT(ISOSURFACE_VOXEL_PROXIMITY_FACTOR)
     ! Setting this value determines when a voxel is deemed to be
     ! "close" to the isosurface, and hence should be subdivided. It
     ! works by seeing if the .iso_value is within this factor times
     ! the difference between the maximum and minimum values on the
     ! voxel vertices. If it is, then the voxel is "close" to the
     ! surface.
     type(MARCHINGCUBE_TYPE) :: cube
     ! The marching cube used to generate the isosurface
     type(CAPPING_SQUARE_TYPE) :: cap_square
     ! The cap square used to generate the caps on the boundary of the
     ! plot grid
     REAL :: del_x DEFAULT(ONE)
     ! The current box x side length in the recursive marching cubes
     ! algorithm.
     REAL :: del_y DEFAULT(ONE)
     ! The current box y side length in the recursive marching cubes
     ! algorithm.
     REAL :: del_z DEFAULT(ONE)
     ! The current box z side length in the recursive marching cubes
     ! algorithm.
     INT :: x DEFAULT(0)
     ! The (partial) x-coordinate of the box in the recursive marching
     ! cubes algorithm.  This is a binary number, with each bit from
     ! the right representing a segment double the size to which the
     ! current box belongs.
     INT :: y DEFAULT(0)
     ! The (partial) y-coordinate of the box in the recursive marching
     ! cubes algorithm.  This is a binary number, with each bit from
     ! the right representing a box double the size to which the
     ! current box belongs.
     INT :: z DEFAULT(0)
     ! The (partial) z-coordinate of the box in the recursive marching
     ! cubes algorithm.  This is a binary number, with each bit from
     ! the right representing a box double the size to which the
     ! current box belongs.
     type(MAP_VEC_INT_VEC_INT_TYPE), allocatable :: map
     ! A map table storing marching cube edge_vertex_index information
     ! as a function of the cube coordinates, [.x,.y,.z]
     BIN :: small_map DEFAULT(FALSE)
     ! Set TRUE if you want to prune dead elements from the map table
     BIN :: big_interior DEFAULT(FALSE)
     ! Set TRUE if the interior of the isosurface is bigger than the
     ! exterior, i.e. if the point_normals are to be reversed on
     ! output. This switch could probably be determined automatically,
     ! assuming the botton left had corner of the plot (the first
     ! point) was "outside".
     INT :: cap_ends DEFAULT(0)
     ! Set non-zero if you want the ends of the isosurface on the
     ! bounding cube to be capped. Set less than one if you want values
     ! less than the isosurface capped, or greater than one if you want
     ! values greater than the iso value capped.
     BIN :: suppress_caps DEFAULT(FALSE)
     ! If TRUE caps are not outputed in put_faces even if they have
     ! been generated.
     STR(len=STR_SIZE) :: surface_property DEFAULT(" ")
     ! The name of a surface property to plot or calculate on the
     ! isosurface surface.
     VEC(REAL,:), allocatable :: surface_property_values
     ! The values of .surface_property, evaluated on the isosurface
     ! surface.
     VEC(REAL,:), allocatable :: surface_iso_values
     ! The values of the iso_value on the surface, in the case when
     ! multiple isosurfaces have been calculated and merged together
     ! into a single surface.
     VEC(REAL,3) :: surface_point DEFAULT(ZERO)
     ! A special point which lies on or near the isosurface, used for
     ! calculations of connected area, for example.
     BIN :: chop_surface_property DEFAULT(FALSE)
     ! When plotting a surface, whether to chop the surface property
     ! accorsding to the lower and upper bounds specified below,
     ! rather than scaling the smallest value to zero and the largest
     ! to one.
     REAL :: surface_property_lb DEFAULT(ISOSURFACE_PROPERTY_LB)
     ! A lower bound to the property value, for calculating surface
     ! areas, and for chopping the range of surface property values.
     REAL :: surface_property_ub DEFAULT(ISOSURFACE_PROPERTY_UB)
     ! A upper bound to the property value, for calculating surface
     ! areas, and for chopping the range of surface property values.
     REAL :: surface_prop_lb_abs DEFAULT(ISOSURFACE_PROPERTY_LB_ABS)
     ! A lower bound to the absolute value of the property, for
     ! calculating the log of the surface property values.
     REAL :: surface_prop_ub_abs DEFAULT(ISOSURFACE_PROPERTY_UB_ABS)
     ! A upper bound to the absolute value of the property, for
     ! calculating the log of the surface property values.
     type(COLOUR_FUNCTION_TYPE), allocatable :: colour
     ! A colourfunction used for colouring the isourface
     VEC(type(ATOM_TYPE),:), allocatable :: atom
     ! A list of atoms associated with the isosurface.
     ! *Pointer copied*.
     type(INTERPOLATOR_TYPE), allocatable :: interpolator
     ! This is used for setting defaults for isosurfaces which make
     ! use of spherical atomiuc densities.
     BIN :: CX_output_fingerprint DEFAULT(FALSE)
     ! Set TRUE to output the fingerprint data during a put_CX call.
     INT :: CX_fingerprint_grid DEFAULT(2)
     ! Sets the resolution of the fingerprint binning grid
     BIN :: CX_output_distance_properties DEFAULT(FALSE)
     ! Set TRUE to output di, de and dnorm properties during put_CX call.
     BIN :: CX_output_shape_properties DEFAULT(FALSE)
     ! Set TRUE to output shape index and curvedness properties during
     ! put_CX call.
     INT :: hirshfeld_power_factor DEFAULT(1)
     ! Raise densities to this power for generation of Hirshfeld
     ! surfaces
   end type

!  =================================
!  Wavefunction methods & treatments
!  =================================
   type SCF_DATA_TYPE

     ! ========
     ! SCF kind
     ! ========
     STR(len=STR_SIZE) :: SCF_kind DEFAULT(" ")
     ! The kind of SCF calculation to perform

     ! ====
     ! ELMO
     ! ====
     BIN :: use_NOMO DEFAULT(FALSE)
     ! Whether to use non-orthogonal MO's of any type
     BIN :: use_ELMO DEFAULT(FALSE)
     ! Whether to use extremely localised MO's of any type
     BIN :: use_ELMO_01 DEFAULT(FALSE)
     ! Whether to use ELMO(0,1) as defined by Davidson & Jayatilaka
     BIN :: use_ELMO_11 DEFAULT(FALSE)
     ! Whether to use ELMO(1,1) as defined by Davidson & Jayatilaka
     INT :: elmo_b DEFAULT(0)
     ! Degree of connectivity for ELMO basis functions
     INT :: elmo_p DEFAULT(1)
     ! Degree of connectivity for ELMO particle interactions

     ! ======================
     ! DFT functional options
     ! ======================
     STR(len=STR_SIZE) :: DFT_functional DEFAULT(" ")
     ! The whole DFT functional, if it was set by one name
     BIN :: DFT_parts_given DEFAULT(FALSE)
     ! TRUE if the exchange or correlation functional was given in input
     STR(len=STR_SIZE) :: DFT_exchange_functional DEFAULT("becke88")
     ! The DFT exchange functional to be used
     STR(len=STR_SIZE) :: DFT_correlation_functional DEFAULT("lyp")
     ! The DFT correlation functional to be used
     BIN :: using_GGA_exchange DEFAULT(TRUE)
     ! TRUE if the exchange functional uses the generalized gradient
     ! approximation, otherwise false.
     BIN :: using_meta_GGA_exchange DEFAULT(FALSE)
     ! TRUE if the exchange functional uses the kinetic energy density
     BIN :: using_meta_GGA_correlation DEFAULT(FALSE)
     ! TRUE if the correlation functional uses the kinetic energy density
     BIN :: using_hybrid_exchange DEFAULT(FALSE)
     ! TRUE if the exchange functional uses a mixture of exact exchange
     ! and exchange functionals
     REAL :: hybrid_exchange_factor DEFAULT(0.2d0)
     ! Amount of hybrid exchange to use for hybrid DFT functionals
     BIN :: using_GGA_correlation DEFAULT(TRUE)
     ! TRUE if the correlation functional uses the generalized
     ! gradient approximation, otherwise false.

     ! ==================
     ! Iteration counters
     ! ==================
     INT :: iteration DEFAULT(0)
     ! The interation count for the SCF procedure
     INT :: total_iterations DEFAULT(0)
     ! The total interation count for the SCF procedure, which does
     ! not get reset after each lambda increment, in the case of a
     ! constrained SCF procedure.
     INT :: lambda_iteration DEFAULT(0)
     ! Lambda iteration count for the constrained SCF procedure

     ! ==========
     ! SCF timers
     ! ==========
     ! CPU seconds accumulated by MOLECULE.SCF; zeroed in initialize_SCF.
     ! Printed as a column of the iteration table and in the results.
     REAL :: time_1e DEFAULT(ZERO)
     ! Overlap and core matrices
     REAL :: time_grid DEFAULT(ZERO)
     ! DFT grid construction
     REAL :: time_guess DEFAULT(ZERO)
     ! The initial guess
     REAL :: time_fock_2e DEFAULT(ZERO)
     ! J and K builds, two-electron integrals included
     REAL :: time_xc DEFAULT(ZERO)
     ! The exchange-correlation matrix and energy
     REAL :: time_diis DEFAULT(ZERO)
     ! Fock matrix extrapolation
     REAL :: time_diag DEFAULT(ZERO)
     ! Updating the MOs (diagonalisation)
     REAL :: time_density DEFAULT(ZERO)
     ! Making the density matrix
     REAL :: time_iteration DEFAULT(ZERO)
     ! CPU seconds of the last iteration, for the table
     INT :: XC_grid_points DEFAULT(0)
     ! Points in the molecular DFT grid, for the timing summary
     INT :: XC_grid_points_unpruned DEFAULT(0)
     ! The same before pruning by the promolecule density
     INT :: min_iterations DEFAULT(1)
     ! The minimum number of SCF iterations to perform
     INT :: max_iterations DEFAULT(100)
     ! The maximum number of SCF iterations to perform

     ! ===============
     ! Stored energies
     ! ===============
     REAL :: nuclear_repulsion_energy DEFAULT(ZERO)
     ! Nuclear repulsion energy for the associated molecule
     REAL :: nuclear_attraction_energy DEFAULT(ZERO)
     ! Electron nuclear attraction energy for the associated molecule
     REAL :: charge_attraction_energy DEFAULT(ZERO)
     ! point-charge attraction energy for the associated molecule
     REAL :: electron_repulsion_energy DEFAULT(ZERO)
     ! Electron-electron repulsion energy for the associated molecule
     REAL :: kinetic_energy DEFAULT(ZERO)
     ! Electron kinetic energy for the associated molecule
     REAL :: DFT_energy_correction DEFAULT(ZERO)
     ! The DFT energy correction to the SCF energy for the associated
     ! molecule
     REAL :: XC_energy DEFAULT(ZERO)
     ! The exchange-correlation energy E_xc of a DFT calculation
     REAL :: exact_exchange_energy DEFAULT(ZERO)
     ! a_0 E_x, the scaled exact exchange energy of a hybrid functional
     REAL :: DFT_alpha_energy_correction DEFAULT(ZERO)
     ! The DFT alpha energy correction to the SCF energy for the
     ! associated molecule
     REAL :: DFT_beta_energy_correction DEFAULT(ZERO)
     ! The DFT beta energy correction to the SCF energy for the
     ! associated molecule
     REAL :: E_initial DEFAULT(ZERO)
     ! The initial-guess SCF energy

     ! Current energies
     REAL :: energy DEFAULT(ZERO)
     ! The SCF energy for the associated molecule
     REAL :: old_energy DEFAULT(ZERO)
     ! The SCF energy from the previous SCF cycle
     REAL :: delta DEFAULT(ZERO)
     ! The change in the main SCF variable (usually the energy)
     ! between cycles

     ! ====================
     ! Constrained SCF data
     ! ====================
     REAL :: lambda DEFAULT(ZERO)
     ! The initial lambda value to use in a constrained SCF procedure
     REAL :: initial_lambda DEFAULT(ZERO)
     ! The starting lambda value in a constrained SCF procedure
     REAL :: lambda_max DEFAULT(ZERO)
     ! The maximum lambda value to use in a constrained SCF procedure
     REAL :: lambda_step DEFAULT(ONE)
     ! The value to step the lambda value between lambda cycles in a
     ! constrained SCF calc.
     REAL :: fit_value DEFAULT(ZERO)
     ! The value of E + lambda*penalty in a constrained SCF calc.
     ! The penalty can be GoF2, or a combination of GoF2's if more 
     ! than one type of diffraction data is to be fitted.
     REAL :: old_fit_value DEFAULT(ZERO)
     ! The value of E + lambda*penalty of the *previous* iteration 
     ! in an XCW calc.
     REAL :: penalty DEFAULT(ZERO)
     ! Penalty statistic for a constrained SCF calc
     REAL :: old_penalty DEFAULT(ZERO)
     ! Penalty statistic for a constrained SCF calc of the previous
     ! iteration
     REAL :: overlap_with_M0s DEFAULT(ONE)
     ! Overlap of the current lambda>0 MOs with the reference M0s.
     ! For a RESTART the reference is this job's starting point, not lambda=0,
     ! so the value is not comparable across a restart boundary. See M0s.
     BIN :: put_constraint_stiffness DEFAULT(FALSE)
     ! Print the constraint stiffness report at each converged lambda
     BIN :: use_stiffness_correction DEFAULT(FALSE)
     ! Correct the overshoot of each constrained SCF step in the space
     ! of reflections, see MOLECULE.SCF:correct_constraint_overshoot
     BIN :: use_matrix_free_stiffness DEFAULT(FALSE)
     ! Never form the derivatives or the gain matrix: products through a
     ! structure factor evaluation and a constraint build, conjugate
     ! gradients for the correction, a trace estimator for p_eff
     REAL :: stiffness_cg_tolerance DEFAULT(1.0d-4)
     ! Relative residual at which the conjugate gradient solve stops
     INT :: stiffness_samples DEFAULT(16)
     ! Sign vectors in the trace estimator, for the part of p_eff and the
     ! leverages outside the deflation vectors
     INT :: stiffness_cg_iterations DEFAULT(0)
     ! Conjugate gradient iterations of the last solve
     STR(len=STR_SIZE) :: stiffness_preconditioner DEFAULT("none")
     ! Preconditioner of the conjugate gradients: none or diagonal
     INT :: stiffness_deflation DEFAULT(20)
     ! Leading eigenvectors of the gain matrix used, in the matrix-free
     ! form, to deflate the conjugate gradients and to give the stiff part
     ! of p_eff and the leverages exactly; 0 is none
     MAT(REAL,:,:), allocatable :: deflation_W
     ! Those eigenvectors, by column, from Lanczos
     VEC(REAL,:), allocatable :: deflation_theta
     ! Their eigenvalues, gains per unit lambda, largest last
     INT :: deflation_lambda_iteration DEFAULT(-1)
     ! The lambda iteration they were made at
     VEC(REAL,:), allocatable :: stiffness_precond
     ! The diagonal of the gain matrix, when that preconditioner is used
     INT :: stiffness_precond_lambda_iteration DEFAULT(-1)
     ! The lambda iteration the preconditioner was made at
     REAL :: gamma_max DEFAULT(ZERO)
     ! The largest gain per unit lambda
     BIN :: stiffness_physical_gaps DEFAULT(FALSE)
     ! Take the level shift off the virtual orbital energies in the gaps:
     ! set for the statistics at convergence, not for the correction
     INT :: stiffness_lambda_iteration DEFAULT(-1)
     ! The lambda iteration the stiffness data below were made at
     BIN :: stiffness_made DEFAULT(FALSE)
     ! The stiffness statistics below exist
     VEC(REAL,:), allocatable :: stiffness_eval
     ! Eigenvalues of the gain matrix, per unit lambda
     MAT(REAL,:,:), allocatable :: stiffness_evec
     ! Its eigenvectors, by column
     MAT(REAL,:,:), allocatable :: stiffness_B
     ! The weighted structure factor derivatives over orbital rotations,
     ! reflections by occupied-virtual pairs; G = (2/(N-p)) B B^T
     VEC(REAL,:), allocatable :: stiffness_diag
     ! Diagonal of the gain matrix: each reflection's own gain
     VEC(REAL,:), allocatable :: hat_diag
     ! Diagonal of the hat matrix: each reflection's leverage
     VEC(REAL,:), allocatable :: fit_residual
     ! (F_pred - F_exp)/sigma when the stiffness was last made
     REAL :: p_eff DEFAULT(ZERO)
     ! Effective number of fitted parameters
     REAL :: chi2_fit DEFAULT(ZERO)
     ! Sum of squared standardised residuals
     REAL :: AIC_sigma DEFAULT(ZERO)
     ! N ln(chi2/N) + 2 p_eff
     REAL :: GCV DEFAULT(ZERO)
     ! Generalised cross-validation, N chi2/(N-p_eff)^2
     REAL :: LOO_sum DEFAULT(ZERO)
     ! Leave-one-out sum of squared residuals
     VEC(REAL,:), allocatable :: lambda_history
     ! Converged lambda values, for the TIH extrapolation
     VEC(REAL,:), allocatable :: GoF2_history
     ! GoF^2 at each converged lambda
     BIN :: TIH3_made DEFAULT(FALSE)
     ! The three-point TIH extrapolation below exists
     REAL :: TIH3_D DEFAULT(ZERO)
     ! Constant term of GoF^2 = D + E/lambda^2 + F/lambda^4
     REAL :: TIH3_E DEFAULT(ZERO)
     ! Its 1/lambda^2 coefficient
     REAL :: TIH3_F DEFAULT(ZERO)
     ! Its 1/lambda^4 coefficient
     REAL :: lambda_TIH3 DEFAULT(ZERO)
     ! |F/D|^(1/4), the three-point TIH halting lambda
     STR(len=STR_SIZE) :: lambda_criterion DEFAULT("loo")
     ! The criterion that chooses lambda: gcv, aic_sigma, aic, bic or loo
     VEC(REAL,:), allocatable :: criterion_history
     ! The chosen criterion at each converged lambda
     REAL :: lambda_opt DEFAULT(ZERO)
     ! The lambda with the smallest criterion so far
     REAL :: criterion_min DEFAULT(ZERO)
     ! The criterion at lambda_opt
     BIN :: zero_oo_constraint_block DEFAULT(FALSE)
     ! Zero the occ-occ block of the constraint matrix. This should
     ! not affect the answer but only the canonicalisation

   ! crystal :: CRYSTAL@ 
   ! ! The crystal information for the diffraction experiment

     ! =====================
     ! Initial guess options
     ! =====================
     STR(len=STR_SIZE) :: initial_density DEFAULT("core")
     ! The kind of density matrix to start the SCF calc
     STR(len=STR_SIZE) :: initial_MOs DEFAULT("--using density--")
     ! The kind of initial MO's to start the SCF calc
     BIN :: restart_SCF DEFAULT(FALSE)
     ! This is set TRUE if restarting from density *or* MOs
     STR(len=STR_SIZE) :: initial_Us DEFAULT(" ")
     ! The kind of initial U matrices to start the CPHF from
     BIN :: use_default_guess_options DEFAULT(TRUE)
     ! Use the default SCF options for promolecule or group SCF
     ! initial guess (except that output is FALSE); otherwise use
     ! current SCF_data options for promolecule/group SCF guess.
     MAT(INT,:,:), allocatable :: initial_atomic_states
     ! A (nx2) matrix which holds the charges and multiplicities of
     ! the n unique atoms in the molecule (as presented by the atom
     ! order), used for nondefault ANO & promolecule initial guess.

     ! ===============
     ! SCF convergence
     ! ===============
     REAL :: convergence DEFAULT(TOL(3))
     ! Required convergence on delta E in the SCF
     REAL :: guess_convergence DEFAULT(TOL(8))
     ! Required convergence for a *guess* SCF -- the atomic calculations
     ! behind the ANOs, promolecule and Hirshfeld weights. Kept separate
     ! from .convergence: those are an internal reference, not the user's
     ! answer, so a loose tolerance on the main SCF must not silently
     ! degrade them.
     BIN :: delta_E_converged DEFAULT(FALSE)
     ! Whether delta E has converged; for reporting margin notes.
     INT :: damp_finish DEFAULT(3)
     ! Iteration when density matrix damping is turned off
     REAL :: damp_factor DEFAULT(0.5d0)
     ! The damping factor to use
     BIN :: damping_on DEFAULT(FALSE)
     ! Whether damping was on last iteration; for margin notes output.
     BIN :: damping_off DEFAULT(FALSE)
     ! Whether damping was off last iteration; for margin notes
     ! output.
     INT :: level_shift_finish DEFAULT(3)
     ! Iteration when level shifting is turned off
     REAL :: level_shift DEFAULT(0.3d0)
     ! Value to level shift the virtual orbitals
     BIN :: level_shift_on DEFAULT(FALSE)
     ! Whether level shift was on last iteration; for margin notes
     ! output
     BIN :: level_shift_off DEFAULT(FALSE)
     ! Whether level shift was off last iteration; for margin notes
     ! output

     ! =================
     ! Linear dependence
     ! =================
     REAL :: linear_dependence_tol DEFAULT(TOL(5))
     ! Eigenvalues of the overlap matrix smaller than this value are
     ! level shifted.
     REAL :: linear_dependence_shift DEFAULT(1.0d12)
     ! A level shift to apply to eigenvalues of the overlap matrix
     ! which are smaller than the above tolerance. If tolerance is
     ! zero, no shifting is done.
     REAL :: min_overlap_mx_eigenvalue DEFAULT(ZERO)
     ! Minimum overlap matrix eigenvalue
     INT :: n_small_S_eigenvalues DEFAULT(0)
     ! Number of small overlap matrix eigenvalues
     INT :: n_neg_S_eigenvalues DEFAULT(0)
     ! Number of negative overlapm matrix eigenvalues

     ! ========
     ! Accuracy
     ! ========
     REAL :: MO_gradient_stepsize DEFAULT(0.01d0)
     ! The stepisize to use for updating the orbitals
     REAL :: max_update_stepsize DEFAULT(0.01d0)
     ! The maximum update stepsize to use for updating any SCF object,
     ! e.g. the orbitals
     REAL :: ERI_disk_cutoff DEFAULT(TOL(12))
     ! Cutoff for the two electron integrals, on disk
     REAL :: ERI_Schwarz_cutoff DEFAULT(TOL(9))
     ! Cutoff for the two electron integrals
     REAL :: ERI_J_density_cutoff DEFAULT(TOL(9))
     ! Cutoff for the J density matrix elements in Schwarz test
     REAL :: ERI_K_density_cutoff DEFAULT(TOL(9))
     ! Cutoff for the K density matrix elements in Schwarz test
     REAL :: ERI_bf_overlap_cutoff DEFAULT(TOL(9))
     ! The cutoff for the ERI Schwarz test.
     REAL :: ERI_primitive_pair_cutoff DEFAULT(TOL(9))
     ! The cutoff for the exponential prefactor for the product of two
     ! gaussian primitives, below which that primitive pair is ignored
     ! in the calculation of the ERI's.
     STR(len=STR_SIZE) :: ERI_accuracy DEFAULT("low")
     ! Named accuracy level setting the four ERI cutoffs together
     BIN :: escalate_ERI_accuracy DEFAULT(FALSE)
     ! Whether to use "very_low" ERI cutoffs while the density is damped
     BIN :: ERI_accuracy_given DEFAULT(FALSE)
     ! Whether the named accuracy levels are in charge of the ERI cutoffs
     REAL :: atom_overlap_cutoff DEFAULT(9)
     ! The value of the maximum overlap between basis functions on two
     ! different atoms, below which the atoms are considered *not*
     ! overlapping. This is used to defined the .overlapping_atoms
     ! array used in linear scaling DFT matrix
     ! element evaluation.
     REAL :: XC_atom_separation_cutoff DEFAULT(15.0d0)
     ! The exchange correlation matrix in DFT calculations is *not*
     ! calculated for basis functions on atoms separated by *more*
     ! than this distance.  This allows linear scaling of the exchange
     ! correlation terms (except for a negligible N^2 contribution due
     ! to the atom-distance test).

     ! ==================
     ! Modify Fock matrix
     ! ==================
     REAL :: ee_adiabatic_constant DEFAULT(ONE)
     ! Constant to multiply the V_ee interactions, for adiabatic
     ! coupling calculations.
     BIN :: use_Fermi_Amaldi DEFAULT(FALSE)
     ! Whether to use Fermi-Amaldi in the SCF procedure
     ! (ignores exchange)
     STR(len=STR_SIZE) :: VMO_canonisation DEFAULT(" ")
     ! Method to use to canonicalize the virtual MOs.
     ! Works by removing an occupied MO from the density.
     ! Allowed are "Hunt-Goddard-1", "Hunt-Goddard-3",
     ! "Huzinaga-Arnau", or "Exchange". The former produce 
     ! singlet & triplet canonicalizations, the latter, the 
     ! average. "Exchange" canonicalises with the K matrix.
     INT :: VMO_canonisation_MO DEFAULT(0)
     ! Index of the MO to be removed from the Fock matrix in 
     ! VMO canonicalization. If a -ve number "n" is input then
     ! the MO to be removed is assumed to be the HOMO-n.
     VEC(REAL,:), allocatable :: VMO_energies
     ! The new VMO canonicalised MO energies. Note that the 
     ! MO_energies in MOLECULE are also re-canonicalised with 
     ! the usual Fock matrix.

     ! ================================
     ! DIIS convergence related options
     ! ================================
     type(DIIS_TYPE) :: SCF_DIIS
     ! For SCF (normally Fock matrix) DIIS extrapolation.
     ! Should we make this allocatable?
     VEC(type(DIIS_TYPE),:), allocatable :: CP_DIIS
     ! For coupled-perturbed DIIS extrapolation
     REAL :: DIIS_error DEFAULT(ZERO)
     ! The genetric DIIS error for iteration control
     REAL :: old_DIIS_error DEFAULT(ZERO)
     ! The generic DIIS error of the previous iteration
     REAL :: DIIS_error_temp_cutoff DEFAULT(TOL(2))
     ! The DIIS error at which pFON fractional occupation numbers are
     ! turned off and the temperature is set to zero.
     BIN :: DIIS_converged DEFAULT(FALSE)
     ! Whether some DIIS object has has decided it converged last
     ! iteration; for margin notes output.
     BIN :: DIIS_saving DEFAULT(FALSE)
     ! Whether some DIIS object  has started saving last iteration;
     ! for margin notes output.
     BIN :: DIIS_started DEFAULT(FALSE)
     ! Whether DIIS has started last iteration; for margin notes.
     BIN :: DIIS_used_up DEFAULT(FALSE)
     ! Whether DIIS has used up all vectors; for margin notes.

     ! ======
     ! L BFGS
     ! ======
     type(L_BFGS_TYPE) :: bfgs
     ! For low memory BFGS extrapolation/minimization

     ! ==========================
     ! General Complex HF options
     ! ==========================
     REAL :: sl_1e_factor DEFAULT(ONE)
     ! Factor to apply to the 1-electron S.L terms
     REAL :: sl_2e_factor DEFAULT(ONE)
     ! Factor to apply to the 2-electron S.L terms
     VEC(REAL,3) :: quantization_axis DEFAULT(([ZERO,ZERO,ONE]))
     ! Quantization axis for GCHF, if using initial MO's as a guess

     ! ======================
     ! Finite temperature SCF
     ! ======================
     REAL :: temperature DEFAULT(ZERO)
     ! The electronic temperature
     REAL :: pFON_start_temperature DEFAULT(1000d0)
     ! The pFON temperature at iteration 0, in Kelvin
     REAL :: pFON_cooling_rate DEFAULT(50d0)
     ! The fall in the pFON temperature per iteration, in Kelvin
     INT :: pFON_finish DEFAULT(20)
     ! Iteration at which pFON is turned off whatever the DIIS error
     BIN :: pFON_on DEFAULT(FALSE)
     ! Whether pFON was on last iteration; for margin notes output.
     BIN :: pFON_off DEFAULT(FALSE)
     ! Whether pFON was off last iteration; for margin notes output.
     REAL :: E_Fermi DEFAULT(ZERO)
     ! The Fermi energy
     REAL :: Ea_Fermi DEFAULT(ZERO)
     ! The alpha-electron Fermi energy
     REAL :: Eb_Fermi DEFAULT(ZERO)
     ! The beta-electron Fermi energy

     ! ================
     ! Logical switches
     ! ================
     BIN :: do_outputs DEFAULT(FALSE)
     ! True if output is wanted for each iteration cycle of the XCW.
     ! this will messup the stdout, but will output cifs, fcf, fco and
     ! residual cube for every lambda step.
     BIN :: output DEFAULT(TRUE)
     ! True if output is wanted; sometimes we supress output e.g. when
     ! we do atomic SCF calculations to get a good initial guess, or
     ! if we are just not interested in the SCF output.
     BIN :: output_results DEFAULT(TRUE)
     ! True if SCF results output is wanted; for unit tests.  
     ! If *only* SCF results are wanted, set .output FALSE.
     BIN :: show_timings DEFAULT(FALSE)
     ! Print the CPU time per iteration in the SCF table and a per-phase
     ! summary in the results. Off by default: timings are not
     ! reproducible, so they must stay out of test references.
     STR(len=STR_SIZE) :: ANO_kind DEFAULT("average")
     ! How an open-shell ANO atom is made spherical: "average" (its density
     ! averaged after the SCF), "aoc" (HF average of configuration), "fon"
     ! (KS with held fractional occupations)
     BIN :: using_open_shell_ensemble DEFAULT(FALSE)
     ! TRUE while an atomic SCF runs as an open-shell ensemble (aoc or fon)
     INT :: n_open_orbitals DEFAULT(0)
     ! Orbitals in the open shell of that ensemble
     INT :: n_open_electrons DEFAULT(0)
     ! Electrons in the open shell of that ensemble
     REAL :: AOC_coupling DEFAULT(ONE)
     ! Roothaan's coupling constant a (= b) of the average of configuration
     REAL :: ensemble_energy_correction DEFAULT(ZERO)
     ! What to add to 1/2 tr[D(h+F)] for the average-of-configuration energy
     BIN :: guess_output DEFAULT(FALSE)
     ! TRUE if the output for the initial guess is to be printed.
     BIN :: is_guess DEFAULT(FALSE)
     ! TRUE if this SCF is an initial guess.
     BIN :: test DEFAULT(FALSE)
     ! Test flag. Set True if some test procedure is to be executed
     BIN :: stabilize_density DEFAULT(FALSE)
     ! Set TRUE if the density is to be averaged over the stabilizer
     ! group in xray constrained calculations (rhf only so far).

     ! ==================
     ! Relativity options
     ! ==================
     STR(len=STR_SIZE) :: relativity_kind DEFAULT("none")
     ! What kind of relativity are you using?
     BIN :: using_iotc DEFAULT(FALSE)
     ! Switch on/off the 1 electron IOTC terms

!    using_SO_coupling :: BIN  DEFAULT(FALSE)
     ! Switch on/off the 1 electron DKH terms
     BIN :: DK1_PCE DEFAULT(FALSE)
     ! Switch on/off the DK1_PCE terms
     BIN :: DK2_PCE DEFAULT(FALSE)
     ! Switch on/off the DK2_PCE terms
     BIN :: DK1 DEFAULT(TRUE)
     ! Switch on/off the DK1 terms
     BIN :: DK2 DEFAULT(TRUE)
     ! Switch on/off the DK2 terms
     BIN :: DK3 DEFAULT(FALSE)
     ! Switch on/off the DK3 terms
     BIN :: using_ppzpp DEFAULT(FALSE)
     ! switch on/off the PCE correction
     BIN :: PCE_CORR DEFAULT(FALSE)
     ! switch on/off the PCE correction
     BIN :: using_SO_FDFF DEFAULT(FALSE)
     ! Switch on/off the SO corrections to SO scalar terms
     ! which are missed in the works of Hess
     BIN :: using_1e_zora_term DEFAULT(FALSE)
     ! Switch on/off the 1 electron ZORA terms
     BIN :: using_pauli_terms DEFAULT(FALSE)
     ! Use the Pauli terms
     BIN :: using_1e_sl_term DEFAULT(FALSE)
     ! Switch on/off the 1 electron S.L term
     BIN :: using_1e_srxa_term DEFAULT(FALSE)
     ! Switch on/off the 1 electron diamagnetic term
     BIN :: using_2e_sl_term DEFAULT(FALSE)
     ! Switch on/off the 2 electron S.L terms
     BIN :: using_aa_term DEFAULT(FALSE)
     ! Switch on/off the A.A diamagnetic term
     BIN :: using_bl_term DEFAULT(FALSE)
     ! Switch on/off the B.L term (complex SCF reqd.)
     BIN :: using_bs_term DEFAULT(FALSE)
     ! Switch on/off the B.S term
     BIN :: using_bs_t_term DEFAULT(FALSE)
     ! Switch on/off the (B.S) T_e term

     ! =========================
     ! Convergence/Build options
     ! =========================
     BIN :: using_damping DEFAULT(TRUE)
     ! True if using damping
     BIN :: using_pFON DEFAULT(FALSE)
     BIN :: converged DEFAULT(FALSE)
     ! TRUE when the last SCF loop ended converged. Set by the SCF drivers
     ! when their loop exits; .is_converged is the live test.
     BIN :: die_if_not_converged DEFAULT(TRUE)
     ! Stop with an error when an SCF ends unconverged. FALSE lets the job
     ! go on with the unconverged orbitals, which it then says it is doing.
     ! True if using pFON
     BIN :: using_FON DEFAULT(FALSE)
     ! True if using finite temperatures
     BIN :: using_delta_build DEFAULT(FALSE)
     ! True if using an incremental (delta) fock build as advocated by
     ! Almlof: the two-electron Fock matrix is updated from the change
     ! in the density, so Schwarz screening sees the (small) delta.
     ! Off by default: with the normal screening cutoffs the increments
     ! are noisy at the 1e-6 level and cost iterations; measured
     ! 2026-09-12, docs/TASK_SCF_SPEEDUP_DATA.md. (It was TRUE for years but
     ! inert, nothing allocated the delta density.)
     INT :: delta_build_period DEFAULT(10)
     ! Rebuild the two-electron Fock matrix from the full density every
     ! this many iterations, so screening errors cannot accumulate.
     ! Zero or less means never.
     BIN :: using_batched_XC DEFAULT(TRUE)
     ! True to integrate the XC matrix batch by batch over the molecular
     ! grid; FALSE selects the old per-atom routines.
     BIN :: using_gaussian_pair_J DEFAULT(FALSE)
     ! True to build J from the primitive pair list (prototype).
     BIN :: using_RI_J DEFAULT(FALSE)
     ! Make J by density fitting in the auxiliary basis
     BIN :: using_COSX DEFAULT(FALSE)
     ! Make K by quadrature on a grid (chain of spheres)
     type(BECKE_GRID_TYPE), allocatable :: COSX_grid
     ! The grid for .using_COSX, from cosx_grid= { }
     type(BECKE_GRID_TYPE), allocatable :: COSX_final_grid
     ! The grid for the energy of the converged SCF, from cosx_final_grid= { }
     BIN :: using_COSX_final_grid DEFAULT(TRUE)
     ! Remake the energy of the converged SCF on .COSX_final_grid
     BIN :: using_COSX_overlap_fitting DEFAULT(TRUE)
     ! Correct the COSX quadrature so that it gives the exact overlaps
     BIN :: using_direct_SCF DEFAULT(TRUE)
     ! True if using direct SCF
     BIN :: using_DIIS DEFAULT(TRUE)
     ! True if using DIIS extrapolation for fock matrices
     BIN :: use_BFGS DEFAULT(TRUE)
     ! True if using L BFGS for minimization; currently only for NOMO
     BIN :: use_LSQ DEFAULT(FALSE)
     ! True if using least squares; currently only for ELMO
     BIN :: using_level_shift DEFAULT(TRUE)
     ! True if using level shifting
     BIN :: using_orbital_locking DEFAULT(FALSE)
     ! True if using orbital locking
     BIN :: using_exponential_update DEFAULT(FALSE)
     ! Set TRUE if using an antisymmetric exponential update
     BIN :: using_MO_gradient_update DEFAULT(FALSE)
     ! Set TRUE if using the gradient of the orbital coefficients to
     ! update the coefficients
     BIN :: using_NDDO DEFAULT(FALSE)
     ! Neglect of diatomic differential overlap
     BIN :: using_NUDO DEFAULT(FALSE)
     ! Neglect of unconnected differential overlap
     BIN :: using_PIE DEFAULT(FALSE)
     ! Projective integral expansion method (PIE) developed by Mayer.
     ! See Mayer, CPL 332, 381 (2000).

     ! =================
     ! Coupled-perturbed
     ! =================
     INT :: n_pert DEFAULT(0)
     ! No. of perturbations in CP equations
     BIN :: using_CP_DIIS DEFAULT(TRUE)
     ! True if using coupled-perturbed DIIS extrapolation

     ! ===================
     ! Cluster/crystal SCF
     ! ===================
     BIN :: restart_skip_asfs DEFAULT(FALSE)
     ! If set true then not only MOs and/or density matrices are
     ! assumed present, but atomic structure factors (ASFs) are
     ! assumed to be on disk. Use for efficient restart.
     REAL :: kappa DEFAULT(ZERO)
     ! The kappa value used to multiply the orthogonality projector,
     ! uses in an embedded SCF procedure
     BIN :: using_cluster_charges DEFAULT(FALSE)
     ! Set TRUE if you want to surround the molecule with charges.
     BIN :: using_SC_cluster_charges DEFAULT(FALSE)
     ! Set TRUE if you want to surround the molecule with *self
     ! consistent* charges.  These charges are reculculated every SCF
     ! iteration.
     BIN :: using_qq_cluster_charges DEFAULT(FALSE)
     ! Set TRUE if you want to surround the molecule with *self
     ! consistent* quadrupole charges. You still need to set
     ! use_sc_cluster_charges
     REAL :: cluster_radius DEFAULT(SIX)
     ! Radius of the cluster to use to generate cluster
     type(CLUSTER_TYPE), allocatable :: cluster
     ! A cluster used to get the cluster charges
     BIN :: defragment DEFAULT(TRUE)
     ! The defragment option in case of network compounds should be
     ! false Lorraine
     BIN :: save_cluster_charges DEFAULT(FALSE)
     ! Set TRUE if you want to save the cluster charges
     BIN :: save_20_cluster_charges DEFAULT(FALSE)
     ! Set TRUE if you want to save the cluster charges -- only about
     ! 20 of them are printed -- for testing
     VEC(REAL,:), allocatable :: cluster_charges
     ! List of saved cluster charges
     MAT(REAL,:,:), allocatable :: cluster_charge_positions
     ! List of saved cluster positions for the charges
     BIN :: using_SC_Lorentz_fields DEFAULT(FALSE)
     ! Set TRUE if you want to use local Lorentz fields from atomic
     ! point charges and point dipoles, summed to infinity using
     ! the Lorentz tensor.
     BIN :: delete_SCF_archives DEFAULT(TRUE)
     ! Delete all disk archived SCF data except for the MO's and
     ! density_mx after an SCF calculation. Keeping archives can
     ! save work later.
     BIN :: delete_all_archives DEFAULT(FALSE)
     ! Delete all archives, even the molecular orbitals.
     ! They will only exist in core. Saves disk writes.

     ! ===================
     ! SCF iteration table
     ! ===================
     VEC(type(EVEC_STR_TYPE),:), allocatable :: table_info
     ! A list of list of informational margin notes
     VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
     ! The SCF iteration table

     ! ===================
     ! Finite size nucleus
     ! ===================
     INT :: nhf DEFAULT(0)
     ! Change of the radius of nucleus for obtaining the
     ! numerical derivative of energy. Used?
   end type
   type ROBY_TYPE
     STR(len=STR_SIZE) :: roby_kind
     ! The kind of Roby calculation to perform

     ! =====
     ! Atoms
     ! =====
     VEC(INT,:), allocatable :: atom_list
     ! The indices of a list of roby atoms to be used in some way
     ! for population analysis
     VEC(type(EVEC_INT_TYPE),:), allocatable :: atom_group
     ! The indices of the roby atoms defining different spaces
     VEC(INT,:), allocatable :: atom_a
     ! The indices of the atoms defining space V_A
     VEC(INT,:), allocatable :: atom_b
     ! The indices of the atoms defining space V_B
     VEC(INT,:), allocatable :: atom_ab
     ! The indices of the roby atoms defining spaces V_A and V_B

     ! =======
     ! Options
     ! =======
     BIN :: is_hyper_analysis DEFAULT(FALSE)
     ! Whether the analysis is a hypervalent on, which 
     ! treats bonding from space .atom(A) to everything 
     ! *not* in any .atom(:)
     BIN :: analyze_all_atom_pairs DEFAULT(FALSE)
     ! If set TRUE, the bond index information is printed out ONLY for every
     ! atom pair which is considered "bonded". Otherwise all pairs of atoms are
     ! analyzed. This is onlyeffective when an atom_list is defined.
     BIN :: analyze_XH_bonds DEFAULT(FALSE)
     ! Analyse heteroatom-H bond indices. The H bonds are
     ! usually not very interesting.
     BIN :: analyze_vdw_atom_pairs DEFAULT(FALSE)
     ! ANalyze vdw bonded pairs only
     BIN :: show_calcs DEFAULT(FALSE)
     ! If TRUE show info when making theta calcs.
     VEC(type(EVEC_INT_TYPE),:), allocatable :: show_calc_groups
     ! List of group pairs for which info should be printed.
     BIN :: use_NAOs DEFAULT(FALSE)
     ! Set TRUE if you want to use NAO atomic projectors instead of
     ! ANO's as defined in the original Gould et al 2008 TCA paper.
     BIN :: use_spherical_averaging DEFAULT(TRUE)
     ! Set TRUE if you want to use spherically averaged NAOs.
     BIN :: use_projected_NAOs DEFAULT(FALSE)
     ! Set TRUE if you want to use projected NAO atomic projectors
     ! instead of NAOs.
     REAL :: bond_range_factor DEFAULT((0.4*BOHR_PER_ANGSTROM))
     ! Used to extend the range of the CCDC bonded test.
     REAL :: zero_angle_cutoff DEFAULT(TOL(2)*RADIAN_PER_DEGREE)
     ! Angles (in radians) *less* than this are regareded as zero
     REAL :: occupied_ANO_cutoff DEFAULT((ONE/14.0d0))
     ! Atomic natural orbitals with occupations less than this number
     ! are regarded as unoccupied. This number is used to define the Roby
     ! atomic projector
     BIN :: output_theta_info DEFAULT(TRUE)
     ! If set TRUE, the bond index information is printed out for every
     ! space V_theta. See the paper for details.
     INT :: n_dial_columns DEFAULT(4)
     ! Dials per row in the dial-diagram survey page
     BIN :: output_ci_latex_labels DEFAULT(FALSE)
     ! If set TRUE then this will output c and i bond indices
     ! above and below the bond when making RGBI pictures.
     ! Default is to output the total bond index and % covalency.

     ! =====
     ! Bonds
     ! =====
     VEC(REAL,:), allocatable :: n1
     ! Roby population for each atom-group
     MAT(REAL,:,:), allocatable :: n2
     ! Roby pair population for each atom-group pair
     ! NOTE: these are *not* shared populations
     ! The not atom-group populations
     REAL :: n_all DEFAULT(ZERO)
     ! Population of all the atoms
     REAL :: n_shared DEFAULT(ZERO)
     ! Roby multiple shared population for a specified atom-group
     MAT(REAL,:,:), allocatable :: cov_index
     ! Roby-Gould covalent bond indices for each pair of atoms
     MAT(REAL,:,:), allocatable :: ion_index
     ! Roby-Gould ionic bond indices for each pair of atoms
     MAT(REAL,:,:), allocatable :: bnd_index
     ! Roby-Gould *total* bond indices for each pair of atoms
     MAT(REAL,:,:), allocatable :: percent_covalency_P
     ! The % covalency of the given bond pair using the Pythagorean
     ! formula
     MAT(REAL,:,:), allocatable :: percent_covalency_A
     ! The % covalency of the given bond pair using the Araki angle
     ! formula

     ! =======
     ! Charges
     ! =======
     VEC(REAL,:), allocatable :: gould_charge
     ! Roby-Gould charges
     VEC(REAL,:), allocatable :: cruickshank_charge
     ! Cruikshank-Avramedes charges
     VEC(REAL,:), allocatable :: summed_n2
     ! Summed atom-group pair populations
     VEC(REAL,:), allocatable :: summed_n3
     ! Summed atom-group triple populations
     VEC(REAL,:), allocatable :: subgroup_pop
     ! The subgroup population contributions when calculating multiple shared
     ! populations.
     MAT(REAL,:,:), allocatable :: proportion_a
     ! The proportion to partition for atom A, between two atoms (A,B)
     ! using Gould's probabilistic scheme
     INT :: charge
     ! The total charge on the molecule

     ! ===================
     ! Theta subspace info
     ! ===================
     MAT(REAL,:,:), allocatable :: theta_C
     ! The matrix of the covalent theta orbitals
     VEC(REAL,:), allocatable :: eigen_C
     ! The array of the covalent theta eigenvalues
     VEC(REAL,:), allocatable :: theta_angle
     ! The array of covalent theta angles
     VEC(INT,:), allocatable :: pair
     ! An array which pairs the positive eigenvalues in eval_C with the
     ! negative eigenvalues, thus definining each theta subspace
     MAT(REAL,:,:), allocatable :: theta_I
     ! The matrix of the ionic theta orbitals
     VEC(REAL,:), allocatable :: eigen_I
     ! The matrix of the ionic theta eigenvalues

     ! =====================
     ! Theta population info
     ! =====================
     VEC(REAL,:), allocatable :: theta_pop_C
     ! Covalent theta orbital populations
     VEC(REAL,:), allocatable :: theta_pop_C_A
     ! Atom "A" theta covalent orbital popualtions
     VEC(REAL,:), allocatable :: theta_pop_C_B
     ! Atom "B" theta covalent orbital populations
     VEC(REAL,:), allocatable :: theta_pop_I
     ! Ionic theta orbital populations
     VEC(REAL,:), allocatable :: theta_pop_I_A
     ! Atom "A" theta ionic orbital popualtions
     VEC(REAL,:), allocatable :: theta_pop_I_B
     ! Atom "B" theta ionic orbital populations
     VEC(REAL,:), allocatable :: theta_cov_index
     ! The vector of each covalent theta bond index
     VEC(REAL,:), allocatable :: theta_ion_index
     ! The vector of each ionic theta bond index

     ! ========
     ! Energies
     ! ========
     VEC(REAL,:), allocatable :: E1
     ! Roby energy for each atom-group
     MAT(REAL,:,:), allocatable :: E2
     ! Roby pair energy for each atom-group pair
     ! NOTE: these are *not* shared populations
     REAL :: E_shared
     ! Roby multiple shared energy for a specified atom-group
     MAT(REAL,:,:), allocatable :: E1_cov
     ! Covalent bond energy for the atom-group pair
     MAT(REAL,:,:), allocatable :: E1_ion
     ! Ionic bond energy for the atom-group pair
     MAT(REAL,:,:), allocatable :: E1_bond
     ! Total bond energy for the atom-group pair

     ! =================
     ! Theta energy info
     ! =================
     VEC(REAL,:), allocatable :: theta_E_C
     ! Covalent theta orbital energies
     VEC(REAL,:), allocatable :: theta_E_C_A
     ! Atom "A" part of the covalent theta energies
     VEC(REAL,:), allocatable :: theta_E_C_B
     ! Atom "B" part of the covalent theta energies
     VEC(REAL,:), allocatable :: theta_E_I
     ! Ionic theta orbital energies
     VEC(REAL,:), allocatable :: theta_E_I_A
     ! Atom "A" part of the ionic theta energies
     VEC(REAL,:), allocatable :: theta_E_I_B
     ! Atom "B" part of the ionic theta energies

   ! theta_E_cov :: VEC{REAL}@
   ! ! The vector of each covalent theta energy index

   ! theta_E_ion :: VEC{REAL}@
   ! ! The vector of each covalent theta energy index

     ! =============
     ! Molecule info
     ! =============
     INT :: spin_multiplicity DEFAULT(1)
     ! The spin multiplicity of the molecule
     type(OPMATRIX_TYPE), allocatable :: density_mx
     ! The density matrix of the molecule
     MAT(REAL,:,:), allocatable :: overlap_mx
     ! The full molecular overlap matrix for Roby analysis
     type(OPMATRIX_TYPE), allocatable :: core_mx
     ! The core hamiltonian matrix of the molecule
     type(OPMATRIX_TYPE), allocatable :: fock_mx
     ! The Fock matrix of the molecule
     type(OPMATRIX_TYPE), allocatable :: E_density_mx
     ! The Krogel et al energy density matrix of the molecule
     ! See Krogel et al (2014) PRB 90 p035125.
     REAL :: E_scf DEFAULT(ZERO)
     ! The SCF energy
     REAL :: E_atomic DEFAULT(ZERO)
     ! The atomic part of the SCF electronic energy, the expectation
     ! value of P_{AB...C} with the SCF density matrix.
     REAL :: E_non_atomic DEFAULT(ZERO)
     ! The complement of E_atomic in E_scf.
     VEC(type(ATOM_TYPE),:), allocatable :: atom
     ! The list of atoms to be used in the Roby calculations
     ! *Pointer copied*.
   end type
   type MO_LOCALISER_TYPE
     STR(len=STR_SIZE) :: algorithm DEFAULT("boys")
     ! Identifier for the kind of localising algorithm
     INT :: n_orb DEFAULT(0)
     ! The number of molecular orbitals to be localized. It must not exceed the
     ! first dimension of the dipole integrals.
     REAL :: accuracy DEFAULT(0.002d0)
     ! Used to decide when the iterative localising algorithm should terminate
     INT :: iteration DEFAULT(0)
     ! The iteration counter, for the number of iterative steps used to refine
     ! the localized orbitals.
     INT :: max_iterations DEFAULT(20)
     ! The maximum number of iterations allowed
     MAT(REAL,:,:), allocatable :: MO
     ! The original molecular orbitals to be localized.
     MAT(REAL,:,:), allocatable :: TM
     ! The orthogonal transformation matrix from the old to the new MO's.
     MAT3(REAL,:,:,:), allocatable :: dipole
     ! The dipole moment integrals in the (current) MO basis, used for the Boys
     ! scheme and for analysing the spread of the localized orbitals. The first
     ! two indices are for the orbitals, the last for the component x, y, or z.
     MAT3(REAL,:,:,:), allocatable :: quadrupole
     ! The quadrupole moment integrals in the (current) MO, used to analyze the
     ! localized orbitals. The first two indices are for the orbitals, the last
     ! for the component xx, yy, zz, xy, xz, yz.
   end type
   type TD_DATA_TYPE

     ! ======
     ! Basics
     ! ======
     STR(len=STR_SIZE) :: method DEFAULT(" ")
     ! The kind of mean-field method used
     STR(len=STR_SIZE) :: implementation DEFAULT(" ")
     ! The kind of states wanted
     STR(len=STR_SIZE) :: dressing DEFAULT(" ")
     ! The kind of doubles dressing method to use.
     STR(len=STR_SIZE) :: DFT_X_functional DEFAULT(" ")
     ! DFT exchange functional to use
     STR(len=STR_SIZE) :: DFT_C_functional DEFAULT(" ")
     ! DFT correlation functional to use
     INT :: n_bf DEFAULT(0)
     ! The number of basis functions
     INT :: n_a DEFAULT(0)
     ! The number of alpha electron
     INT :: n_b DEFAULT(0)
     ! The number of beta electron
     INT :: n_occ DEFAULT(0)
     ! The number of unoccupied orbitals
     INT :: n_vir DEFAULT(0)
     ! The number of virtual orbitals

     ! ==============
     ! MOs & energies
     ! ==============
     MAT(REAL,:,:), allocatable :: MOo
     ! The restricted occupied molecular orbitals.
     MAT(REAL,:,:), allocatable :: MOv
     ! The restricted virtual molecular orbitals.
     VEC(REAL,:), allocatable :: Eo
     ! The restricted occupied MO energies.
     VEC(REAL,:), allocatable :: Ev
     ! The restricted virtual MO energies.
     VEC(REAL,:), allocatable :: Ei
     ! The improved virtual energies. Only if canonicalisation
     ! was performed in the SCF calculations. Must be activated
     ! by the .use_IVO_energies flag, see below.
     MAT(REAL,:,:), allocatable :: Fv
     ! The virtual-virtual block of the Fock matrix with the
     ! new VMO canonicalised MOs.

     ! ====================
     ! State selection info
     ! ====================
     REAL :: tol_S DEFAULT(ZERO)
     ! The tolerance to exclude single determinant excitations 
     ! which are too large, using the Koopmans energy difference.
     BIN :: use_IVO_energies DEFAULT(FALSE)
     ! If TRUE then reconanocalised VMO energies are used to
     ! select the single excitations, but the VV block of the
     ! original Fock matrix is still used in calculations.
     MAT(INT,:,:), allocatable :: single
     ! List of the pair of virtual-occupied indices describing
     ! the single excitation determinants.
     !    Column 1 ... is the virtual  orbital index "A"
     !    Column 2 ... is the occupied orbital index "I"
     ! Importantly, these pairs are sorted by increasing orbital 
     ! energy difference. If non-canonical VMOs are used then the
     ! diagonal elements of VV block of the Fock mx are used as
     ! the orbital energies.
     INT :: min_OMO DEFAULT(0)
     ! The minimim occupied orbital in the singles-window
     INT :: max_VMO DEFAULT(0)
     ! The maximum virtual orbital in the singles-window
     VEC(REAL,:), allocatable :: deltaE
     ! Sorted list of single-excitation energy differences
     ! corresponding to the .single list above.
     REAL :: omega DEFAULT(ZERO)
     ! Energy of a particular state (above the ground state)
     ! for which double excitations within a certain tolerance
     ! .tol_D are to be considered. See .double below.
     REAL :: tol_D DEFAULT(ZERO)
     ! The tolerance to select interacting double excited
     ! determinants from a given excitation energy .omega,
     ! as described in the Mitra et al (2004) method.
     MAT(INT,:,:), allocatable :: double
     ! List of vo-vo quartet indices, in columns 
     !    1 ... for index BJ
     !    2 ... for index AI
     ! respectively, where each index corresponds to a single
     ! excitation in .single. States are sorted so that multiple
     ! BJs occur for each AI, which determines the number of
     ! differnt calls to the Fock builder.
     MAT(INT,:,:), allocatable :: S_list
     ! List of first and last elements for double excitations
     ! with the same single excitation parent.
     VEC(REAL,:), allocatable :: delEab
     ! The list of double-excitation energy differences.
     VEC(INT,:), allocatable :: S
     ! The total spin value S for the state. Only appears
     ! if both singlet (S=0) & triplet (S=1) states are calculated.
     VEC(STR(len=STR_SIZE),:), allocatable :: Slab
     ! Convenient state labels like S1, T2, etc.
     INT :: n_max_show DEFAULT(25)
     ! The maximum no. of singles or doubles states to show 
     ! in output. If set to zero, all states are shown.
     BIN :: show_iterations DEFAULT(TRUE)
     ! If FALSE suppress showing table of Davidson iteration residuals.
     BIN :: show_evecs DEFAULT(TRUE)
     ! If FALSE suppress showing eigenvectors. For test suite,
     ! since eigenvector phases and components of degenerate
     ! M values are not reproducible.

     ! ===================
     ! Davidson parameters
     ! ===================
     INT :: n_evals DEFAULT(4)
     ! Number of lowest-energy excited-state eigenvalues wanted.
     INT :: n_keep DEFAULT(8)
     ! Maximum no. of Davidson trial eigenvector sets to keep. 
     ! Must be at least 2+1=3.
     REAL :: tol_Davidson DEFAULT(TOL(2))
     ! The Davidson convergence criteria.
     BIN :: use_M_0 DEFAULT(FALSE)
     ! If TRUE use M=0 determinants only as an initial guess when
     ! doing an u-CIS-ss calculation. In this case triplets will
     ! not be distinguishable via a triple degeneracy.
     BIN :: use_MGS DEFAULT(TRUE)
     ! If TRUE use Modified Gram-Schmidt QR to orthonormalise the
     ! trial veactors after Davidson update. If not, use the
     ! Householder algorithm.
     BIN :: archive_results DEFAULT(TRUE)
     ! If TRUE then archive vectors and eigenvalues to disk.
     ! If FALSE don't read the archived results for restart. 

     ! ==============
     ! Iteration info
     ! ==============
     INT :: iteration DEFAULT(0)
     ! Iteration counter, for the number of steps used in
     ! the Davidsaon procedure.
     INT :: min_iterations DEFAULT(3)
     ! Minimum no. of iterations allowed
     INT :: max_iterations DEFAULT(100)
     ! Maximum no. of iterations allowed
     INT :: n_trial DEFAULT(0)
     ! The no. of Davidson trial vectors currently kept.
     VEC(REAL,:), allocatable :: max_res
     ! The maximum residual vector length, over all excited 
     ! eigenstates, over all trial vectors.

     ! =============
     ! State results
     ! =============
     VEC(REAL,:), allocatable :: eval
     ! TD excited-state eigenvalues
     MAT(REAL,:,:), allocatable :: evec
     ! TD excited-state eigenvectors
     VEC(REAL,:), allocatable :: eval0
     ! Singlet (S=0) excited-state eigenvalues
     MAT(REAL,:,:), allocatable :: evec0
     ! Singlet (S=0) excited state eigenvectors
     VEC(REAL,:), allocatable :: eval1
     ! Triplet (S=1) excited-state eigenvalues
     MAT(REAL,:,:), allocatable :: evec1
     ! Triplet (S=1) excited state eigenvectors
     VEC(REAL,:), allocatable :: evalc
     ! Correction due to doubles excitation.

     ! ====================
     ! Davidson temporaries
     ! ====================
     MAT(REAL,:,:), allocatable :: V
     ! Davidson trial vector columns.
     !    Size: (d,4d) x .n_eval*n_keep 
     ! where d (or 4d) is the number of single excitations 
     ! (times 4 for the no. of spin blocks aa, ba, ab, bb 
     ! in the unrestricted case). Big memory!
     MAT(REAL,:,:), allocatable :: W
     ! W = A.V needed in step 2. of the Davidson procedure.
     ! See Crouzeix et al (1990) Inria RR 1353. Size as V.
     VEC(REAL,:), allocatable :: R
     ! Space for one residual vector.  Size: (d,4d).
     MAT3(REAL,:,:,:), allocatable :: Vao
     ! Space for *restricted* trial vectors V transformed into 
     ! the AO basis i.e. these are asymmetric density matrices 
     ! needed for (A x V) in an efficient Davidson method.
     ! Size: .n_eval x n_bf x n_bf
     MAT3(REAL,:,:,:), allocatable :: Aao
     ! Space the *restricted* 2e A.V product, in the AO basis.
     ! Size: .n_eval x n_bf x n_bf
     MAT3(REAL,:,:,:), allocatable :: Jao
     ! Space for the *unrestricted* J A.V matrix products.
     ! Size: 4*.n_eval x n_bf x n_bf
     MAT3(REAL,:,:,:), allocatable :: Kao
     ! Space for the *unrestricted* K A.V matrix products.
     ! Size: .n_eval x n_bf x n_bf

     ! ===============
     ! Iteration table
     ! ===============
     VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
     ! The iteration table
   end type

!   type GSD_TREE
!
!     n_bf :: INT   DEFAULT(0)
!     ! The number of basis functions
!
!     n_occ :: INT   DEFAULT(0)
!     ! The number of unoccupied orbitals
!
!     n_vir :: INT   DEFAULT(0)
!     ! The number of virtual orbitals
!
!     ! ==========================
!     ! Determinant MOs & energies
!     ! ==========================
!
!     MOo :: MAT{REAL}@
!     ! The restricted occupied molecular orbitals.
!
!     MOv :: MAT{REAL}@
!     ! The restricted virtual molecular orbitals.
!
!     Eo :: VEC{REAL}@
!     ! The restricted occupied MO energies.
!
!     Ev :: VEC{REAL}@
!     ! The restricted virtual MO energies.
!
!     Fvo :: MAT{REAL}@
!     ! The virtual-occupied block of the Fock matrix.
!     ! IS normally zero for the reference S_NODE but not
!     ! for excited determinants.
!
!     Fvv :: MAT{REAL}@
!     ! The virtual-virtual block of the Fock matrix with the
!     ! new VMO canonicalised MOs.
!
!     ! ==================
!     ! Single excitations
!     ! ==================
!
!     single :: MAT{INT}@
!     ! List of the pair of virtual-occupied indices describing
!     ! the single excitation determinants.
!     !    Column 1 ... is the virtual  orbital index "A"
!     !    Column 2 ... is the occupied orbital index "I"
!     ! Importantly, these pairs are sorted by increasing orbital 
!     ! energy difference. If non-canonical VMOs are used then the
!     ! diagonal elements of VV block of the Fock mx are used as
!     ! the orbital energies.
!
!     deltaE :: VEC{REAL}@
!     ! Sorted list of single-excitation energy differences
!     ! corresponding to the .single list above.
!
!     min_OMO :: INT  DEFAULT(0)
!     ! The minimim occupied orbital in the singles-window
!
!     max_VMO :: INT  DEFAULT(0)
!     ! The maximum virtual orbital in the singles-window
!
!     ! ==================
!     ! Double excitations
!     ! ==================
!
!     dindex :: VEC{INT}@
!     ! Indices of singles from which doubles are generated
!     ! by single excitation.
!
!     double :: VEC{GSD_TREE}@
!     ! The actual doubles wavefuntions for each index in dindex.
!
!     ! ====================
!     ! Partial wavefunction
!     ! ====================
!
!     evec :: MAT{REAL}@
!     ! This nodes eigenvector storage
!
!     ! ====================
!     ! Davidson temporaries
!     ! ====================
!
!     V :: MAT{REAL}@
!     ! Davidson trial vector columns.
!     !    Size: (d,4d) x .n_eval*n_keep 
!     ! where d (or 4d) is the number of single excitations 
!     ! (times 4 for the no. of spin blocks aa, ba, ab, bb 
!     ! in the unrestricted case). Big memory!
!     
!     W :: MAT{REAL}@
!     ! W = A.V needed in step 2. of the Davidson procedure.
!     ! See Crouzeix et al (1990) Inria RR 1353. Size as V.
!
!     Vao :: MAT3{REAL}@
!     ! Space for *restricted* trial vectors V transformed into 
!     ! the AO basis i.e. these are asymmetric density matrices 
!     ! needed for (A x V) in an efficient Davidson method.
!     ! Size: .n_eval x n_bf x n_bf
!
!     Aao :: MAT3{REAL}@
!     ! Space the *restricted* 2e A.V product, in the AO basis.
!     ! Size: .n_eval x n_bf x n_bf
!
!     Jao :: MAT3{REAL}@
!     ! Space for the *unrestricted* J A.V matrix products.
!     ! Size: 4*.n_eval x n_bf x n_bf
!
!     Kao :: MAT3{REAL}@
!     ! Space for the *unrestricted* K A.V matrix products.
!     ! Size: .n_eval x n_bf x n_bf
!
!   end

!   array type VEC{GSD_TREE}
!
!      ! A 1-D array of GSD_TREE objects
!
!   end

!   type CI_DATA
!
!     ! ========================
!     ! Basics & state selection
!     ! ========================
!
!     method :: STR  DEFAULT("u-cis")
!     ! The kind of mean-field method used
!
!     tol_S :: REAL, readonly   DEFAULT(ZERO)
!     ! Tolerance to exclude single determinant excitations 
!     ! which are too high in energy, by Koopman's theorem.
!
!     omega :: REAL   DEFAULT(ZERO)
!     ! Energy of a particular state (above the ground state)
!     ! for which double excitations within a certain tolerance
!     ! .tol_D are to be considered. Used for dressing.
!
!     tol_D :: REAL   DEFAULT(ZERO)
!     ! The tolerance to select interacting double excited
!     ! determinants from a given excitation energy .omega.
!
!     ! ===================
!     ! Davidson parameters
!     ! ===================
!
!     n_evals :: INT, readonly   DEFAULT(4)
!     ! Number of excited-state eigenvalues wanted.
!
!     n_keep :: INT, readonly   DEFAULT(8)
!     ! Maximum no. of Davidson trial eigenvector sets to keep. 
!     ! Must be at least 3+1=4 for the triplet and singlet.
!
!     tol_Davidson :: REAL, readonly   DEFAULT(TOL(2))
!     ! Davidson convergence criteria on residual vectors.
!
!     ! ============
!     ! Wavefunction
!     ! ============
!
!     ground :: GSD_TREE@
!     ! The ground state. Includes single excitations, and
!     ! recursive links to the double excitations.
!
!     ! ==============
!     ! Iteration info
!     ! ==============
!
!     iteration :: INT  DEFAULT(0)
!     ! Iteration counter, for the number of steps used in
!     ! the Davidsaon procedure.
!
!     min_iterations :: INT  DEFAULT(3)
!     ! Minimum no. of iterations allowed
!
!     max_iterations :: INT  DEFAULT(100)
!     ! Maximum no. of iterations allowed
!
!     n_trial :: INT  DEFAULT(0)
!     ! The no. of Davidson trial vectors currently kept.
!     
!     max_res :: VEC{REAL}@
!     ! The maximum residual vector length, over all excited 
!     ! eigenstates, over all trial vectors.
!
!     ! =============
!     ! State results
!     ! =============
!
!     eval :: VEC{REAL}@
!     ! TD excited-state eigenvalues
!
!     evec :: MAT{REAL}@
!     ! TD excited-state eigenvectors
!
!     ! ===========
!     ! Output info
!     ! ===========
!
!     n_max_show :: INT   DEFAULT(25)
!     ! The maximum no. of singles or doubles states to show 
!     ! in output. If set to zero, all states are shown.
!
!     S :: VEC{INT}@
!     ! The total spin value S for the state. Only appears
!     ! if both singlet (S=0) & triplet (S=1) states are calculated.
!
!     Slab :: VEC{STR}@
!     ! Convenient state labels like S1, T2, etc.
!
!     table :: VEC{TABLE_COLUMN}@
!     ! The iteration table
!
!   end

!  ========
!  Molecule
!  ========
   type MOLECULE_TYPE
     STR(len=STR_SIZE) :: name DEFAULT("unknown")
     ! Name of molecule
     INT :: charge DEFAULT(0)
     ! Electric charge of the molecule
     INT :: spin_multiplicity DEFAULT(1)
     ! Spin multiplicity of the molecule
     INT :: n_e DEFAULT(0)
     ! No of electrons
     VEC(INT,:), allocatable :: n_e_on_atom
     ! No of electrons on the atom -- used in ELMO methods
     INT :: n_a DEFAULT(0)
     ! No of alpha electrons
     INT :: n_b DEFAULT(0)
     ! No of beta electrons
     VEC(REAL,3) :: E_field DEFAULT(ZERO)
     ! Applied electric field in atomic units
     VEC(REAL,3) :: B_field DEFAULT(ZERO)
     ! Applied magnetic field in atomic units
     VEC(REAL,3) :: gauge_origin DEFAULT(ZERO)
     ! Global gauge origin for magnetic field

     ! =========
     ! Atom info
     ! =========
     INT :: n_atom DEFAULT(0)
     ! No of atoms
     VEC(type(ATOM_TYPE),:), allocatable :: atom
     ! List of atoms in molecule
     VEC(type(EVEC_INT_TYPE),:), allocatable :: atoms_bonded_to_atom
     ! List of atom connections
     BIN :: atom_info_made DEFAULT(FALSE)
     ! Set TRUE if the atom info has been made.
     INT :: n_atom_kind DEFAULT(0)
     ! No of atoms of a different kind
     VEC(type(EVEC_INT_TYPE),:), allocatable :: Salvador_bonds_for
     ! The Salvador & Ramos-Cordoba (2013) neighbour atoms.
     VEC(type(EVEC_REAL_TYPE),:), allocatable :: Salvador_radii_for
     ! The Salvador & Ramos-Cordoba (2013) radii for each neighbour.
     ! See JCP 139 p. 071103.
     MAT(REAL,:,:), allocatable :: Salvador_chi
     ! The Salvador & Ramos-Cordoba (2013) ratio of radii, otherwise
     ! known as "chi".

     ! ===========
     ! Atom groups
     ! ===========
     VEC(type(ATOM_GROUP_TYPE),:), allocatable :: atom_group
     ! A list of atom groups in the molecule, for group property decomposition
     VEC(INT,:), allocatable :: atom_group_charges
     ! The charges on the auto-detected atom groups
     VEC(type(MOLECULE_TYPE),:), allocatable :: mol
     ! List of sub-molecules of this molecule

     ! ===================
     ! Gaussian basis info
     ! ===================
     STR(len=STR_SIZE) :: basis_name DEFAULT(" ")
     ! A string representing the name of the gaussian basis set class
     ! to be used for each atom
     INT :: n_basis DEFAULT(0)
     ! No of basis sets
     INT :: basis_l_max DEFAULT(-1)
     ! The maximum l value for the basis set
     VEC(type(BASIS_TYPE),:), allocatable :: basis
     ! List of basis sets used
     STR(len=STR_SIZE) :: auxiliary_basis_name DEFAULT(" ")
     ! The library name of the auxiliary (fitting) basis set
     VEC(type(BASIS_TYPE),:), allocatable :: auxiliary_basis
     ! The auxiliary basis sets, one per kind of atom; always spherical
     VEC(INT,:), allocatable :: auxiliary_basis_for_atom
     ! The index in .auxiliary_basis of each atom's auxiliary basis
     MAT(REAL,:,:), allocatable :: RI_metric_factor
     ! Cholesky factor of the auxiliary coulomb metric, see .initialize_RI_J
     BIN :: use_spherical_basis DEFAULT(FALSE)
     ! Set true if you want to use spherical harmonic basis set
     BIN :: use_CRYSTAL23_order DEFAULT(FALSE)
     ! Set true if you want to use CRYSTAL23 p function order for
     ! spherical harmonic basis set
     INT :: n_bf DEFAULT(0)
     ! Total number of basis functions in the molecular basis set
     INT :: n_prim DEFAULT(0)
     ! Total number of primitives in the molecular basis set

     ! =================
     ! Slater basis info
     ! =================
     STR(len=STR_SIZE) :: slaterbasis_name DEFAULT(" ")
     ! A string representing the name of the Slater basis set class
     ! to be used for each atom
     VEC(type(SLATERBASIS_TYPE),:), allocatable :: slaterbasis
     ! List of Slater basis sets used
     STR(len=STR_SIZE) :: coppensbasis_name DEFAULT(" ")
     ! A string representing the name of the Coppens basis set class
     ! to be used for each atom
     VEC(type(COPPENSBASIS_TYPE),:), allocatable :: coppensbasis
     ! List of coppens basis sets used
     BIN :: basis_info_made DEFAULT(FALSE)
     ! Set TRUE if the gaussian basis set info has been made.

     ! ===========================
     ! Becke grid and related info
     ! ===========================
     type(BECKE_GRID_TYPE), allocatable :: becke_grid
     ! Becke DFT integration grid data
     MAT(REAL,:,:), allocatable :: XC_pts
     ! The DFT integration grid for the whole molecule: every atom's
     ! partitioned grid, laid end to end. Built once per SCF.
     VEC(REAL,:), allocatable :: XC_wts
     ! The weights of .XC_pts, partition included.
     VEC(INT,:), allocatable :: XC_index
     ! The row of the unique-atom grid each point of .XC_pts came from,
     ! so the cached basis functions of the parent atom can be reused.
     VEC(INT,:), allocatable :: XC_first_pt
     ! XC_first_pt(a) is the first row of .XC_pts belonging to atom "a".
     VEC(INT,:), allocatable :: XC_last_pt
     ! XC_last_pt(a) is the last row of .XC_pts belonging to atom "a".
     INT :: XC_n_unpruned DEFAULT(0)
     ! The number of points in the molecular DFT grid before pruning.
     VEC(INT,:), allocatable :: XC_batch_pt
     ! Rows of .XC_pts listed batch by batch.
     VEC(INT,:), allocatable :: XC_batch_first
     ! Batch "b" is .XC_batch_pt(XC_batch_first(b):XC_batch_last(b)).
     VEC(INT,:), allocatable :: XC_batch_last
     ! See .XC_batch_first.
     VEC(INT,:), allocatable :: XC_batch_shell
     ! The significant shells of every batch, listed batch by batch.
     VEC(INT,:), allocatable :: XC_batch_first_shell
     ! Batch "b"'s shells are .XC_batch_shell(XC_batch_first_shell(b):XC_batch_last_shell(b)).
     VEC(INT,:), allocatable :: XC_batch_last_shell
     ! See .XC_batch_first_shell.
     type(BECKE_GRID_TYPE), allocatable :: COSX_grid
     ! The integration grid for the chain-of-spheres exchange
     MAT(REAL,:,:), allocatable :: COSX_pts
     ! The molecular grid of .COSX_grid, as .XC_pts
     VEC(REAL,:), allocatable :: COSX_wts
     ! The weights of .COSX_pts
     VEC(INT,:), allocatable :: COSX_batch_pt
     ! As .XC_batch_pt, for .COSX_pts
     VEC(INT,:), allocatable :: COSX_batch_first
     ! As .XC_batch_first
     VEC(INT,:), allocatable :: COSX_batch_last
     ! As .XC_batch_last
     VEC(INT,:), allocatable :: COSX_batch_shell
     ! As .XC_batch_shell
     VEC(INT,:), allocatable :: COSX_batch_first_shell
     ! As .XC_batch_first_shell
     VEC(INT,:), allocatable :: COSX_batch_last_shell
     ! As .XC_batch_last_shell
     MAT(REAL,:,:), allocatable :: COSX_fit
     ! S S_num^-1, S_num the overlap matrix by quadrature on .COSX_pts
     VEC(type(EVEC_INT_TYPE),:), allocatable :: overlapping_atoms_for_atom
     ! List of atoms which have significant overlap with another

     ! ==================================
     ! Plot, interpolator and isosurfaces
     ! ==================================
     type(PLOT_GRID_TYPE), allocatable :: plot_grid
     ! Rectangular grid data, for plots
     type(CELL_MAP_TYPE), allocatable :: cell_map
     ! A density of the unit cell as Fourier coefficients, for maps
     BIN :: use_interpolators DEFAULT(TRUE)
     ! Set to use interpolators
     type(INTERPOLATOR_TYPE), allocatable :: interpolator
     ! Template interpolator object used for setting options
     type(ISOSURFACE_TYPE), allocatable :: isosurface
     ! An object used for creating triangluated meshes for isosurface plots.

     ! ==================================
     ! Crystal, cluster and symmetry info
     ! ==================================
     type(CRYSTAL_TYPE), allocatable :: crystal
     ! Crystal data for the enclosing crystal
     type(CLUSTER_TYPE), allocatable :: cluster
     ! Crystal cluster data
     type(CIF_TYPE), allocatable :: cif
     ! Crystallographic Information file (CIF) object
     type(CRYSTAL23_TYPE), allocatable :: crystal23
     ! Crystal23 periodic density matrix info.
     type(POINTGROUP_TYPE), allocatable :: pointgroup
     ! Pointgroup symmetry of the molecule
     type(MOLECULE_TYPE), allocatable :: saved
     ! For saving self and temporarily using an alternative in its place

     ! ==========
     ! Shell info
     ! ==========
     INT :: n_shell DEFAULT(0)
     ! Total number of shells in the molecular basis set
     INT :: n_shell_pairs DEFAULT(0)
     ! Total number of shell pairs in the molecular basis set
     INT :: n_unique_shells DEFAULT(0)
     ! Number of unique shells in the basis set.
     VEC(INT,:), allocatable :: atom_for_shell
     ! Atom index for molecular shell index array
     VEC(INT,:), allocatable :: atom_shell_for_shell
     ! Atom shell index for molecular shell index array
     VEC(INT,:), allocatable :: first_shell_for_atom
     ! First molecule shell index for an atom
     VEC(INT,:), allocatable :: last_shell_for_atom
     ! Last molecule shell index for an atom
     VEC(INT,:), allocatable :: basis_shell_for_shell
     ! Map a shell of the molecule to a shell of the basis set
     VEC(INT,:), allocatable :: first_basis_fn_for_shell
     ! First basis function for a given shell
     VEC(INT,:), allocatable :: last_basis_fn_for_shell
     ! Last basis function for a given shell
     VEC(INT,:), allocatable :: angular_moment_for_shell
     ! Angular moment for a given shell
     VEC(INT,:), allocatable :: first_cartesian_bf_for_shell
     ! First cartesian basis function for a given shell
     VEC(INT,:), allocatable :: last_cartesian_bf_for_shell
     ! Last cartesian basis function for a given shell
     INT :: n_bf_cartesian DEFAULT(0)
     ! No. of basis functions, counting cartesian components
     VEC(INT,:), allocatable :: first_basis_fn_for_atom
     ! For the atom basis function limits
     VEC(INT,:), allocatable :: last_basis_fn_for_atom
     ! For the atom basis function limits
     MAT(type(SHELL1PAIR_TYPE),:,:), allocatable :: basis_shell1pair
     ! Precomputed stuff for shellpairs of the basis set, to speed up
     ! later calculations of gaussian integrals.
     VEC(REAL,:), allocatable :: max_I
     ! Maximum of the (ab|ab) integrals, used for SChwarz inequality test to
     ! eliminate two electron integrals.

     ! ========
     ! SCF info
     ! ========
     type(SCF_DATA_TYPE), allocatable :: SCF_data
     ! SCF data object
     type(OPVECTOR_TYPE), allocatable :: MO_energies
     ! The orbital energies
     VEC(REAL,:), allocatable :: VMO_energies_1
     ! The virtual Hiuzinaga-Arnau MO energies for singlets
     VEC(REAL,:), allocatable :: VMO_energies_3
     ! The virtual Hiuzinaga-Arnau MO energies for triples
     type(OPMATRIX_TYPE), allocatable :: M0s
     ! The unfitted reference MOs, for overlap_with_M0s.
     ! NOT necessarily the lambda=0 MOs: a RESTART starts at lambda>0 and seeds
     ! these with the MOs at the restart lambda. Set in MOLECULE.SCF.
     type(OPMATRIX_TYPE), allocatable :: MOs
     ! The molecular orbitals
     type(OPMATRIX_TYPE), allocatable :: density_mx
     ! The density matrix
     type(OPMATRIX_TYPE), allocatable :: old_density_mx
     ! The old density matrix used for damping
     type(OPMATRIX_TYPE), allocatable :: delta_density_mx
     ! The delta density matrix used for incremental Fock matrix
     ! builds in the direct SCF method
     type(OPMATRIX_TYPE), allocatable :: NOs
     ! The natural orbitals
     type(OPVECTOR_TYPE), allocatable :: NO_occ_nos
     ! The natural orbital occupation numbers
     type(MO_LOCALISER_TYPE), allocatable :: localiser
     ! An MO localiser object
     type(OPMATRIX_TYPE), allocatable :: E_density_mx
     ! The E density matrix in the AO basis.
     ! See Krogel et al (2014) Phys. Rev B 90 p. 035125
     type(OPMATRIX_TYPE), allocatable :: E_NOs
     ! The energy natural orbitals
     type(OPVECTOR_TYPE), allocatable :: E_NO_energies
     ! Eigenvalues of the energy density maytrix

     ! ==================
     ! Time-depedent info
     ! ==================
     type(TD_DATA_TYPE) :: TD_data
     ! Time-dependent calculation data object

     ! =================
     ! Integral matrices
     ! =================
     MAT(REAL,:,:), allocatable :: overlap_mx
     ! The basis function overlap matrix
     MAT(REAL,:,:), allocatable :: overlap_mx_evecs
     ! The overlap matrix eigenvectors
     VEC(REAL,:), allocatable :: overlap_mx_evals
     ! The overlap matrix eigenvalues
     VEC(REAL,:), allocatable :: max_S
     ! The maximum value of the basis function overlap matrix
     ! over pairs of shells ... for skip testing ...
     type(OPMATRIX_TYPE), allocatable :: kinetic_energy_mx
     ! The kinetic energy integral matrix
     type(OPMATRIX_TYPE), allocatable :: nuclear_attraction_mx
     ! The nuclear attraction energy integral matrix
     type(OPMATRIX_TYPE), allocatable :: core_mx
     ! The basis function core matrix
     type(OPMATRIX_TYPE), allocatable :: fock_mx
     ! The fock matrix
     type(OPMATRIX_TYPE), allocatable :: fock_2e_mx
     ! The 2e part of the fock matrix
     type(OPMATRIX_TYPE), allocatable :: constraint_mx
     ! The constraint matrix, used for X-ray or PND constrained SCF calculations

   ! nomo_gradient :: MAT{REAL}@
     ! This is the NOMO/ELMO energy derivative wrt to the occupied
     ! coefficients only. It is not a square matrix.

     ! =============
     ! Bond analysis
     ! =============
     type(ROBY_TYPE), allocatable :: roby
     ! A Roby data object

     ! ===============
     ! CrystalExplorer
     ! ===============
     STR(len=PATH_SIZE) :: CX_file_name DEFAULT(" ")
     ! The name of the Crystal Explorer (CX) output file
     BIN :: CX_uses_angstrom DEFAULT(TRUE)
     ! Set TRUE if output for CX should be in Angstrom, not Bohr.

     ! ===================
     ! Atomic partitioning
     ! ===================
     STR(len=STR_SIZE) :: atom_partition_model DEFAULT(" ")
     ! Set to the model desired to partition the total electropn
     ! density into atomic pieces, for moment calculations. This is
     ! used to determine the partition model for atomic form factors
     ! in CRYSDTAL unless overriden.
     BIN :: partition_info_made DEFAULT(FALSE)
     ! Set TRUE if atomic moments (of any kind) are made
     BIN :: u_HA_moments_made DEFAULT(FALSE)
     ! Set TRUE if the unrestricted Hirshfeld atomic moments are made

     ! ================
     ! Polarizabilities
     ! ================
     MAT3(REAL,:,:,:), allocatable :: U_electric_dipole
     ! The derivative U matrices for electric dipole perturbations
     MAT(REAL,:,:), allocatable :: dipole_polarisability
     ! The dipole polarisability of the molecule.
     MAT3(REAL,:,:,:), allocatable :: dipole_hyperpolarisability
     ! The dipole hyperpolarisability of the molecule.
     BIN :: atomic_polarisabilities_made DEFAULT(FALSE)
     ! Set TRUE if the atomic polarisabilities are made
     MAT3(REAL,:,:,:), allocatable :: U2_electric_dipole
     ! The second order U matrices for electric dipole perturbations

     ! ==================
     ! Energy derivatives
     ! ==================
     MAT(REAL,:,:), allocatable :: force_constants
     ! The second order force constants
     MAT(REAL,:,:), allocatable :: normal_mode_eigenvectors
     ! The normal mode eigenvectors
     VEC(REAL,:), allocatable :: normal_mode_eigenvalues
     ! The normal mode eigenvalues
     REAL :: FD_hessian_step DEFAULT(0.01d0)
     ! Cartesian step (bohr) for make_FD_hessian
     REAL :: internal_ADP_temperature DEFAULT(ZERO)
     ! Temperature (Kelvin) for make_internal_ADPs
     REAL :: frequency_scale_factor DEFAULT(-ONE)
     ! Harmonic frequency scale factor; negative means the default (see below)
     REAL :: FD_hessian_scale DEFAULT(-ONE)
     ! Scale factor for the method make_FD_hessian used: Scott & Radom, J. Phys.
     ! Chem. 100, 16502 (1996), fitted for 6-31G(d) -- HF 0.8953, B3LYP 0.9614,
     ! BLYP 0.9945 -- and applied whatever the basis. Negative: none known
     REAL :: soft_mode_cutoff DEFAULT(200d0)
     ! Modes below this (cm^-1) are soft, left out of the stiff-mode ADPs
     MAT3(REAL,:,:,:), allocatable :: stiff_mode_ADPs
     ! U of each atom from the stiff internal modes, (3,3,n_atom), bohr^2
     REAL :: phi_step_size DEFAULT(MOLECULE_PHI_STEP_SIZE)
     ! Default dimensionless normal coordinate step size for evaluating
     ! anharmionic constants
     MAT3(REAL,:,:,:), allocatable :: phi3_force_constants
     ! The third order force constants in cm^-1
     MAT4(REAL,:,:,:,:), allocatable :: phi4_force_constants
     ! The fourth order force constants in cm^-1

     ! ====================
     ! Finite/point nucleus
     ! ====================
     STR(len=STR_SIZE) :: nucleus_model DEFAULT("point")
     ! What kind of nucleus to use

     ! =========================
     ! Connection table variable
     ! =========================
     MAT(INT,:,:), allocatable :: elmo_connection_table

     ! =================
     ! Atomic multipoles
     ! =================
     VEC(REAL,:), allocatable :: charges
     MAT(REAL,:,:), allocatable :: dipoles
     MAT(REAL,:,:), allocatable :: quadrupoles
     MAT(REAL,:,:), allocatable :: octupoles
   end type

!  ==================
!  Energies breakdown 
!  ==================
   type BREAKDOWN_DATA_TYPE

     !  =============
     !  Molecule data
     !  =============
     type(MOLECULE_TYPE), allocatable :: mol
     type(MOLECULE_TYPE), allocatable :: mol1
     type(MOLECULE_TYPE), allocatable :: mol2
     ! The molecules the breakdown is being calculated for
     MAT(REAL,:,:), allocatable :: density_mx
     MAT(REAL,:,:), allocatable :: new_density_mx
     ! The restricted part of the density matrix for the supermolecule, before and after
     ! symmorthonormalisation respectively
     MAT(BIN,:,:), allocatable :: mol1_bonded
     MAT(BIN,:,:), allocatable :: mol2_bonded
     ! molN_bonded(i, j) = whether atoms i and j within molecule N are 
     ! chemically bonded
     MAT3(REAL,:,:,:), allocatable :: attraction_mx
     ! The nuclear attraction integrals for each nucleus

     !  ===========================
     !  Matrices of atom-atom terms
     !  ===========================
     MAT(REAL,:,:), allocatable :: E_nn
     MAT(REAL,:,:), allocatable :: E_ne
     MAT(REAL,:,:), allocatable :: E_ee
     MAT(REAL,:,:), allocatable :: E_el
     MAT(REAL,:,:), allocatable :: E_pol
     MAT(REAL,:,:), allocatable :: E_disp
     MAT(REAL,:,:), allocatable :: E_ex
     MAT(REAL,:,:), allocatable :: E_rep
     MAT(REAL,:,:), allocatable :: E_er
     MAT(REAL,:,:), allocatable :: E_tot
     MAT(REAL,:,:), allocatable :: distances
     ! Respectively the nucleus-nucleus repulsion energies   
     ! Nucleus-electron attraction energies   
     ! Electron-electron repulsion energies   
     ! Total electrostatic energies (sum of the above three terms)
     ! Polarisation energies - use polarisation coefficients from Thakkar and
     ! Lupinetti (2006)
     ! Dispersion energies - of the form C6 / r^6, where r is the distance
     ! between the atoms, and C6 a coefficient from Grimme (2006) J. Comp. Chem. 27(15) p. 1787
     ! Exchange energies (arise from the combination of molecular orbitals into
     ! a slater determinant)
     ! Repulsion energies (arise from the orthogonalisation of the orbitals of
     ! the two atom groups with respect to each other)
     ! Exchange-repulsion energies (sum of the above two terms)
     ! Total energies (sum of the scaled electrostatic, polarisation, dispersion and
     ! exchange-repulsion terms)
     ! And distances between nuclei
     MAT(REAL,:,:), allocatable :: E_ee_helper
     MAT(REAL,:,:), allocatable :: E_ex_helper
     MAT(REAL,:,:), allocatable :: E_rep_helper
     ! Helper variables

     !  =====================================
     !  Functional group interaction energies
     !  =====================================
     INT :: num_groups
     INT :: num_queries
     VEC(STR(len=STR_SIZE),:), allocatable :: word_groups
     ! Stores lists of functional groups to make queries about 
     ! Groups written using a special notation
     VEC(type(EVEC_EVEC_INT_TYPE),:), allocatable :: mol1_groups
     VEC(type(EVEC_EVEC_INT_TYPE),:), allocatable :: mol2_groups
     ! Stores lists of functional group pairs to calculate interaction energies
     ! between
     ! Index 1 is the number of the group
     ! Index 2 is the number of the atom within the group 
     MAT(INT,:,:), allocatable :: queries
     ! Stores the indices of the pairs of groups for which interaction energies are to be
     ! calculated
     ! Index 1 is the number of the pair
     ! Index 2 gives the index of the group on the molecule
     MAT3(REAL,:,:,:), allocatable :: group_energies
     ! Stores the interaction energies between the groups, each group and the
     ! rest of the other molecules, and the rest of the two molecules
     ! Index 1 is the number of the pair
     ! Index 2:
     ! 1 - between the groups
     ! 2 - between the group on molecule 1 and the rest of molecule 2
     ! 3 - between the rest of molecule 1 and the group on molecule 2
     ! 4 - between the rest of molecule 1 and the rest of molecule 2
     ! Index 3:
     ! 1 - electrostatic energy
     ! 2 - polarisation energy
     ! 3 - dispersion energy
     ! 4 - exchange-repulsion energy
     ! 5 - total energy
     STR(len=STR_SIZE) :: cif_filename
     ! The cif file the molecular wavefunctions are generated from

     !  ================
     !  Output variables
     !  ================
     STR(len=STR_SIZE) :: energy_units
     STR(len=STR_SIZE) :: distance_units
     ! Units the output is presented in
     REAL :: energy_factor
     REAL :: distance_factor
     ! Conversion factors from atomic units to the units selected for output
     BIN :: print_atom_breakdown
     BIN :: print_distances
     BIN :: print_extra_terms
     BIN :: print_totals
     ! Flags for whether or not to print the atom-atom
     ! Electrostatic, polarisation, dispersion, exchange-repulsion and total terms,
     ! Distances
     ! Electron-electron repulsion, electron-nucleus attraction,
     ! Nucleus-nucleus repulsion, exchange and repulsion terms
     ! And Totals
     VEC(STR(len=STR_SIZE),:), allocatable :: mol1_symbols
     VEC(STR(len=STR_SIZE),:), allocatable :: mol2_symbols
     ! MolN_symbols(i) = the symbol for the ith atom in molecule N

     !  ===========
     !  Other Stuff
     !  ===========
     VEC(REAL,:), allocatable :: scale_factors
     ! The factors used to scale each of the 4 energy terms
     ! Index:
     ! 1 - electrostatic factor
     ! 2 - polarisation factor
     ! 3 - dispersion factor
     ! 4 - exchange-repulsion factor
   end type

end module
