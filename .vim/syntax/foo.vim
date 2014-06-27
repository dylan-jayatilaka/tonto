" Vim syntax file
" Language: Foo

let fortran_dialect="f95"
let fortran_fixed_source = 0
let b:fortran_dialect = "f95"
let b:fortran_fixed_source = 0



" Vim syntax file
" Language:	Fortran95 (and Fortran90, Fortran77, F and elf90)
" Version:	6.0b
" Last Change:	2001 Mar 23
" Maintainer:	Ajit J. Thakkar <ajit@unb.ca>; <http://www.unb.ca/chem/ajit/>
" Credits:
"  Some items based on the fortran syntax file by Mario Eusebio and
"   Preben Guldberg, and some on suggestions by Andrej Panjkov,
"   Bram Moolenaar, Thomas Olsen, Michael Sternberg, Christian Reile,
"   Walter Dieudonné and Alexander Wagner.

" Quit if a syntax file is already loaded
if exists("b:current_syntax")
  finish
endif

" let b:fortran_dialect = fortran_dialect if set correctly by user
let b:fortran_dialect = matchstr(fortran_dialect,'\<\(f\(9[05]\|77\)\|elf\|F\)\>')

  syn match fortran90Identifier		"\<\a\w*\>" contains=fortranSerialNumber
  if exists("fortran_more_precise")
    syn match fortranConstructName "\(\<end\s*do\s\+\)\@<=\a\w*\>"
    syn match fortranConstructName "\(\<end\s*if\s\+\)\@<=\a\w*\>"
    syn match fortranConstructName "\(\<end\s*select\s\+\)\@<=\a\w*\>"
  endif

syn match   fortranUnitHeader	"\<end\>"

syn keyword fortranType		character complex integer
syn keyword fortranType		intrinsic
syn match fortranType		"\<implicit\>"
syn keyword fortranStructure	dimension
syn keyword fortranStorageClass	parameter save
syn match fortranUnitHeader	"\<subroutine\>"
syn keyword fortranCall		call
syn match fortranUnitHeader	"\<function\>"
syn match fortranUnitHeader	"\<program\>"
syn keyword fortranStatement	ENSURE DIE WARN DIE_IF
syn keyword fortranStatement	return stop
syn keyword fortranConditional	else then
syn match fortranConditional	"\<if\>"
syn match fortranRepeat		"\(\<parallel\> \)\?\<do\>"

syn keyword fortranTodo		contained todo fixme

"Catch errors caused by too many right parentheses
syn region fortranParen transparent start="(" end=")" contains=ALLBUT,fortranParenError,@fortranCommentGroup
syn match  fortranParenError   ")"

syn match fortranOperator	"\.\s*n\=eqv\s*\."
syn match fortranOperator	"\.\s*\(and\|or\|not\)\s*\."
syn match fortranOperator	"\(and\|or\|not\)"
syn match fortranOperator	"\(+\|-\|/\|\*\)"

syn match fortranBoolean	"\.\s*\(true\|false\)\s*\."

syn keyword fortranReadWrite	backspace close inquire open rewind endfile
syn keyword fortranReadWrite	read write print

"If tabs are allowed then the left margin checks do not work
if exists("fortran_have_tabs")
  syn match fortranTab		"\t"  transparent
else
  syn match fortranTab		"\t"
endif

syn keyword fortranIO		unit file iostat access blank fmt form
syn keyword fortranIO		recl status exist opened number named name
syn keyword fortranIO		sequential direct rec
syn keyword fortranIO		formatted unformatted nextrec

syn keyword fortran66Intrinsic		cabs ccos cexp clog csin csqrt
syn keyword fortran66Intrinsic		dacos dasin datan datan2 dcos dcosh
syn keyword fortran66Intrinsic		ddim dexp dint dlog dlog10 dmod dabs
syn keyword fortran66Intrinsic		dnint dsign dsin dsinh dsqrt dtan
syn keyword fortran66Intrinsic		dtanh iabs idim idnint isign idint ifix
syn keyword fortran66Intrinsic		amax0 amax1 dmax1 max0 max1
syn keyword fortran66Intrinsic		amin0 amin1 dmin1 min0 min1
syn keyword fortran66Intrinsic		amod float sngl alog alog10

" Intrinsics provided by some vendors
syn keyword fortranExtraIntrinsic	cdabs cdcos cdexp cdlog cdsin cdsqrt
syn keyword fortranExtraIntrinsic	cqabs cqcos cqexp cqlog cqsin cqsqrt
syn keyword fortranExtraIntrinsic	qacos qasin qatan qatan2 qcos qcosh
syn keyword fortranExtraIntrinsic	qdim qexp iqint qlog qlog10 qmod qabs
syn keyword fortranExtraIntrinsic	qnint qsign qsin qsinh qsqrt qtan
syn keyword fortranExtraIntrinsic	qtanh qmax1 qmin1
syn keyword fortranExtraIntrinsic	dimag qimag dcmplx qcmplx dconjg qconjg
syn keyword fortranExtraIntrinsic	gamma dgamma qgamma algama dlgama qlgama
syn keyword fortranExtraIntrinsic	erf derf qerf erfc derfc qerfc

syn keyword fortran77Intrinsic	abs acos aimag aint anint asin atan atan2
syn keyword fortran77Intrinsic	cos sin tan sinh cosh tanh exp log log10
syn keyword fortran77Intrinsic	sign sqrt int cmplx nint min max conjg
syn keyword fortran77Intrinsic	char ichar index
syn match fortran77Intrinsic	"\<len\s*[(,]"me=s+3
syn match fortran77Intrinsic	"\<real\s*("me=s+4
syn match fortranType		"\<implicit\s\+real"
syn match fortranType		"^\s*real\>"
syn match fortran90Intrinsic	"\<logical\s*("me=s+7
syn match fortranType		"\<implicit\s\+logical"
syn match fortranType		"^\s*logical\>"

"Numbers of various sorts
" Integers
syn match fortranNumber	display "\<\d\+\(_\a\w*\)\=\>"
" floating point number, without a decimal point
syn match fortranFloatNoDec	display	"\<\d\+[deq][-+]\=\d\+\(_\a\w*\)\=\>"
" floating point number, starting with a decimal point
syn match fortranFloatIniDec	display	"\.\d\+\([deq][-+]\=\d\+\)\=\(_\a\w*\)\=\>"
" floating point number, no digits after decimal
syn match fortranFloatEndDec	display	"\<\d\+\.\([deq][-+]\=\d\+\)\=\(_\a\w*\)\=\>"
" floating point number, D or Q exponents
syn match fortranFloatDExp	display	"\<\d\+\.\d\+\([dq][-+]\=\d\+\)\=\(_\a\w*\)\=\>"
" floating point number
syn match fortranFloat	display	"\<\d\+\.\d\+\(e[-+]\=\d\+\)\=\(_\a\w*\)\=\>"
" Numbers in formats
syn match fortranFormatSpec	display	"\d*f\d\+\.\d\+"
syn match fortranFormatSpec	display	"\d*e[sn]\=\d\+\.\d\+\(e\d+\>\)\="
syn match fortranFormatSpec	display	"\d*\(d\|q\|g\)\d\+\.\d\+\(e\d+\)\="
syn match fortranFormatSpec	display	"\d\+x\>"
" The next match would pick up identifiers as well
" syn match fortranFormatSpec	display	"\<\(a\|i\)\d\+"

" Numbers as labels
syn match fortranLabelNumber	display	"^\d\{1,5}\>"
syn match fortranLabelNumber	display	"^ \d\{1,4}\>"ms=s+1
syn match fortranLabelNumber	display	"^  \d\{1,3}\>"ms=s+2
syn match fortranLabelNumber	display	"^   \d\d\=\>"ms=s+3
syn match fortranLabelNumber	display	"^    \d\>"ms=s+4

if exists("fortran_more_precise")
  " Numbers as targets
  syn match fortranTarget	display	"\(\<if\s*(.\+)\s*\)\@<=\(\d\+\s*,\s*\)\{2}\d\+\>"
  syn match fortranTarget	display	"\(\<do\s\+\)\@<=\d\+\>"
  syn match fortranTarget	display	"\(\<go\s*to\s*(\=\)\@<=\(\d\+\s*,\s*\)*\d\+\>"
endif

syn keyword fortranTypeEx	external
syn keyword fortranIOEx		format
syn keyword fortranStatementEx	continue
syn match fortranStatementEx	"\<go\s*to\>"
syn region fortranStringEx	start=+'+ end=+'+ contains=fortranContinueMark,fortranLeftMargin,fortranSerialNumber
syn keyword fortran77IntrinsicEx	lge lgt lle llt mod
syn keyword fortranStatementOb	assign pause to

  syn keyword fortranType	type array none

  syn keyword fortranStructure	private public intent optional 
  syn keyword fortranStructure	pointer target allocatable readonly selfless
  syn keyword fortranStorageClass	in out
  syn match fortranStorageClass	"\<kind\s*="me=s+4
  syn match fortranStorageClass	"\<len\s*="me=s+3

  syn match fortranUnitHeader	"\<module\>"
  syn keyword fortranUnitHeader	use only contains
  syn keyword fortranUnitHeader	recursive result interface operator
  syn keyword fortranStatement	allocate deallocate nullify cycle exit
  syn match fortranConditional	"\<select\>"
  syn keyword fortranConditional	case default DEFAULT DEFAULT_NULL where elsewhere

  syn match fortranOperator	"\(\(>\|<\)=\=\|==\|/=\|=\)"
  syn match fortranOperator	"=>"
  syn match fortranOperator	"::"
  syn match fortranOperator	":::"

  syn region fortranString	start=+"+ end=+"+	contains=fortranLeftMargin,fortranContinueMark,fortranSerialNumber
  syn keyword fortranIO		pad position action delim readwrite
  syn keyword fortranIO		eor advance nml

  syn keyword fortran90Intrinsic	adjustl adjustr all allocated any
  syn keyword fortran90Intrinsic	associated bit_size btest ceiling
  syn keyword fortran90Intrinsic	count cshift date_and_time
  syn keyword fortran90Intrinsic	digits dot_product eoshift epsilon exponent
  syn keyword fortran90Intrinsic	floor fraction huge iand ibclr ibits ibset ieor
  syn keyword fortran90Intrinsic	ior ishft ishftc lbound len_trim
  syn keyword fortran90Intrinsic	matmul maxexponent maxloc maxval merge
  syn keyword fortran90Intrinsic	minexponent minloc minval modulo mvbits nearest
  syn keyword fortran90Intrinsic	pack present product radix random_number
  syn match fortran90Intrinsic		"\<not\>\(\s*\.\)\@!"me=s+3
  syn keyword fortran90Intrinsic	random_seed range repeat reshape rrspacing scale
  syn keyword fortran90Intrinsic	selected_int_kind selected_real_kind scan
  syn keyword fortran90Intrinsic	shape size spacing spread set_exponent
  syn keyword fortran90Intrinsic	tiny transpose trim ubound unpack verify
  syn keyword fortran90Intrinsic	precision sum system_clock
  syn match fortran90Intrinsic	"\<kind\>\s*[(]"me=s+4

  syn match  fortranUnitHeader	"\<end\s*function"
  syn match  fortranUnitHeader	"\<end\s*interface"
  syn match  fortranUnitHeader	"\<end\s*module"
  syn match  fortranUnitHeader	"\<end\s*program"
  syn match  fortranUnitHeader	"\<end\s*subroutine"
  syn match  fortranRepeat	"\<end\s*do"
  syn match  fortranConditional	"\<end\s*where"
  syn match  fortranConditional	"\<select\s*case"
  syn match  fortranConditional	"\<end\s*select"
  syn match  fortranType	"\<end\s*type"
  syn match  fortranType	"\<in\s*out"

  syn keyword fortranUnitHeaderEx	procedure
  syn keyword fortranIOEx		namelist
  syn keyword fortranConditionalEx	while
  syn keyword fortran90IntrinsicEx	achar iachar transfer

  syn keyword fortranInclude		include
  syn keyword fortran90StorageClassR	sequence

syn match   fortranConditional	"\<end\s*if"
syn match   fortranIO        	contains=fortranOperator "\<e\(nd\|rr\)\s*=\s*\d\+"
syn match   fortranConditional	"\<else\s*if"

syn keyword fortranUnitHeaderR	entry
syn match fortranTypeR		display "double\s\+precision"
syn match fortranTypeR		display "double\s\+complex"
syn match fortranUnitHeaderR	display "block\s\+data"
syn keyword fortranStorageClassR	common equivalence data
syn keyword fortran77IntrinsicR	dble dprod
syn match   fortran77OperatorR	"\.\s*[gl][et]\s*\."
syn match   fortran77OperatorR	"\.\s*\(eq\|ne\)\s*\."

if b:fortran_dialect == "f95"
  syn keyword fortranRepeat		forall
  syn match fortranRepeat		"\<end\s*forall"
  syn keyword fortran95Intrinsic	null cpu_time
  syn keyword fortranType		elemental ELEMENTAL pure PURE
  if exists("fortran_more_precise")
    syn match fortranConstructName "\(\<end\s*forall\s\+\)\@<=\a\w*\>"
  endif
endif

syn cluster fortranCommentGroup contains=fortranTodo
syn match fortranComment	excludenl "!.*$" contains=@fortranCommentGroup

"cpp is often used with Fortran
syn match	cPreProc		"^\s*#\s*\(define\|ifdef\)\>.*"
syn match	cPreProc		"^\s*#\s*\(elif\|if\)\>.*"
syn match	cPreProc		"^\s*#\s*\(ifndef\|undef\)\>.*"
syn match	cPreCondit		"^\s*#\s*\(else\|endif\)\>"
syn region	cIncluded	contained start=+"[^(]+ skip=+\\\\\|\\"+ end=+"+ contains=fortranLeftMargin,fortranContinueMark,fortranSerialNumber
syn match	cIncluded		contained "<[^>]*>"
syn match	cInclude		"^\s*#\s*include\>\s*["<]" contains=cIncluded

"Synchronising limits assume that comment and continuation lines are not mixed
syn sync minlines=20

if exists("fortran_fold")
  syn sync fromstart
"  syn region fortranProgram transparent fold keepend start="^program\s\+\z(\a\w*\)" excludenl end="^end\s*\(program\(\s\+\z1\>\)\=\|$\)" contains=ALLBUT,fortranModule
  syn region fortranModule transparent fold keepend start="^\(virtual \)\?module\s\+\z(\a\w*\)" excludenl end="^end\s*\(module\(\s\+\z1\>\)\=\|$\)" contains=ALLBUT,fortranProgram

  syn region fortranSubroutine transparent fold keepend extend start="^!\=   \z(\a\w*\)\(.*\_s\_^!\=   !\)\@=" excludenl end="^!\=   end\s*\($\|subroutine\)" contains=ALLBUT,fortranProgram,fortranModule,fortranSubroutine,fortranFunction
  syn region fortranFunction transparent fold keepend extend start="^!\=   \z(\a\w*\)\(.* result.*\_s\_^!\=   !\)\@=" excludenl end="^!\=   end\s*\($\|function\)" contains=ALLBUT,fortranProgram,fortranModule,fortranSubroutine,fortranFunction
  syn region fortranInterface transparent fold keepend extend start="^\s*interface\s\+\z(\a\w*\)" excludenl end="\<end\s*\($\|interface\(\s\+\z1\>\)\=\)" contains=ALLBUT,fortranProgram,fortranModule
  syn region fortranType transparent fold keepend extend start="^!\=\s*\(array\|type\)\s\+\z(\a\w*\)" excludenl end="\<end\s*\($\|type\(\s\+\z1\>\)\=\)" contains=ALLBUT,fortranProgram,fortranModule,fortranSubroutine,fortranFunction
  syn region fortranData transparent fold keepend start="^!\=   data\s\+\z(\a\(\w\|[( :,)]\)*\)/" excludenl end="!\=/\s*\($\|!.*$\)" contains=ALLBUT,fortranProgram,fortranModule,fortranSubroutine,fortranFunction,fortranInterface,fortranData

"  syn region fortran77Loop transparent fold keepend start="\<do\s\+\z(\d\+\)" end="^\s*\z1\>" contains=ALLBUT,fortranUnitHeader,fortranStructure,fortranStorageClass,fortranType,fortranProgram,fortranModule,fortranSubroutine,fortranFunction,fortranBlockData
"  syn region fortran90Loop transparent fold keepend extend start="\(\<end\s\+\)\@<!\<do\(\s\+\a\|\s*$\)" excludenl end="\<end\s*do\>" contains=ALLBUT,fortranUnitHeader,fortranStructure,fortranStorageClass,fortranType,fortranProgram,fortranModule,fortranSubroutine,fortranFunction,fortranBlockData

  if exists("fortran_fold_conditionals")
"    syn region fortranIfBlock transparent fold keepend extend start="\(\<e\(nd\|lse\)\s\+\)\@<!\<if\s*(.\+)\s*then\>" end="\<end\s*if\>" contains=ALLBUT,fortranUnitHeader,fortranStructure,fortranStorageClass,fortranType,fortranProgram,fortranModule,fortranSubroutine,fortranFunction,fortranBlockData
"    syn region fortranCase transparent fold keepend extend start="\<select\s*case\>" end="\<end\s*select\>" contains=ALLBUT,fortranUnitHeader,fortranStructure,fortranStorageClass,fortranType,fortranProgram,fortranModule,fortranSubroutine,fortranFunction,fortranBlockData
  endif
endif

" The default highlighting differs for each dialect.
" Transparent groups:
" fortranParen, fortranLeftMargin
" fortranProgram, fortranModule, fortranSubroutine, fortranFunction,
" fortranBlockData
" fortran77Loop, fortran90Loop, fortranIfBlock, fortranCase
hi def link fortranStatement		Statement
hi def link fortranConstructName	Special
hi def link fortranConditional		Conditional
hi def link fortranRepeat		Repeat
hi def link fortranTodo			Todo
hi def link fortranContinueMark		Todo
hi def link fortranString		String
hi def link fortranNumber		Number
hi def link fortranOperator		Operator
hi def link fortranBoolean		Boolean
hi def link fortranLabelError		Error
hi def link fortranObsolete		Todo
hi def link fortranType			Type
hi def link fortranStructure		Type
hi def link fortranCall			fortranUnitHeader
hi def link fortranUnitHeader		fortranPreCondit
hi def link fortranReadWrite		fortran90Intrinsic
hi def link fortran95Intrinsic		fortran90Intrinsic
hi def link fortran77Intrinsic		fortran90Intrinsic
hi def link fortran90Intrinsic		Special

"if ( b:fortran_dialect == "elf" || b:fortran_dialect == "F" )
  hi def link fortranStatementOb	fortranObsolete
  hi def link fortran66Intrinsic	fortranObsolete
  hi def link fortran77IntrinsicR	fortranObsolete
  hi def link fortranUnitHeaderR	fortranObsolete
  hi def link fortranTypeR		fortranObsolete
  hi def link fortran77OperatorR	fortranObsolete
  hi def link fortranInclude		fortranObsolete
"else
"  hi def link fortranStatementOb	Statement
"  hi def link fortran66Intrinsic	fortran90Intrinsic
"  hi def link fortran77IntrinsicR	fortran90Intrinsic
"  hi def link fortranUnitHeaderR	fortranPreCondit
"  hi def link fortranTypeR		fortranType
"  hi def link fortran77OperatorR	fortranOperator
"  hi def link fortranInclude		Include
"endif

"if ( b:fortran_dialect == "F" )
  hi def link fortranLabelNumber	fortranObsolete
  hi def link fortranTarget		fortranObsolete
  hi def link fortranFormatSpec		fortranObsolete
  hi def link fortranFloatDExp		fortranObsolete
  hi def link fortranFloatNoDec		fortranObsolete
  hi def link fortranFloatIniDec	fortranObsolete
  hi def link fortranFloatEndDec	fortranObsolete
  hi def link fortranTypeEx		fortranObsolete
  hi def link fortranIOEx		fortranObsolete
  hi def link fortranStatementEx	fortranObsolete
  hi def link fortranStringEx		fortranObsolete
  hi def link fortran77IntrinsicEx	fortranObsolete
  hi def link fortranUnitHeaderEx	fortranObsolete
  hi def link fortranConditionalEx	fortranObsolete
  hi def link fortran90IntrinsicEx	fortranObsolete
"else
"  hi def link fortranLabelNumber	Special
"  hi def link fortranTarget		Special
"  hi def link fortranFormatSpec		Identifier
"  hi def link fortranFloatDExp		fortranFloat
"  hi def link fortranFloatNoDec		fortranFloat
"  hi def link fortranFloatIniDec	fortranFloat
"  hi def link fortranFloatEndDec	fortranFloat
"  hi def link fortranTypeEx		fortranType
"  hi def link fortranIOEx		fortranIO
"  hi def link fortranStatementEx	fortranStatement
"  hi def link fortranStringEx		fortranString
"  hi def link fortran77IntrinsicEx	fortran90Intrinsic
"  hi def link fortranUnitHeaderEx	fortranUnitHeader
"  hi def link fortranConditionalEx	fortranConditional
"  hi def link fortran90IntrinsicEx	fortran90Intrinsic
"endif
hi def link fortranFloat		Float

hi def link fortran90Identifier		fortranIdentifier
"Uncomment the next line if you want all fortran variables to be highlighted
"hi def link fortranIdentifier		Identifier
hi def link fortranPreCondit		PreCondit
hi def link fortranInclude		Include
hi def link cIncluded			fortranString
hi def link cInclude			Include
hi def link cPreProc			PreProc
hi def link cPreCondit			PreCondit
hi def link fortranParenError		Error
hi def link fortranComment		Comment
hi def link fortranSerialNumber		Todo
hi def link fortranTab			Error

" Vendor extensions
hi def link fortranExtraIntrinsic	Special

let b:current_syntax = "fortran"

" vim: ts=8 tw=132



" Insert modifications for Foo

syn keyword fortranStructure PTR 

syn match   fortranConditional "end"

syn keyword fortranType leaky recursive routinal functional get_from
syn keyword fortranType impure pure PURE elemental ELEMENTAL
syn keyword fortranType inlined_by_foo
syn keyword fortranType template

" Built-in and virtual types

syn keyword fortranType SELF_TYPE
syn keyword fortranType ELEMENT_TYPE
syn keyword fortranType OBJECT
syn keyword fortranType INTRINSIC 
syn keyword fortranType MAP KEY VAL

" Derived types and system

syn keyword fortranType TYPES 
syn keyword fortranType SYSTEM 
syn keyword fortranType DEBUG
syn keyword fortranType TIME
syn keyword fortranType PARALLEL
syn keyword fortranType COMMAND_LINE

" Intrinsic types and arrays

syn keyword fortranType STR BSTR BIN INT REAL CPX
syn keyword fortranType VEC  MAT  MAT3  MAT4  MAT5  MAT6  MAT7
syn keyword fortranType VEC_ MAT_ MAT3_ MAT4_ MAT5_ MAT6_ MAT7_
syn keyword fortranType OPVECTOR OPMATRIX

" Files & IO

syn keyword fortranType ARCHIVE
syn keyword fortranType TEXTFILE
syn keyword fortranType FILE
syn keyword fortranType CIF
syn keyword fortranType BUFFER 
syn keyword fortranType UNIT_NUMBER
syn keyword fortranType TABLE_COLUMN

" Gaussians, basis functions and shells

syn keyword fortranType GAUSSIAN
syn keyword fortranType GAUSSIAN2
syn keyword fortranType GAUSSIAN4 

syn keyword fortranType SHELL SHELL1 SHELL2 SHELL4 
syn keyword fortranType SHELLPAIR  SHELLQUARTET 
syn keyword fortranType SHELL1PAIR SHELL1QUARTET
syn keyword fortranType SLATERSHELL 
syn keyword fortranType COPPENSORBITAL 

" Bases

syn keyword fortranType BASIS
syn keyword fortranType SLATERBASIS
syn keyword fortranType COPPENSBASIS

" Integrals

syn keyword fortranType RYS
syn keyword fortranType GAUSSIAN_DATA
syn keyword fortranType RMS_INDICES RMS2_INDICES

" DFT and numerical integration

syn keyword fortranType BECKE_GRID
syn keyword fortranType LEBEDEV 
syn keyword fortranType QUADRATURE 
syn keyword fortranType DFT_FUNCTIONAL

" Symmetry

syn keyword fortranType SPACEGROUP 
syn keyword fortranType POINTGROUP
syn keyword fortranType IRREP

" Crystals, unit cells and reflection data

syn keyword fortranType CRYSTAL
syn keyword fortranType UNIT_CELL 
syn keyword fortranType DIFFRACTION_DATA 
syn keyword fortranType REFLECTION 
syn keyword fortranType MULTI_T_ADP

" Colours

syn keyword fortranType COLOR
syn keyword fortranType COLOR_FUNCTION 

" Plots and isosurfaces

syn keyword fortranType CAPPING_SQUARE
syn keyword fortranType ISOSURFACE 
syn keyword fortranType PLOT_GRID 
syn keyword fortranType MARCHINGCUBE

" SCF & wavefunctions

syn keyword fortranType SCFDATA 
syn keyword fortranType DIIS FOCK_DIIS

" Properties and analysis

syn keyword fortranType INTERPOLATOR
syn keyword fortranType ROBY

" Terms and polynomials
syn keyword fortranType TERM_1D
syn keyword fortranType TERM
syn keyword fortranType POLYNOMIAL

" Geminals
syn keyword fortranType GEMINAL_MF_SPECTRUM
syn keyword fortranType GEMINAL_MF_SCHEME

" Atoms, clusters and molecules

syn keyword fortranType ATOM 
syn keyword fortranType ATOM_GROUP
syn keyword fortranType CLUSTER
syn keyword fortranType MOLECULE 
syn keyword fortranType MOLECULE.BASE 
syn keyword fortranType MOLECULE.CE
syn keyword fortranType MOLECULE.CP
syn keyword fortranType MOLECULE.FOCK
syn keyword fortranType MOLECULE.GEM
syn keyword fortranType MOLECULE.GRID
syn keyword fortranType MOLECULE.INTS
syn keyword fortranType MOLECULE.MAIN
syn keyword fortranType MOLECULE.MISC
syn keyword fortranType MOLECULE.PLOT
syn keyword fortranType MOLECULE.PROP
syn keyword fortranType MOLECULE.REL
syn keyword fortranType MOLECULE.SCF
syn keyword fortranType MOLECULE.TAD
syn keyword fortranType MOLECULE.XTAL

syn keyword fortranOperator AND OR EQV NEQV NOT IN INOUT OUT

hi! link fortranContinueMark NONE
hi! Identifier term=underline ctermfg=Black guifg=Black

set comments=:!
