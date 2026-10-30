! ==================================================================
!
! Stand-alone Hirshfeld-atom refinement program
!
! Copyright (C) Dylan Jayatilaka, 2015
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Library General Public License for more details.
!
! You should have received a copy of the GNU Library General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA  02111-1307, USA.
! ==================================================================

! The version, commit and build date (GIT_VERSION ...), remade on every build
#  include "build_stamp"
program run_HAR

#  include "macros"

   include "run_har.use"

   implicit none

   STR(len=STR_SIZE) :: default_basis DEFAULT("def2-SVP")
   STR(len=STR_SIZE) :: default_grid DEFAULT("low")
   VEC(STR(len=9),6) :: grid_levels
   type(MOLECULE_TYPE), allocatable :: m
   type(COMMAND_LINE_TYPE) :: command_line
   STR(len=STR_SIZE) :: option
   STR(len=PATH_SIZE) :: value  ! an option value is often a path
   STR(len=STR_SIZE) :: spinorbitals
   STR(len=STR_SIZE) :: mess,etime,ctime
   STR(len=STR_SIZE) :: basis,basnam,gracc
   STR(len=PATH_SIZE) :: basdir  ! a path: STR is only 256
   STR(len=STR_SIZE) :: cif,guess,job,out,err,scf
   STR(len=STR_SIZE) :: relativity,nucleus
   STR(len=STR_SIZE) :: tail
   STR(len=STR_SIZE) :: std_f2,std_f,shx_f2,shx_f
   REAL :: dtol,fos,fzcut
   REAL :: ldtol,radius,ten
   VEC(STR(len=STR_SIZE),:), allocatable :: anharm,disperse
   VEC(INT,:), allocatable :: grp_charge
   VEC(INT,:), allocatable :: grp_mult
   REAL :: wavelen
   INT :: i,charge,mult
   INT :: maxlen  ! len() into a declared INT: see CLAUDE.md sec 8
   INT :: g,n_groups,maxit
   BIN :: do_4_only,extinct
   BIN :: h_ADPs,h_iso,h_pos
   BIN :: radius_is_0
   BIN :: tmetal,defragment
   BIN :: disk_sfs
   BIN :: mmcif,do_fragHAR
   BIN :: rescube
   type(TEXTFILE_TYPE), allocatable :: file

   ! Macro to create Tonto system object
   ! Initialise MPI parallel stuff too.
   TONTO_CREATE

   ! Set stdout & std_err to screen. std_err must be *opened* as well as
   ! created: it is closed and deleted below once the <job> name is known,
   ! and TEXTFILE:close_and_delete dies on a file that does not exist.
   call create_stdout
   call create_std_err
   call open_for_(std_err,"write")

   ! Defence in depth, not the fix. hart never reads a job file, but it once
   ! segfaulted on the second HAR cycle because MOLECULE.READ:read_archive
   ! reached for the global stdin to look for a trailing "normalise", and
   ! hart had created none -- dereferencing an unallocated TEXTFILE. That
   ! routine no longer touches stdin (the qualifier is an argument now, and
   ! read_archive_and_normalise is its keyword), so this is here only in case
   ! another library path reaches the same way. Create but do NOT open it:
   ! there is no stdin file, and an empty buffer answers such questions
   ! correctly. See TASKS_AND_HISTORY.md for the survey of who else could.
   call create_stdin

   ! Allowed values for --grid-accuracy.
   !
   ! Assigned here, NOT with a `data` statement in the declarations: the
   ! ANTLR4 translator silently DROPS a `data` statement at `program` scope
   ! (it keeps them at module scope). See TASKS_AND_HISTORY.md.
   grid_levels = ["very_low ","low      ","medium   ","high     ","very_high","extreme  "]

   ! Default option values
   basis = default_basis
   basdir = " "  ! see the TONTO_BASIS_SET_DIRECTORY fallback below
   charge = 0
   cif = "urea.cif"
   disk_sfs = FALSE
   dtol = TOL(2)
   do_4_only = FALSE
   err = " "
   extinct = FALSE
   fos = THREE
   fzcut = -ONE
   gracc = default_grid
   guess = "mos"
   h_ADPs = TRUE
   h_iso = FALSE
   h_pos = TRUE
   job = " "
   ldtol = TOL(5)
   maxit = 0  ! 0 = leave the library default (100) alone
   mmcif = FALSE
   mult = 1
   rescube = TRUE
   out = " "
 ! radius     = EIGHT*BOHR_PER_ANGSTROM 
   radius = ZERO
   defragment = TRUE
   shx_f2 = " "
   std_f2 = " "
   shx_f = " "
   std_f = " "
   relativity = " "  ! i.e. not set: follows the basis set, see below
   nucleus = "point"
   scf = "rhf"
   ten = TEN*BOHR_PER_ANGSTROM
   wavelen = -ONE  ! i.e. not set; see --wavelength

   ! Get command line !!!!!!!!!
   call set_defaults_(command_line)
   call process_options_(command_line)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! --help and --version report and stop; neither needs a <cif-file>, so the
   ! argument checks below must not run for them.
   if (NOT (has_option_(command_line,"help") OR has_option_(command_line,"version"))) then
      if (NOT has_arguments_(command_line)) then

            ! No arguments
         call text_(stdout,"Supply a <cif-file> and, if needed, a <hkl> file")
         call text_(stdout,"via the --shelx-f <hkl> or --shelx-f2 <hkl> options.")
         call text_(stdout,"Type 'hart --help' for additional help.")
         stop 1
      else if (command_line%n_args>1) then

            ! Too many arguments
         call text_(stdout,"Supply a <cif-file> and, if needed, a <hkl> file")
         call text_(stdout,"via the --shelx-f <hkl> or --shelx-f2 <hkl> options.")
         call text_(stdout,"Type 'hart --help' for additional help.")
         stop 1
      else

         ! First argument
         cif = command_line%arg(1)

         ! No .cif or .cif2 extension. Both are accepted: a .cif2 file is
         ! what a previous hart run writes, and what a restart needs.
         tail = filename_tail_(cif)
         if (NOT is_one_of_(tail,["cif ","cif2"])) then
            call text_(stdout,"Supply a cif file with a '.cif' or '.cif2' extension!")
            call text_(stdout,"Type 'hart --help' for additional help.")
            stop 1
         end if
      end if

      ! Get the <job>
      if (has_option_(command_line,"job")) then
         job = value_for_option_(command_line,"job")
      end if

      ! Braced integer lists, one entry per atom group. Read here rather than
      ! in the option loop below because they are vectors, not scalars, and
      ! COMMAND_LINE owns the brace/integer checking. Their length is checked
      ! against the real number of atom groups once the structure is read.
      if (has_option_(command_line,"group-charges")) then
         call int_vec_for_option_(command_line,"group-charges",grp_charge)
      end if
      if (has_option_(command_line,"group-multiplicities")) then
         call int_vec_for_option_(command_line,"group-multiplicities",grp_mult)
         DIE_IF(any(grp_mult<1),"run_HAR:run_HAR ... --group-multiplicities: must all be 1 or more")
      end if

      ! Preprocess main arguments
      if (job==" ") job = trim(filename_head_(cif))
      if (out==" ") out = trim(job)//".out"
      if (err==" ") err = trim(job)//".err"

      ! Set std_err properly
      call close_and_delete_(std_err)
      call destroy_(std_err)
      call create_std_err
      call set_name_(std_err,err)
      call open_for_(std_err,"write")
   end if  ! -- NOT help/version

   ! Analyze command line options
   do i = 1,command_line%n_options

      ! Get options
      option = command_line%option(i)
      value = command_line%option_value(i)

      ! Analyze options
      select case (option)

       ! case ("anharm        ");  anharm     = value.split
       ! case ("4th-order-only");  do_4_only  = value.to_bin
      case ("basis         "); basis = value
      case ("basis-dir     "); basdir = value
       ! case ("charge        ");  charge     = value.to_int
      case ("cluster-radius"); radius = to_real_(value)*BOHR_PER_ANGSTROM
      case ("complete-mol  "); defragment = to_bin_(value)  ! alias, see --defragment
      case ("defragment    "); defragment = to_bin_(value)
      case ("dispersion    "); disperse = split_(value)
      case ("disk-sfs      "); disk_sfs = to_bin_(value)
      case ("dtol          "); dtol = to_real_(value)
      case ("extinction    "); extinct = to_bin_(value)
      case ("fos           "); fos = to_real_(value)
      case ("fzcut         "); fzcut = to_real_(value)

         ! Braced integer lists; read above via COMMAND_LINE:int_vec_for_option,
         ! which is where the brace and integer checking lives.
      case ("group-charges       ")  ! already dealt with above
      case ("group-multiplicities")  ! already dealt with above

      case ("grid-accuracy "); gracc = value
      case ("max-iterations"); maxit = to_int_(value)
      case ("mmcif         "); mmcif = to_bin_(value)
      case ("residual-cube "); rescube = to_bin_(value)
      case ("wavelength    "); wavelen = to_real_(value)
      case ("h-adps        "); h_ADPs = to_bin_(value)
      case ("h-iso         "); h_iso = to_bin_(value)
      case ("h-pos         "); h_pos = to_bin_(value)  !test_new refine_H_pos Lorraine
      case ("job           ")  ! already dealt with above
       ! case ("ldtol         ");  ldtol      = value.to_real
       ! case ("mult          ");  mult       = value.to_int
      case ("shelx-f       "); shx_f = value
      case ("shelx-f2      "); shx_f2 = value
      case ("std-f         "); std_f = value
      case ("std-f2        "); std_f2 = value
      case ("scf           "); scf = value
      case ("relativity    "); relativity = value
      case ("nucleus       "); nucleus = value
       ! case ("scf-guess     ");  guess      = value
       ! case ("wavelength    ");  wavelength = value.to_real

      case ("version")
         call text_(stdout,"This is hart, from Tonto version"//TONTO_VERSION//" "//GIT_VERSION)
         stop

      case ("help")
         call text_(stdout,"SYNOPSIS")
         call flush_(stdout)
         call text_(stdout,"    hart [options] <cif-file>")
         call flush_(stdout)
         call text_(stdout,"    . Do a Hirshfeld-atom refinement (HAR) on a fragment of the crystal structure")
         call text_(stdout,"      whose initial atom coordinates and ADPs appear in the <cif-file>.")
         call flush_(stdout)
         call text_(stdout,"    . X-ray structure factors must be in the <cif-file> or in a given <hkl> file.")
         call flush_(stdout)
         call text_(stdout,"    . Output comprises a standard asymmetric-unit CIF and an enhanced tonto CIF")
         call text_(stdout,"      in CIF2 format which is necessary for restart")
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"EXPLANATION")
         call flush_(stdout)
         call text_(stdout,"    . HAR is a final 'polishing' least-sqaures refinement applied after a normal")
         call text_(stdout,"      normal X-ray refinement, with results in the <cif-file>. X-ray reflections")
         call text_(stdout,"      may be within the <cif-file> in standard format. Data as _shelx_hkl_file")
         call text_(stdout,"      comment ios currently not accepted.")
         call flush_(stdout)
         call text_(stdout,"    . HAR produces the best structure from available data, including H atom")
         call text_(stdout,"      bond lengths as accurate as from neutron measurements - provided data is")
         call text_(stdout,"      collected to at least 0.7 A^-1")
         call flush_(stdout)
         call text_(stdout,"    . HAR makes use of 'tailor-made' aspherical atomic form factors calculated")
         call text_(stdout,"      on-the-fily from a Hirshfeld-partitioned electron density (ED) - not from")
         call text_(stdout,"      spherical-atom form factors.")
         call flush_(stdout)
         call text_(stdout,"    . The ED is calculated from a gaussian <basis> set single determinant SCF")
         call text_(stdout,"      <wavefunction> - either SCF or DFT - for a fragment of the crystal embedded in")
         call text_(stdout,"      an electrostatic crystal field")
         call flush_(stdout)
         call text_(stdout,"    . The crystal field comprises atom-centered charges and dipoles placed on whole")
         call text_(stdout,"      molecules surrounding the the central fragment out to a desired <radius>. The")
         call text_(stdout,"      charges and dipoles on surrrouding molecules are themselves calculated from the")
         call text_(stdout,"      atoms on the central fragment, so must be calcuated iteratively")
         call flush_(stdout)
         call text_(stdout,"    . The fragment structure is chosen so that the leading symmetry-unique atoms are")
         call text_(stdout,"      in a 'realistic' chemical environment i.e. ideally the fragment is as large as")
         call text_(stdout,"      practically possible. Use of a GUI is helpful.")
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"RESTRICTIONS")
         call flush_(stdout)
         call text_(stdout,"    . The fragment must have zero overall charge")
         call flush_(stdout)
         call text_(stdout,"    . The fragment must have an even number of electrons i.e. be closed shell")
         call flush_(stdout)
         call text_(stdout,"    . All symmetry-unique atoms and molecules must appear in the fragment")
         call flush_(stdout)
         call text_(stdout,"    . Disordered structures may not be refined")
         call flush_(stdout)
         call text_(stdout,"    . Elements beyond Z=36 (Kr), up to Z=86 (Rn), need --relativity x2c and an")
         call text_(stdout,"      x2c basis set")
         call flush_(stdout)
         call text_(stdout,"    . Transition metals (Z=21 to 30, Sc to Zn) are not supported")
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"REFERENCES")
         call flush_(stdout)
         call text_(stdout,"    . 'Hirshfeld atom refinement'")
         call text_(stdout,"      S.C. Capelli, H.-B. Buergi, B. Dittrich, S. Grabowsky, D. Jayatilaka")
         call text_(stdout,"      (2014) IUCrJ 1 pp 361-379")
         call flush_(stdout)
         call text_(stdout,"    . 'X-ray structure refinement using aspherical atomic density functions obtained")
         call text_(stdout,"      from quantum-mechanical calculations'")
         call text_(stdout,"      D. Jayatilaka and B. Dittrich,")
         call text_(stdout,"      (2008) Acta Cryst. A 64, 383–393")
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"OPTIONS")
          ! stdout.flush    
          ! stdout.text("   --anharm '<atom-label-list>'")
          ! stdout.flush    
          ! stdout.text("       Refine the listed atoms anharmonically using a 4th or Gram-Charlier expansion.")
          ! stdout.text("       The <atom-label-list> must correspond to the *unique* labels in the <cif-file>")
          ! stdout.text("       and must be separated by spaces e.g. 'S1 Cl'")
         call flush_(stdout)
         call text_(stdout,"   --basis <basis>")
         call flush_(stdout)
         call text_(stdout,"       The gaussian atomic <basis> set to use. A file with the bases, also called")
         call text_(stdout,"       <basis>, must reside in the <basis-dir> folder (see --basis-dir option)")
         call flush_(stdout)
         call text_(stdout,"       Default: def2-SVP")
         call flush_(stdout)
         call text_(stdout,"       Other allowed values for <basis> are:")
         call flush_(stdout)
         call text_(stdout,"       Testing    Adequate    Excellent     Benchmark    Availability")
         call text_(stdout,"       ______________________________________________________________")
         call flush_(stdout)
         call text_(stdout,"       STO-3G     def2-SVP    def2-TZVP    def2-TZVPP    H--Kr       ")
         call text_(stdout,"                   cc-pVDZ      cc-pVTZ       cc-pVQZ    H--Kr (No K)")
         call text_(stdout,"       ______________________________________________________________")
         call flush_(stdout)
         call text_(stdout,"       For structures with atoms beyond Kr, the all-electron sets made for the")
         call text_(stdout,"       scalar-relativistic X2C hamiltonian (see --relativity), H--Rn:")
         call flush_(stdout)
         call text_(stdout,"       x2c-SVPall    x2c-TZVPall    x2c-TZVPPall")
         call flush_(stdout)
         call text_(stdout,"       Any other basis set with a file in the <basis-dir> folder may be used.")
         call flush_(stdout)
         call text_(stdout,"   --basis-dir <basis-dir>")
         call flush_(stdout)
         call text_(stdout,"       The location of the <basis-set> library files. See --basis option above.")
         call flush_(stdout)
         call text_(stdout,"       Default: the TONTO_BASIS_SET_DIRECTORY environment variable if it is")
         call text_(stdout,"                set, otherwise './basis_sets'")
          ! stdout.flush    
          ! stdout.text("   --charge <q>")
          ! stdout.flush    
          ! stdout.text("       Set the (integer) charge of the molecule in au. Default: 0.")
         call flush_(stdout)
         call text_(stdout,"   --shelx-f  <hkl>")
         call text_(stdout,"   --shelx-f2 <hkl>")
         call text_(stdout,"   --std-f    <hkl>")
         call text_(stdout,"   --std-f2   <hkl>")
         call flush_(stdout)
         call text_(stdout,"       Supply one of the different <hkl> reflection files. The --shelx file is in")
         call text_(stdout,"       fortran fixed format '(I4,I4,I4,8F,8F)' for h, k, l, F or F2, and sigma.")
         call text_(stdout,"       The --std file format is free and whitespace separated. Extra items may")
         call text_(stdout,"       at the end of each line, as well as blank lines and an (0 0 0) reflection.")
         call text_(stdout,"       Comments of any kind are disallowed.")
         call flush_(stdout)
         call text_(stdout,"   --cluster-radius <radius>")
         call flush_(stdout)
         call text_(stdout,"       The electric potential from whole molecules within <radius> Angstrom of")
         call text_(stdout,"       the fragment is modeled by Hirshfeld charges and dipoles placed at each")
         call text_(stdout,"       atom site.")
         call flush_(stdout)
         call text_(stdout,"       Default:  0")
         call text_(stdout,"       Maximum: 10")
         call flush_(stdout)
         call text_(stdout,"   --dispersion '<dispersion-coefficient-list>'")
         call flush_(stdout)
         call text_(stdout,"       Enter dispersion coefficients to be used in the refinement as a quoted")
         call text_(stdout,"       space-separated list of the element-symbol, f' and f'', repeated. Values")
         call text_(stdout,"       for elements which are not specified are assumed to be zero.")
         call flush_(stdout)
         call text_(stdout,"       Example: --dispersion 'S 0.04370 0.04026  Cl 0.05457 0.05224'")
         call flush_(stdout)
         call text_(stdout,"       Default: No dispersion used")
         call flush_(stdout)
         call text_(stdout,"   --disk-sfs [t|f]")
         call flush_(stdout)
         call text_(stdout,"       Whether to write aspherical atomic structure factors to disk.")
         call text_(stdout,"       With this information, write your own HAR refinement program!")
         call flush_(stdout)
         call text_(stdout,"       Default: f")
         call flush_(stdout)
         call text_(stdout,"   --dtol <dtol>")
         call flush_(stdout)
         call text_(stdout,"       Convergence tolerance <dtol> for the DIIS extrapolation procedure.")
         call flush_(stdout)
         call text_(stdout,"       Default: 0.01")
         call text_(stdout,"       Minimum: 0.00001")
         call flush_(stdout)
         call text_(stdout,"   --extinction [t|f]")
         call flush_(stdout)
         call text_(stdout,"       Whether to correct extinction via the Larson formula.")
         call flush_(stdout)
         call text_(stdout,"       Default: f")
          ! stdout.flush    
          ! stdout.text("   --refine-4th-order-only [t|f]'")
          ! stdout.flush    
          ! stdout.text("       Refine only the 4th order Gram-Charlier coefficients, leave any")
          ! stdout.text("       3rd order coefficients unchanged. If not specified in the CIF")
          ! stdout.text("       initial coeffiicents are assumed to be zero.")
         call flush_(stdout)
         call text_(stdout,"   --fos <ratio>")
         call flush_(stdout)
         call text_(stdout,"       The ratio of F/sigma below which the reflection is rejected.")
         call flush_(stdout)
         call text_(stdout,"       Default: 3")
         call text_(stdout,"       Minimum: 0")
         call flush_(stdout)
         call text_(stdout,"   --fzcut <F_z>")
         call flush_(stdout)
         call text_(stdout,"       If <cif-file> is from a previous HAR and has F_pred values, then remove")
         call text_(stdout,"       reflections which have a z-score greater than F_z=(F_pred-F_expt)/sigma.")
         call text_(stdout,"       Use this option with care only after an initial refinement to remove")
         call text_(stdout,"       outliers. By default no reflections are pruned.")
         call flush_(stdout)
         call text_(stdout,"   --grid-accuracy [very_low|low|medium|high|very_high|extreme]")
         call flush_(stdout)
         call text_(stdout,"       Accuracy needed for DFT integrations.")
         call flush_(stdout)
         call text_(stdout,"       Default: low")
         call flush_(stdout)
         call text_(stdout,"   --group-charges '{ <q1> <q2> ... }'")
         call flush_(stdout)
         call text_(stdout,"       The charge on each atom group, in group order. One entry per group;")
         call text_(stdout,"       hart stops if the count does not match. Quote the whole list: it is a")
         call text_(stdout,"       single option value, and the braces and any minus signs must survive")
         call text_(stdout,"       the shell.")
         call flush_(stdout)
         call text_(stdout,"       Example, for a zwitterion split into two residues:")
         call text_(stdout,"                --group-charges '{ 1 -1 }'")
         call flush_(stdout)
         call text_(stdout,"       Default: every group neutral")
         call flush_(stdout)
         call text_(stdout,"   --group-multiplicities '{ <m1> <m2> ... }'")
         call flush_(stdout)
         call text_(stdout,"       The spin multiplicity <m>=2S+1 of each atom group, in group order.")
         call text_(stdout,"       Same form and same one-per-group rule as --group-charges.")
         call flush_(stdout)
         call text_(stdout,"       Default: every group a closed-shell singlet")
         call flush_(stdout)
         call text_(stdout,"   --h-adps [t|f]")
         call flush_(stdout)
         call text_(stdout,"       Set whether to refine the H-atom ADPs. If set to 'f' only H atom positions")
         call text_(stdout,"       are refined.")
         call flush_(stdout)
         call text_(stdout,"       Default: t")
         call flush_(stdout)
         call text_(stdout,"   --h-iso [t|f] ")
         call flush_(stdout)
         call text_(stdout,"       Set isotropic H-atom ADPs and then refine isotropically.")
         call flush_(stdout)
         call text_(stdout,"       Default: f")
         call flush_(stdout)
         call text_(stdout,"       - If '--h-adps f' is set you get isotropic H ADPs without refinement")
         call text_(stdout,"       - Using this option will produce poorer X-H bond lengths")
         call flush_(stdout)
         call text_(stdout,"   --h-pos [t|f]")
         call flush_(stdout)
         call text_(stdout,"       Set whether to refine H atom positions.")
         call flush_(stdout)
         call text_(stdout,"       Default: t")
         call flush_(stdout)
         call text_(stdout,"   --help")
         call flush_(stdout)
         call text_(stdout,"       Print this documentation")
         call flush_(stdout)
         call text_(stdout,"   --job <job>")
         call flush_(stdout)
         call text_(stdout,"       The name of the job; used to name the output file and other intermediates.")
         call flush_(stdout)
         call text_(stdout,"       Default: The head of <cif-file> e.g. 'urea' if <cif-file> is 'urea.cif'.")
         call text_(stdout,"                The name of the output-file will then be <job>.out")
         call flush_(stdout)
         call text_(stdout,"   --max-iterations <n>")
         call flush_(stdout)
         call text_(stdout,"       Stop after at most <n> iterations, whether or not the refinement has")
         call text_(stdout,"       converged. Caps both the rigid-atom fit and the outer HAR cycle. Useful")
         call text_(stdout,"       for a smoke test, or to bound a long job. Must be 3 or more.")
         call flush_(stdout)
         call text_(stdout,"       Default: 100")
         call flush_(stdout)
         call text_(stdout,"   --mmcif [t|f]")
         call flush_(stdout)
         call text_(stdout,"       Read the <cif-file> as an mmCIF, i.e. with '_atom_site.' item names and")
         call text_(stdout,"       residue (compound sequence) labels. Residues are then split into capped")
         call text_(stdout,"       fragments by Ulf Ryde's method and refined by fragHAR.")
         call flush_(stdout)
         call text_(stdout,"       Default: f")
          ! stdout.flush    
          ! stdout.text("   --ldtol <ldtol>")
          ! stdout.flush    
          ! stdout.text("       Eliminate linear dependent basis functions i.e. eigenfunctions of the overlap")
          ! stdout.text("       matrix, with eigenvalue less that <tol>.  Default: 0.00001.")
          ! stdout.flush    
          ! stdout.text("   --mult <m>")
          ! stdout.flush    
          ! stdout.text("       The multiplicity of the <wavefunction> i.e. <m>=2S+1 where S is the total spin.")
          ! stdout.text("       Default: 1.")
         call flush_(stdout)
         call text_(stdout,"   --residual-cube [t|f]")
         call flush_(stdout)
         call text_(stdout,"       Write a residual-density cube over the unit cell after refinement.")
         call text_(stdout,"       This is by far the largest file a refinement produces -- megabytes even")
         call text_(stdout,"       for a dipeptide -- so turn it off for large structures or batch runs.")
         call flush_(stdout)
         call text_(stdout,"       Default: t")
         call flush_(stdout)
         call text_(stdout,"   --scf <wavefunction>")
         call flush_(stdout)
         call text_(stdout,"       The scf <wavefunction> to be used. Allowed options:")
         call flush_(stdout)
         call text_(stdout,"       rhf     restricted Hartree-Fock")
         call text_(stdout,"       rks     restricted Kohn-Sham/BLYP")
         call flush_(stdout)
         call text_(stdout,"       Default: rhf")
         call flush_(stdout)
         call text_(stdout,"   --relativity [none|x2c]")
         call flush_(stdout)
         call text_(stdout,"       The relativistic hamiltonian. x2c is the scalar exact two-component")
         call text_(stdout,"       one-electron hamiltonian, needed for atoms beyond Kr.")
         call flush_(stdout)
         call text_(stdout,"       Default: x2c if the <basis> name begins with x2c-, otherwise none")
         call flush_(stdout)
         call text_(stdout,"   --nucleus [point|gaussian]")
         call flush_(stdout)
         call text_(stdout,"       The model of the nuclear charge. gaussian needs --relativity x2c.")
         call flush_(stdout)
         call text_(stdout,"       Default: point")
          ! stdout.text("       uhf     unrestricted Hartree-Fock")
          ! stdout.text("       uks     unrestricted Kohn-Sham/BLYP")
          ! stdout.text("       gchf    general-complex (relativistic) Hartree-Fock")
          ! stdout.flush    
          ! stdout.text("   --scf-guess [mos|density]")
          ! stdout.flush    
          ! stdout.text("       Initial guess for SCF <wavefunction> method.")
          ! stdout.flush    
         call flush_(stdout)
         call text_(stdout,"   --wavelength <wavelength>")
         call flush_(stdout)
         call text_(stdout,"       The <wavelength> of the X-radiation, in Angstrom. Needed only when the")
         call text_(stdout,"       <cif-file> has no _diffrn_radiation_wavelength item; it defines the")
         call text_(stdout,"       Bragg angle of each reflection.")
         call flush_(stdout)
         call text_(stdout,"       Default: taken from the <cif-file>")
         call flush_(stdout)
         call text_(stdout,"   --defragment [t|f]")
         call flush_(stdout)
         call text_(stdout,"       Complete the molecules before refining anything. A CIF holds the")
         call text_(stdout,"       asymmetric unit, which is a quarter of a molecule for urea and a half")
         call text_(stdout,"       for many structures; HAR runs a quantum calculation on whatever")
         call text_(stdout,"       fragment it is handed, so refining an incomplete molecule gives a")
         call text_(stdout,"       converged and meaningless answer. With 't', crystal symmetry is")
         call text_(stdout,"       applied first to grow every fragment into whole molecules.")
         call flush_(stdout)
         call text_(stdout,"       WARNING: say 'f' for a network solid -- diamond, silica, a")
         call text_(stdout,"       coordination polymer, anything covalently bonded without end. There")
         call text_(stdout,"       is no whole molecule to complete in such a structure, so the growth")
         call text_(stdout,"       has no natural stopping point and the run will not terminate. This")
         call text_(stdout,"       option defaults to TRUE, so a network solid must turn it off.")
         call flush_(stdout)
         call text_(stdout,"       Default: TRUE")
         call flush_(stdout)
         call text_(stdout,"   --complete-mol [t|f]")
         call flush_(stdout)
         call text_(stdout,"       An accepted spelling of --defragment, kept because existing scripts")
         call text_(stdout,"       write it. Identical in effect.")
         call flush_(stdout)
         call text_(stdout,"       Default: TRUE")
         call flush_(stdout)
         call text_(stdout,"   --version")
         call flush_(stdout)
         call text_(stdout,"       Print the version of hart and stop")
         stop

      case default
         call text_(stdout,"Type 'hart --help' for help.")
         call flush_(stdout)
         allocate(tonto%known_keywords(31))
         tonto%known_keywords(1) = "basis         "
         tonto%known_keywords(2) = "basis-dir     "
         tonto%known_keywords(3) = "cluster-radius"
         tonto%known_keywords(4) = "complete-mol  "
         tonto%known_keywords(5) = "defragment    "
         tonto%known_keywords(6) = "dispersion    "
         tonto%known_keywords(7) = "disk-sfs      "
         tonto%known_keywords(8) = "dtol          "
         tonto%known_keywords(9) = "extinction    "
         tonto%known_keywords(10) = "fos           "
         tonto%known_keywords(11) = "fzcut         "
         tonto%known_keywords(12) = "group-charges       "
         tonto%known_keywords(13) = "group-multiplicities"
         tonto%known_keywords(14) = "grid-accuracy "
         tonto%known_keywords(15) = "max-iterations"
         tonto%known_keywords(16) = "mmcif         "
         tonto%known_keywords(17) = "residual-cube "
         tonto%known_keywords(18) = "wavelength    "
         tonto%known_keywords(19) = "h-adps        "
         tonto%known_keywords(20) = "h-iso         "
         tonto%known_keywords(21) = "h-pos         "
         tonto%known_keywords(22) = "job           "
         tonto%known_keywords(23) = "shelx-f       "
         tonto%known_keywords(24) = "shelx-f2      "
         tonto%known_keywords(25) = "std-f         "
         tonto%known_keywords(26) = "std-f2        "
         tonto%known_keywords(27) = "scf           "
         tonto%known_keywords(28) = "relativity    "
         tonto%known_keywords(29) = "nucleus       "
         tonto%known_keywords(30) = "version"
         tonto%known_keywords(31) = "help"
         call unknown_(tonto,option,"run_HAR:run_HAR")
         deallocate(tonto%known_keywords)
         stop 1
      end select
   end do

   ! Initialize output file
   call destroy_(stdout)
   call create_(stdout)
   call set_name_(stdout,out)
   call open_for_(stdout,"write")
   call set_stdout_unit_(tonto,stdout%unit)

   ! Report
   call text_(stdout,"======================================")
   call text_(stdout,"Welcome to the hart refinement program")
   call text_(stdout,"======================================")
   call flush_(stdout)
   call text_(stdout,"This is a command line program for performing Hirshfeld atom")
   call text_(stdout,"refinement (HAR) on a crystal structure stored in a CIF file")
   call text_(stdout,"using the Tonto library for quantum crystallography.")
   call flush_(stdout)
   call text_(stdout,"For detailed help, run the command:")
   call flush_(stdout)
   call text_(stdout,"   hart --help")
   call flush_(stdout)
   call text_(stdout,"If you publish any of the results from this program please")
   call text_(stdout,"cite the following original scientific publications:")
   call flush_(stdout)
   call text_(stdout,"   D. Jayatilaka & B. Dittrich (2008) Acta Cryst. A 64 p. 383")
   call text_(stdout,"   S. Capelli et al (2014) IUCrJ 1 p. 361")
   call text_(stdout,"   M. Woinska et al (2016) Sci. Adv. 2 e1600192")
   call flush_(stdout)
   call flush_(stdout)
   call text_(stdout,"===============")
   call text_(stdout,"Program options")
   call text_(stdout,"===============")
   call flush_(stdout)
   call text_(stdout,"You have run hart with the following options:")
   call put_command_optarg_(command_line)
   call flush_(stdout)
   call text_(stdout,"The hart program version is:")
   call text_(stdout,"                                                               ")
   call text_(stdout,"Version: "//TONTO_VERSION//" v. "//GIT_VERSION)
   call text_(stdout,"Platform: "//TONTO_PLATFORM)
   call text_(stdout,"Build-date: "//TONTO_BUILD_DATE)
   call flush_(stdout)
   call flush_(stdout)
   call text_(stdout,"=======")
   call text_(stdout,"WARNING")
   call text_(stdout,"=======")
   call flush_(stdout)
   call text_(stdout,"This software is always under heavy development and many")
   call text_(stdout,"authors release codes before they are fully tested. Do not")
   call text_(stdout,"use codes which have not been tested if they have not been")
   call text_(stdout,"published in the literature. Contact original authors, who")
   call text_(stdout,"will generally be interested to obtain help with testing,")
   call text_(stdout,"validation, and publication.")
   call flush_(stdout)
   call flush_(stdout)
   call text_(stdout,"=======")
   call text_(stdout,"License")
   call text_(stdout,"=======")
   call flush_(stdout)
   call text_(stdout,"This program is distributed under the terms of the GNU")
   call text_(stdout,"Lesser General Public License (LGPL) version 3, the full")
   call text_(stdout,"text of which is available at:")
   call flush_(stdout)
   call text_(stdout,"  www.gnu.org/licenses/lgpl-3.0.en.html")

 ! stdout.flush
 ! stdout.flush
 ! stdout.text("============")
 ! stdout.text("Contributors")
 ! stdout.text("============")

 ! stdout.flush
 ! stdout.text("Tonto & the hart program has been brought to you with the work")
 ! stdout.text("of the following groups and companies:")
 ! stdout.flush
 ! stdout.text(". The University of Western Australia (Jayatilaka)")
 ! stdout.text(". The University of Bremen (Grabowsky)")
 ! stdout.text(". The Slovak University of Technology (Bucinsky)")
 ! stdout.text(". The University of Warsaw (Dominiak,Wozniak)")
 ! stdout.text(". Olex2 @ Durham University (Puschmann,Dolomanov)")

 ! stdout.flush
 ! stdout.text("Important individual contributions are listed below")
 ! stdout.text("in roughly chronological order:")
 ! stdout.flush
 ! stdout.text(". D. Jayatilaka     - System design & conception.  Help.")
 ! stdout.text("                      Hirshfeld atom refinement (HAR)")
 ! stdout.text("                      X-ray constrained wavefunctions (XCW)")
 ! stdout.flush                     
 ! stdout.text(". D. Grimwood       - System design, integrals, XCW")
 ! stdout.flush                     
 ! stdout.text(". S. Wolff          - DFT, integration grids, isosurfaces")
 ! stdout.text("                      Roby bond indices")
 ! stdout.flush                     
 ! stdout.text(". C. Taylor         - Roby bond indices")
 ! stdout.flush                     
 ! stdout.text(". A. Whitton        - Joint X-ray/PND fitting")
 ! stdout.text("  M. Hudak")        
 ! stdout.flush                     
 ! stdout.text(". A. Whitten        - Sum-over-states (SOS) polarisabilities")
 ! stdout.text("  M. Spackman         in-crystal refractive indices")
 ! stdout.flush                     
 ! stdout.text(". M. Turner         - CPHF, Polarisabilities, energies")
 ! stdout.text("                      isosurface face capping,")
 ! stdout.text("                      CrystalkExplorer interface")
 ! stdout.flush                     
 ! stdout.text(". L. Bucinsky       - Relativistic methods")
 ! stdout.flush                     
 ! stdout.text(". B. Dittrich       - Original HAR method, dipersion")
 ! stdout.flush                     
 ! stdout.text(". M. Susli          - gnuplot picture output")
 ! stdout.flush                     
 ! stdout.text(". S. Grabowsky      - X-ray wavefunction refinement (XWR)")
 ! stdout.text("                      ELI-D and bonding tools")
 ! stdout.flush
 ! stdout.text(". J. Kreszczakovska - Anharmonicity & babies")
 ! stdout.text("  (nee Bak)")
 ! stdout.flush
 ! stdout.text(". H.-B. Buergi      - HAR validation & anharmonicity")
 ! stdout.text("  S. Capelli")
 ! stdout.text("  L. Malaspina")
 ! stdout.flush                     
 ! stdout.text(". M. Woinska        - H atom positions & ADPs; F2 refinement")
 ! stdout.flush                     
 ! stdout.text(". P. Cassam-Chenai  - Geminal/CI methods")
 ! stdout.flush                     
 ! stdout.text(". P. Spackman       - Build system, Travis test integration")
 ! stdout.text("                      B3LYP, spherical harmonics, multipoles")
 ! stdout.text("                      isosurface informatics")
 ! stdout.text("                      CrystalExplorer interface")
 ! stdout.flush                     
 ! stdout.text(". K. Abdalhussein   - Bond indices, bond energies,")
 ! stdout.text("                      finite temperature SCF")
 ! stdout.flush                     
 ! stdout.text(". M. Davidson       - Nonorthogonal SCF & ELMO")
 ! stdout.flush                     
 ! stdout.text("Did I forget anyone? Let me know.")

 ! stdout.flush
 ! stdout.flush
 ! stdout.text("Tonto library methods will now be called ...")
 ! stdout.flush
 ! stdout.flush

   ! Start timer !!!!!!!
   call start_timing_(std_time)
   !!!!!!!!!!!!!!!!!!!!!

   ! Post-processing
 ! DIE_IF(shx_f2==" " AND shx_f==" " AND std_f2==" " AND std_f==" ", "no .hkl file supplied")
   ! Where are the basis sets? If --basis-dir was not given, fall back to the
   ! environment, exactly as run_molecule.foo does, so that hart can be run
   ! from anywhere without a machine-specific path on the command line.
   if (basdir==" ") then

      ! status is -1 when the value was TRUNCATED to fit, and >0 when the
      ! variable is simply unset. Treating those alike -- as this did --
      ! turns an over-long path into a silent fallback to ./basis_sets, and
      ! the failure then surfaces as "no library basis set", naming the
      ! basis rather than the path. See TASKS_AND_HISTORY.md, "long paths".
      call get_environment_variable(BASIS_LIBRARY_ENV_NAME,basdir,status=i)
      maxlen = len(basdir)
      DIE_IF(i<0,"run_HAR:run_HAR ... the "//BASIS_LIBRARY_ENV_NAME//" path is too long: it does "//"not fit in "//trim(to_str_(maxlen))//" characters")
      if (i/=0) basdir = " "
   end if
   if (basdir==" ") basdir = "./basis_sets"
   ! No check on the directory itself: whether INQUIRE reports a directory as
   ! existing is compiler dependent. The basis file inside it is checked below,
   ! and that message names the full path anyway.
   DIE_IF(dtol<TOL(5),"run_HAR:run_HAR ... dtol is too small: "//trim(to_str_(dtol)))
   DIE_IF(dtol>TOL(1),"run_HAR:run_HAR ... dtol is too large: "//trim(to_str_(dtol)))
 ! DIE_IF(NOT ldtol <TOL(5), "ldtol is too small: "//trim(ldtol.to_str))
   DIE_IF(fos<ZERO,"run_HAR:run_HAR ... fos ratio must be positive")
   DIE_IF(radius<ZERO,"run_HAR:run_HAR ... radius must be positive")
   DIE_IF(radius>ten,"run_HAR:run_HAR ... radius must be less than 10 angstrom")
   DIE_IF(NOT is_one_of_(gracc,grid_levels),"run_HAR:run_HAR ... unknown grid accuracy: "//trim(gracc))
   ! Validated here, not in the setter: DIFFRACTION_DATA's own check is an
   ! ENSURE and compiles away in the optimised build every user has. The lower
   ! bound is min_iterations, which defaults to 2.
   DIE_IF(maxit<0,"run_HAR:run_HAR ... --max-iterations must not be negative")
   DIE_IF(maxit>0 AND maxit<3,"run_HAR:run_HAR ... --max-iterations must be 3 or more (min_iterations is 2)")
   DIE_IF(NOT is_one_of_(guess,["mos    ","density"]),"run_HAR:run_HAR ... unknown initial guess: "//trim(guess))
   DIE_IF(NOT is_one_of_(scf,["rhf ","rks "]),"run_HAR:run_HAR ... unknown scf: "//trim(scf))

   ! The x2c basis sets are made for the X2C hamiltonian
   if (relativity==" ") then
      relativity = "none"
      if (basis(1:4)=="x2c-") relativity = "x2c"
   end if
   DIE_IF(NOT is_one_of_(relativity,["none","x2c "]),"run_HAR:run_HAR ... unknown relativity: "//trim(relativity))
   DIE_IF(NOT is_one_of_(nucleus,["point   ","gaussian"]),"run_HAR:run_HAR ... unknown nucleus: "//trim(nucleus))
   DIE_IF(nucleus=="gaussian" AND relativity/="x2c","run_HAR:run_HAR ... --nucleus gaussian needs --relativity x2c")

   ! Basis exists?
   basnam = trim(basdir)//"/"//trim(basis)
   call create_(file,basnam)
   DIE_IF(NOT exists_(file),"run_HAR:run_HAR ... basis set does not exist: "//trim(basnam))
   call destroy_(file)

   ! Initialize molecule
   call create_(m)
   call set_indices_(GAUSSIAN_DATA_L_MAX)
   call put_banner_
   call set_name_(m,job)
   call set_basis_directory_(m,basdir)
   call set_basis_name_(m,basis)
   m%use_spherical_basis = TRUE  ! before the atoms are read
   m%nucleus_model = nucleus
   call set_charge_(m,charge)
   call set_spin_multiplicity_(m,mult)

   ! Read CIF
   call create_(m%cif)
   call set_file_name_(m%cif,cif)

   ! An mmCIF? This must be set BEFORE process_CIF, since it selects which
   ! item names are looked for (_atom_site.* rather than _atom_site_*) and
   ! brings in the compound_sequence_id's that Ryde capping groups on.
   if (mmcif) call set_is_mmCIF_(m%cif,TRUE)
   call process_CIF_(m)
   call update_(m%cif)

   ! Complete whole molecules?
   if (defragment) then
      call defragment_(m)
   end if

   ! Set job name
   call set_name_(m,job)

   ! More checks
   DIE_IF(NOT allocated(m%crystal),"run_HAR:run_HAR ... no crystal information available!")
   DIE_IF(NOT allocated(m%crystal%xray_data),"run_HAR:run_HAR ... no x-ray F structure factors supplied!")
   DIE_IF(NOT allocated(m%atom),"run_HAR:run_HAR ... no atoms specified")
   DIE_IF(any(m%atom%atomic_number>36) AND relativity/="x2c","run_HAR:run_HAR ... atoms beyond Kr need --relativity x2c and an x2c basis set")
   DIE_IF(any(m%atom%atomic_number>86),"run_HAR:run_HAR ... hart is not available for atoms beyond Rn, Z>86")
   tmetal = any(m%atom%atomic_number>=21 AND m%atom%atomic_number<=30)
   DIE_IF(tmetal,"run_HAR:run_HAR ... hart is not routinely available for transition metals")

   ! Isotropic H ADPs? Set them.
   if (h_iso) call set_isotropic_H_ADP_(m%atom)

   ! Diffraction data info !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   call set_data_kind_(m%crystal%xray_data,"x-ray")
   call set_partition_model_(m%crystal%xray_data,"oc-hirshfeld")
   call set_refine_extinction_(m%crystal%xray_data,extinct)
   call set_use_text_SFs_(m%crystal%xray_data,disk_sfs)
   call set_do_residual_cube_(m%crystal%xray_data,rescube)

   ! Cap the refinement. Bounds BOTH loops -- the inner rigid-atom fit and the
   ! outer HAR cycle -- since too_many_fit_iterations and too_many_ref_iterations
   ! are both measured against max_iterations.
   if (maxit>0) call set_max_iterations_(m%crystal%xray_data,maxit)

   ! Wavelength. Not every CIF carries _diffrn_radiation_wavelength, and
   ! DIFFRACTION_DATA defaults it to -1 i.e. "not set", which leaves theta
   ! undefined for every reflection. Only override when asked. The option is
   ! in Angstrom and converted here, exactly as the CIF reader does with
   ! _diffrn_radiation_wavelength; the stored quantity is in Bohr.
   if (wavelen>ZERO) call set_wavelength_(m%crystal%xray_data,wavelen*BOHR_PER_ANGSTROM)

   ! Ulf Ryde's capped residues. Only meaningful for an mmCIF, which is where
   ! the compound_sequence_id's come from, so it follows --mmcif rather than
   ! being an option of its own: splitting a covalent chain into residues
   ! WITHOUT capping would leave dangling bonds, so there is no second choice
   ! to offer. A multi-molecule (non-polymer) asymmetric unit needs no capping
   ! and is grouped by plain connectivity instead -- see below.
   if (mmcif) call set_use_Ryde_capping_(m%crystal%xray_data,TRUE)

   ! Dispersion
   if (allocated(disperse)) then
      call set_dispersion_coefficients_(disperse)
      call set_tabular_dispersion_(m%atom)
      call set_add_dispersion_to_F_calc_(m%crystal%xray_data,TRUE)
   end if

   ! Anharmonicity
   if (allocated(anharm)) then
      call set_refine_4th_order_(m%crystal%xray_data,anharm)
      call set_refine_4th_order_only_(m%crystal%xray_data,do_4_only)
   end if

   ! Refine isotropic H ADPs?
   if (h_iso) call set_refine_H_U_iso_(m%crystal%xray_data,TRUE)

   ! Refine H ADPs at all?
   call set_refine_H_ADPs_(m%crystal%xray_data,h_ADPs)

   ! Refine H positions at all?
   call set_refine_H_positions_(m%crystal%xray_data,h_pos)

   ! Read reflections from a separate file?
   if (shx_f2/=" ") then
      call create_(file,shx_f2)
      call open_for_(file,"read")
      call read_shelx_F2_hkl_file_(m%crystal%xray_data%reflections,file)
      call destroy_(file)
   else if (shx_f/=" ") then
      call create_(file,shx_f)
      call open_for_(file,"read")
      call read_shelx_F_hkl_file_(m%crystal%xray_data%reflections,file)
      call destroy_(file)
   else if (std_f2/=" ") then
      call create_(file,std_f2)
      call open_for_(file,"read")
      call read_std_F2_hkl_file_(m%crystal%xray_data%reflections,file)
      call destroy_(file)
   else if (std_f/=" ") then
      call create_(file,std_f)
      call open_for_(file,"read")
      call read_std_F_hkl_file_(m%crystal%xray_data%reflections,file)
      call destroy_(file)
   end if

   ! Finalize data, and do data pruning
   ! --fos 0 means "no cutoff", and the way to express that is NOT to set it:
   ! DIFFRACTION_DATA:set_F_sigma_cutoff ENSUREs val>ZERO and says so in its own
   ! comment ("make sure zero is not entered ... just leave it off!"), while the
   ! field DEFAULTS to -TWO and every consumer tests `> ZERO`. Calling it with 0
   ! therefore worked only in release, where the ENSURE compiles away, and
   ! aborted in any debug build -- which is exactly how it was found.
   if (fos>ZERO) call set_F_sigma_cutoff_(m%crystal%xray_data,fos)
   if (fzcut>ZERO) call set_F_z_cutoff_(m%crystal%xray_data,fzcut)
   call update_(m%crystal%xray_data,m%crystal%unit_cell,m%crystal%spacegroup)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! Atom groups !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   ! Remake them now that use_Ryde_capping is known. This mirrors what a tonto
   ! job does: the groups are first made during process_CIF, when capping is
   ! still off, and remade when the crystal block is read -- MOLECULE.MAIN's
   ! read_crystal ends in exactly this call. Without it, an --mmcif run keeps
   ! the connectivity groups it built earlier and Ryde capping never happens.
   call update_atom_basis_group_info_(m)
   n_groups = 0
   if (allocated(m%atom_group)) n_groups = size(m%atom_group)

   ! fragHAR, or plain HAR? Decided by the data, not by an option: more than
   ! one group means more than one molecule (or capped residue) in the
   ! asymmetric unit, which is precisely the case HAR_refinement cannot do.
   do_fragHAR = n_groups>1

   ! Charges and multiplicities are per group, so the lists must be the length
   ! the structure actually has. Getting this wrong silently -- refining a
   ! zwitterion as two neutral fragments, say -- gives a converged, plausible,
   ! wrong answer, so it is a DIE and the message names both numbers.
   if (allocated(grp_charge)) then
      DIE_IF(size(grp_charge)/=n_groups,"run_HAR:run_HAR ... --group-charges has "//trim(to_str_(size(grp_charge)))//" entries but there are "//trim(to_str_(n_groups))//" atom groups")
   end if
   if (allocated(grp_mult)) then
      DIE_IF(size(grp_mult)/=n_groups,"run_HAR:run_HAR ... --group-multiplicities has "//trim(to_str_(size(grp_mult)))//" entries but there are "//trim(to_str_(n_groups))//" atom groups")
   end if

   ! Self-consistent cluster charges are a whole-molecule SCF idea and the
   ! fragment SCF does not use them. Silently ignoring --cluster-radius would
   ! hand back a refinement that is not the one that was asked for.
   DIE_IF(do_fragHAR AND radius>ZERO,"run_HAR:run_HAR ... --cluster-radius is not (yet) available with fragment refinement")
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !!!!!!!!!
   call put_(m)
   !!!!!!!!!

   ! Apply the per-group charges and multiplicities, then remake the group
   ! sub-molecules so they pick them up. Same two steps, in the same order, as
   ! MOLECULE.MAIN:read_atom_groups -- read onto the groups, then update.
   if (allocated(grp_charge) OR allocated(grp_mult)) then

      ! .atom_group_charges is the copy that SURVIVES. set_connected_atom_groups
      ! re-applies it every time the groups are rebuilt from the connection
      ! table, and fragHAR_refinement rebuilds them on every LS cycle. A charge
      ! written only onto the group itself would be dropped there.
      if (allocated(grp_charge)) then
         call create_(m%atom_group_charges,n_groups)
         m%atom_group_charges = grp_charge
      end if
      do g = 1,n_groups
         if (allocated(grp_charge)) call set_charge_(m%atom_group(g),grp_charge(g))
         if (allocated(grp_mult)) call set_spin_multiplicity_(m%atom_group(g),grp_mult(g))
      end do
      call update_atom_basis_group_info_(m)
   end if

   ! Set Becke grid
   call create_(m%becke_grid)
   call set_atom_info_(m%becke_grid,m%atom)
   call set_accuracy_(m%becke_grid,gracc)
   call set_grid_data_(m%becke_grid)

   ! Initial SCF - only if using cluster charges
   call create_(m%SCF_data)
   if (do_fragHAR) then

      ! Each fragment gets its own SCF, driven by MOLECULE.SCF:fragment_SCF.
      ! The kinds are literally "fragment-rhf" / "fragment-rks" -- see the
      ! dispatch in molecule.scf.foo and the SCF_kind case list in scf_data.foo.
      call set_SCF_kind_(m%SCF_data,"fragment-"//trim(scf))
   else
      call set_SCF_kind_(m%SCF_data,scf)
   end if
   call set_relativity_kind_(m%SCF_data,relativity)
   spinorbitals = spinorbital_genre_(m%SCF_data)
   call set_initial_density_(m%SCF_data,"promolecule")

   ! NO second guess call here. The block that used to sit here was
   !
   !    select case (guess)
   !    case ("density"); m.SCF_data.set_initial_MOs(spinorbitals)
   !    case ("mos    "); m.SCF_data.set_initial_density(spinorbitals)
   !    end
   !
   ! and it was fatal. "guess" is always "mos" (--scf-guess is one of the
   ! frozen options), so it always ran the second branch, and
   ! SCF_DATA:set_initial_density("restricted") does not mean "guess a
   ! restricted density" -- it rewrites .initial_density to "r", i.e. *read*
   ! a restricted density from an SCF archive. No archive exists on a first
   ! run, so the SCF read garbage and segfaulted. It also silently discarded
   ! the promolecule guess set on the line above. (The two branches are
   ! swapped against their own names as well: "density" called set_initial_MOs.)
   !
   ! The equivalent tonto job -- tests/long/urea_rhf_STO-3G_HAR/stdin -- sets
   ! `initial_density= promolecule` and nothing else, which is what hart now
   ! does. Reviving --scf-guess means rewriting this correctly, not restoring
   ! the above. See TASKS_AND_HISTORY.md.
   call set_convergence_(m%SCF_data,TOL(3))
   call set_convergence_tolerance_(m%SCF_data%SCF_DIIS,dtol)
   call set_linear_dependence_tol_(m%SCF_data,ldtol)

   ! Damping and level shifting for the fragment SCF. Fragments are charged
   ! and often open-shell-looking near the promolecule guess, so the plain
   ! DIIS settings above oscillate. These are the values the equivalent tonto
   ! job uses (tests/long/gly_ala_fragHAR_rhf_STO-3G/stdin) -- hardcoded here
   ! in the same spirit as the SCF/DIIS tolerances hart already fixes.
   if (do_fragHAR) then
      call set_using_damping_(m%SCF_data,TRUE)
      call set_damp_factor_(m%SCF_data,0.99d0)
      call set_using_level_shift_(m%SCF_data,TRUE)
      call set_level_shift_(m%SCF_data,100.0d0)
   end if
   call update_(m%SCF_data)

   ! A whole-of-asymmetric-unit SCF, but ONLY for a single fragment. With more
   ! than one fragment there is no single molecule to converge -- the fragments
   ! are separate wavefunctions and fragHAR_refinement does their SCF itself
   ! (via fragment_SCF, first call before the LS cycle). The tonto job has its
   ! `scf` keyword commented out for the same reason.
   if (NOT do_fragHAR) then

      !!!!!!!!!
      call scf_(m)
      !!!!!!!!!

      ! Cluster/ED SCF settings
      radius_is_0 = abs(radius)<TOL(2)
      if (NOT radius_is_0) then
         call set_using_SC_cluster_charges_(m%SCF_data,TRUE)
         call set_cluster_radius_(m%SCF_data,radius)
         call set_defragment_(m%SCF_data,defragment)
         call update_(m%SCF_data)
         call scf_(m)  ! Cluster charge SCF here
      end if
   end if

   ! HAR !!!!!!!!!!!!!!!!!!!!!!
   if (do_fragHAR) then
      call fragHAR_refinement_(m)
   else
      call HAR_refinement_(m)
   end if
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! Job timing
   call flush_(stdout)
   mess = "job "//'"'//trim(m%name)//'"'
   ctime = elapsed_time_message_(std_time,mess)
   etime = elapsed_cpu_time_message_(std_time,mess)
   call text_(stdout,etime)
   call text_(stdout,ctime)

   ! Clean-up
   call cleanup_

   ! Clean-up files
   call destroy_(std_err)
   call destroy_(stdout)
   call destroy_(stdin)

   ! Clean-up tonto system
   TONTO_DESTROY

end program
