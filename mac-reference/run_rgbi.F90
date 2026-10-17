! ================================================================
!
! Stand alone RGBI program from gaussian FChk file
!
! Copyright (C) Dylan Jayatilaka, 2019
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
!
! ================================================================

! The version, commit and build date (GIT_VERSION ...), remade on every build
#  include "build_stamp"
program run_RGBI

#  include "macros"

   include "run_rgbi.use"

   implicit none

   type(COMMAND_LINE_TYPE) :: command_line
   STR(len=STR_SIZE) :: file,tail
   STR(len=STR_SIZE) :: option
   STR(len=PATH_SIZE) :: value  ! an option value is often a path
   BIN :: ci_labels
   VEC(STR(len=STR_SIZE),:), allocatable :: grpstr
   VEC(type(EVEC_INT_TYPE),:), allocatable :: groups
   INT :: i
   type(MOLECULE_TYPE), allocatable :: m

   ! Macro to create Tonto system object
   ! Initialise MPI parallel stuff too.
   TONTO_CREATE

   ! Set stdout & std_err to screen
   call create_stdout
   call create_std_err

   ! Default option values
   ci_labels = FALSE
 ! use_NAOs  = FALSE

   ! Get command line !!!!!!!!!
   call set_defaults_(command_line)
   call process_options_(command_line)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! Sanity check
   if (NOT has_option_(command_line,"help")) then
      if (NOT has_arguments_(command_line) OR command_line%n_args>1) then

      ! No arguments, or too many: a usage error, so exit non-zero
         DIE("run_RGBI:run_RGBI ... Supply one <.FChk> or <.molden> file. Type 'rgbi --help' for help.")
      else

      ! First argument
         file = command_line%arg(1)
         tail = filename_tail_(file)
         call to_lower_case_(tail)
         DIE_IF(NOT is_one_of_(tail,["fchk  ","molden"]),"run_RGBI:run_RGBI ... file must end in .fchk or .molden")
      end if
   end if  ! -- NOT help

   ! Analyze command line options
   do i = 1,command_line%n_options

      ! Get options
      option = command_line%option(i)
      value = command_line%option_value(i)

      ! Analyze options
      select case (option)

      case ("groups")
         call create_(grpstr,1)
         grpstr(1) = value
         call create_stdin
         call redirect_(stdin,grpstr)
         call read_all_(stdin,groups)
         call revert_(stdin)
         call destroy_(stdin)
         call destroy_(grpstr)

      case ("ci-labels")
         ci_labels = TRUE

    ! case ("nao")
    !    use_NAOs = value.to_bin

      case ("help")
         call text_(stdout,"SYNOPSIS")
         call flush_(stdout)
         call text_(stdout,"    rgbi [ options ] <file.FChk> | <file.molden>")
         call flush_(stdout)
         call text_(stdout,"    Obtaining Roby-Gould bond indices from a gaussian")
         call text_(stdout,"    <.FChk> file or from a <.molden> file.")
         call flush_(stdout)
         call text_(stdout,"    With more free software such as LaTeX, chemfig, and Indigo,")
         call text_(stdout,"    labelled structures and 'dial diagrams' may be produced with")
         call text_(stdout,"    the make-rgbi-pic and make-rgbi-dials scripts installed beside")
         call text_(stdout,"    this program (from the rgbi-scripts folder of the source).")
         call flush_(stdout)
         call text_(stdout,"    If you use this work please cite the references below so")
         call text_(stdout,"    that people know what you have done.")
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"EXPLANATION")
         call flush_(stdout)
         call text_(stdout,"    The Roby-Gould method uses projection operator techniques to produce")
         call text_(stdout,"    chemically sensible bond indices from a wavefunction.")
         call flush_(stdout)
         call text_(stdout,"    The Roby-Gould method has several advantages over other bond index")
         call text_(stdout,"    methods, including, in no particular order:")
         call flush_(stdout)
         call text_(stdout,"    . The method converges with basis set size")
         call flush_(stdout)
         call text_(stdout,"    . An ionic bond index is defined alongside (but separate to) a")
         call text_(stdout,"      covalent bond index, so that the concept of bonding comprises two")
         call text_(stdout,"      components and indeed, is treated as a two-dimensional vector.")
         call text_(stdout,"      It is the only method which treats covalency and ionicity on an")
         call text_(stdout,"      equal theoretical footing.")
         call flush_(stdout)
         call text_(stdout,"    . The bond indices are calculated as a sum over individual orbital")
         call text_(stdout,"      contributions, each contribution being equal to half the number of")
         call text_(stdout,"      electrons in bonding orbitals minus half the number of electrons")
         call text_(stdout,"      in antibonding orbitals. The individual vector contributions")
         call text_(stdout,"      may conveniently be viewed using a 'dial diagram' graphic.")
         call flush_(stdout)
         call text_(stdout,"    . The expressions for the indices may also be obtained as expectation")
         call text_(stdout,"      values of Hermitian operators, like any other QM observable.")
         call flush_(stdout)
         call text_(stdout,"    . The bond indices may be calculated between groups of atoms")
         call flush_(stdout)
         call text_(stdout,"    . The formalism is geometric in nature, depending on projection")
         call text_(stdout,"      operators, and it is not related to any Hamiltonian.")
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"REFERENCES")
         call flush_(stdout)
         call text_(stdout,"   . M.D. Gould et al (2008) Theor. Chem. Acc. 199 p119-270")
         call text_(stdout,"   . K. Alhameedi et al (2018) Int. J. Quantum Chem. e25603")
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"OPTIONS")
         call flush_(stdout)
         call text_(stdout,"   --groups '{ { <group-A-atom-indices> } { <group-B-atom-indices } }'")
         call flush_(stdout)
         call text_(stdout,"       Specify the indices of the atoms in two groups A and B between which")
         call text_(stdout,"       the Roby-Gould indices are calculated.")
         call flush_(stdout)
         call text_(stdout,"       Default: Do an RGBI analysis between all individual atoms which are")
         call text_(stdout,"       close enough to be regarded as bonded according to the Cambridge")
         call text_(stdout,"       structural database criteria.")
         call flush_(stdout)
         call text_(stdout,"       Example: --groups '{ { 1 4 6 } { 2 3 5 } }'")
         call flush_(stdout)
         call text_(stdout,"       An RG bond analysis is done between atoms with indices 1,4,6")
         call text_(stdout,"       (in group A) and between atoms 2,3,5 (in group B)")
         call flush_(stdout)
         call text_(stdout,"   --ci-labels")
         call flush_(stdout)
         call text_(stdout,"       Specify to print i/c bond indices above/below the bond line when")
         call text_(stdout,"       using the latex-based make-rgbi-pic script")
         call flush_(stdout)
       ! stdout.text("   --nao [t|f]")
       ! stdout.flush    
       ! stdout.text("       Specify to use Reed et al's pre natural atomic orbitals to define")
       ! stdout.text("       the atoms and atomic projectors via true (t) or false (f)")
       ! stdout.flush
       ! stdout.text("       Default: to use the occupied atomic natural orbitals with occupancy")
       ! stdout.text("       greater than 0.05 electrons i.e. false or 'f'")
         call flush_(stdout)
         call text_(stdout,"   --help")
         call flush_(stdout)
         call text_(stdout,"       Print this documentation")
         call flush_(stdout)
         stop

      case default
         call text_(stdout,"Type 'rgbi --help' for help.")
         call flush_(stdout)
         allocate(tonto%known_keywords(3))
         tonto%known_keywords(1) = "groups"
         tonto%known_keywords(2) = "ci-labels"
         tonto%known_keywords(3) = "help"
         call unknown_(tonto,option,"run_RGBI:run_RGBI")
         deallocate(tonto%known_keywords)
         stop
      end select
   end do

   ! Create molecule
   call setup_(m)
   m%name = filename_head_(file)

   ! Process input file
   select case (tail)
   case ("fchk  "); call read_g09_FChk_file_(m,file)
   case ("molden"); call read_molden_file_(m,file)
   end select

   ! Initialize output file
   call destroy_(stdout)
   call create_(stdout)
   call set_name_(stdout,"stdout")
   call open_for_(stdout,"write")
   call set_stdout_unit_(tonto,stdout%unit)

   ! Safe to report to 'stdout'
   call flush_(stdout)
   call text_(stdout,"=============================")
   call text_(stdout,"Roby-Gould bond index program")
   call text_(stdout,"=============================")
   call flush_(stdout)
   call text_(stdout,"This is a command line program for obtaining Roby-Gould")
   call text_(stdout,"bond indices from a gaussian FChk file")
   call flush_(stdout)
   call text_(stdout,"For detailed help, run the command:")
   call flush_(stdout)
   call text_(stdout,"   rgbi --help")
   call flush_(stdout)
   call text_(stdout,"Basic usage:")
   call flush_(stdout)
   call text_(stdout,"rgbi <FChk-file>")
   call flush_(stdout)
   call text_(stdout,"If you publish any of the results from this program please")
   call text_(stdout,"cite the following original scientific publications:")
   call flush_(stdout)
   call text_(stdout,"   M.D. Gould et al (2008) Theor. Chem. Acc. 199 p119-270")
   call text_(stdout,"   K. Alhameedi et al (2018) Int. J. Quantum Chem. e25603")
   call text_(stdout,"   M. Woinska et al (2016) Sci. Adv. 2 e1600192")
   call flush_(stdout)
   call flush_(stdout)
   call text_(stdout,"===============")
   call text_(stdout,"Program options")
   call text_(stdout,"===============")
   call flush_(stdout)
   call text_(stdout,"You have run rgbi with the following options:")
   call put_command_optarg_(command_line)
   call flush_(stdout)
   call text_(stdout,"The rgbi program version is:")
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
   call flush_(stdout)
   call flush_(stdout)
   call text_(stdout,"============")
   call text_(stdout,"Contributors")
   call text_(stdout,"============")
   call flush_(stdout)
   call text_(stdout,"Tonto & the hart program has been brought to you with the work")
   call text_(stdout,"of the following groups and companies:")
   call flush_(stdout)
   call text_(stdout,". The University of Western Australia (Jayatilaka)")
   call text_(stdout,". The University of Bremen (Grabowsky)")
   call flush_(stdout)
   call text_(stdout,"Important relevant individual contributions are listed below")
   call text_(stdout,"in roughly chronological order:")
   call flush_(stdout)
   call text_(stdout,". D. Jayatilaka     - System design & conception.  Help.")
   call text_(stdout,"                      Hirshfeld atom refinement (HAR)")
   call text_(stdout,"                      X-ray constrained wavefunctions (XCW)")
   call flush_(stdout)
   call text_(stdout,". S. Wolff          - DFT, integration grids, isosurfaces")
   call text_(stdout,"                      Roby bond indices")
   call flush_(stdout)
   call text_(stdout,". C. Taylor         - Roby bond indices")
   call flush_(stdout)
   call text_(stdout,". M. Susli          - gnuplot picture output")
   call flush_(stdout)
   call text_(stdout,". S. Grabowsky      - X-ray wavefunction refinement (XWR)")
   call text_(stdout,"                      ELI-D and bonding tools")
   call flush_(stdout)
   call text_(stdout,". K. Abdalhussein   - Bond indices, bond energies,")
   call text_(stdout,"                      finite temperature SCF")
   call flush_(stdout)
   call text_(stdout,"Did I forget anyone? Sorry & let me know.")
   call flush_(stdout)
   call flush_(stdout)
   call text_(stdout,"Tonto library methods will now be called ...")
   call flush_(stdout)
   call flush_(stdout)

   ! Print out
   call put_banner_
   call put_(m)

   ! Make integrals
   call make_overlap_mx_(m)
   call make_core_mx_(m)

   ! Set up kind of Roby analysis
   call create_(m%roby,m)
   call set_roby_kind_(m%roby,"atom_bond_analysis")
 ! m.roby.set_use_NAOs(use_NAOs)
   call set_output_ci_latex_labels_(m%roby,ci_labels)
   if (allocated(groups)) then
      call set_roby_kind_(m%roby,"group_bond_analysis")
      m%roby%atom_group = groups
   end if
   call roby_analysis_(m)

   ! Clean-up tonto system
   TONTO_DESTROY

end program
