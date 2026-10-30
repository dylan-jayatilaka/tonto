!==================================================================
!
! TD_DATA
!
! For doing time-dependent SCF calculations. Either DFT or HF.
!
! Copyright (C) Dylan Jayatilaka (2024)
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
!==================================================================
module TD_DATA_MODULE

#  include "macros"

   include "td_data.use"

   implicit none

   include "td_data.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(TD_DATA_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"TD_DATA:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(TD_DATA_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"TD_DATA:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(TD_DATA_TYPE), INOUT :: self
      call destroy_(self%MOo)
      call destroy_(self%MOv)
      call destroy_(self%Eo)
      call destroy_(self%Ev)
      call destroy_(self%single)
      call destroy_(self%deltaE)
      call destroy_(self%double)
      call destroy_(self%delEab)
      call destroy_(self%eval)
      call destroy_(self%evec)
   end subroutine


!  ===============================================
!  Set defaults, reset, update and realted methods
!  ===============================================

   PURE subroutine update(self)
   ! Set default SCF data values
      type(TD_DATA_TYPE), INOUT :: self

      ! Sanity conditions
      if (self%dressing=="mazur") then
         ENSURE(self%method=="u-cis-ss","TD_DATA:update ... mazur dressing needs method='u-cis-ss'")
         ENSURE(self%use_M_0,"TD_DATA:update ... mazur dressing needs M=0 via 'use_M_0 = T'")
      end if

      ! Find S window
      call find_singles_(self)

   ! .find_doubles
   end subroutine

   PURE subroutine set_defaults_0(self,n_bf,n_a,n_b)
   ! Set default SCF data values
      type(TD_DATA_TYPE), INOUT :: self
      INT, IN :: n_bf
      INT, IN :: n_a
      INT, IN :: n_b

      ! Leave existing options, if any
      self%n_bf = n_bf
      self%n_a = n_a
      self%n_b = n_b

      ! Restricted case only
      self%n_occ = n_a
      self%n_vir = n_bf-n_a
   end subroutine

   PURE subroutine set_defaults_1(self,MOs,MO_energies,IVO_energies)
   ! Set default SCF data values
      type(TD_DATA_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: MOs
      VEC(REAL,:), IN :: MO_energies
      VEC(REAL,:), allocatable, optional, IN :: IVO_energies
      INT :: no
   ENSURE(self%n_occ>0,"TD_DATA:set_defaults_1 ... n_occ is not set, call set_defaults!")

      ! Get OMOs and VMOs
      no = self%n_occ
      self%MOo = MOs(:,1:no)
      self%MOv = MOs(:,no+1:self%n_bf)

      ! Note: canonised energies from Fvv are used if
      ! VMO_canonisation was requested in scf_data block.
      self%Eo = MO_energies(1:no)
      self%Ev = MO_energies(no+1:self%n_bf)
      self%Ei = self%Ev

      ! Copy if there
      if (present(IVO_energies)) then
         if (allocated(IVO_energies)) then
            self%Ei = IVO_energies
         end if
      end if
   end subroutine

   PURE subroutine set_archive_results(self,val)
   ! Set whether to archive vectors and eigenvalues to disk
      type(TD_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%archive_results = val
   end subroutine

   PURE subroutine set_dressing(self,val)
   ! Set the dressing method to calculate doubles correction 
   ! to the CIS method.
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(is_one_of_(val,["mazur ","window"]),"TD_DATA:set_dressing ... unknown method")
      self%dressing = lower_cased_(val)
   end subroutine

   PURE subroutine set_implementation(self,val)
   ! Set the type of implementation for the method.
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(is_one_of_(val,["in-core ","davidson"]),"TD_DATA:set_implementation ... unknown method")
      self%implementation = lower_cased_(val)
   end subroutine

   subroutine set_method(self,val)
   ! Set the kind of TD method to use.
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%method = lower_cased_(val)
      select case (self%method)

      ! Davidson
      case ("r-cis-ss-s=0   ")
      case ("r-cis-ss-s=0&1 ")
      case ("r-cis-ss-s=1    ")
   ! case("r-cis-ss-s=0-d1") ! Testing
   ! case("r-cis-ss-s=0-d2") ! Testing
   ! case("r-cis-ss-s=0-v0") ! Testing
      case ("u-cis-ss       ")

      ! In-core
      case ("r-cis-s=0      ")
      case ("r-cis-s=0&1    ")
      case ("r-cis-s=1      ")
   ! case("r-cis-ss-s=0   ") ! See above
      case ("r-tdhf-s=0     ")
      case ("r-tdhf-s=0&1   ")
      case ("r-tdhf-s=1     ")
      case ("u-cis          "); self%tol_S = ZERO
      case ("u-cis-v1       ")

      case default; allocate(tonto%known_keywords(12))
      tonto%known_keywords(1) = "r-cis-ss-s=0   "
      tonto%known_keywords(2) = "r-cis-ss-s=0&1 "
      tonto%known_keywords(3) = "r-cis-ss-s=1    "
      tonto%known_keywords(4) = "u-cis-ss       "
      tonto%known_keywords(5) = "r-cis-s=0      "
      tonto%known_keywords(6) = "r-cis-s=0&1    "
      tonto%known_keywords(7) = "r-cis-s=1      "
      tonto%known_keywords(8) = "r-tdhf-s=0     "
      tonto%known_keywords(9) = "r-tdhf-s=0&1   "
      tonto%known_keywords(10) = "r-tdhf-s=1     "
      tonto%known_keywords(11) = "u-cis          "
      tonto%known_keywords(12) = "u-cis-v1       "
      call unknown_(tonto,self%method,"TD_DATA:set_method")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine set_n_evals(self,val)
   ! Set number of excited-state eigenvalues to search for.
      type(TD_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>0,"TD_DATA:set_n_evals ... n_evals must be +ve")
      if (is_unrestricted_(self)) then
         ENSURE(mod(val,4)==0,"TD_DATA:set_n_evals ... n_evals must be multiple of 4")
      end if
      self%n_evals = val

      ! Set the same number to show
      call set_n_max_show_(self,val)
   end subroutine

   PURE subroutine set_n_keep(self,val)
   ! Set the maximum number of eigensolutions to keep.
      type(TD_DATA_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>0,"TD_DATA:set_n_keep ... n_keep must be +ve")
   ENSURE(val>2,"TD_DATA:set_n_keep ... n_keep must be at least 3")
   ENSURE(val>2,"TD_DATA:set_n_keep ... n_keep must be more than n_evals")
      self%n_keep = val
   end subroutine

   PURE subroutine set_n_max_show(self,val)
   ! Set number of states (i.e. selected singles, doubles, eigenvalues)
   ! to display in the output. A zero value means show all.
      type(TD_DATA_TYPE), INOUT :: self
      INT, IN :: val
      self%n_max_show = val

      ! No point calculating more
   !  if (val<.n_evals) .set_n_evals(val)
   end subroutine

   PURE subroutine set_omega(self,val)
   ! Set energy .omega from which interacting double
   ! excitation determinants are to be selected.
      type(TD_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"TD_DATA:set_omega ... omega must be +ve")
      self%omega = val
   end subroutine

   PURE subroutine set_show_iterations(self,val)
   ! Set whether to show the Davidson iteration residuals in
   ! a tabular format. Good for test valoidation.
      type(TD_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%show_iterations = val
   end subroutine

   PURE subroutine set_show_evecs(self,val)
   ! Set whether to show the eigenvalue column output.
      type(TD_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%show_evecs = val
   end subroutine

   PURE subroutine set_tol_D(self,val)
   ! Set double-excitation determinant energy selection tolerance
      type(TD_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"TD_DATA:set_tol_D ... tol_D must be +ve")
      self%tol_D = val
   end subroutine

   PURE subroutine set_tol_Davidson(self,val)
                                !
   ! Set Davidson iteration convergence tolerance on eigenvectors
      type(TD_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>ZERO,"TD_DATA:set_tol_Davidson ... tol_Davidson must be +ve")
      self%tol_Davidson = val
   end subroutine

   PURE subroutine set_tol_S(self,val)
   ! Set the single-excitation tolerance window.
      type(TD_DATA_TYPE), INOUT :: self
      REAL, IN :: val
   ENSURE(val>=ZERO,"TD_DATA:set_tol_S ... tol_S must be non -ve")
      self%tol_S = val
   end subroutine

   PURE subroutine set_use_IVO_energies(self,val)
   ! Set whether to use improved virtual energies (IVOs) from
   ! a previous SCF canonicalisation to define determinants
   ! which comprise the S window.
      type(TD_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_IVO_energies = val
   end subroutine

   PURE subroutine set_use_M_0(self,val)
   ! Set whether to use M=0 basis only.
      type(TD_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_M_0 = val
   end subroutine

   PURE subroutine set_use_MGS(self,val)
   ! Set whether to use Modified Gram-Schmidt QR orthonormalisation.
   ! If not set, Davidson uses the Householde QR algorithm.
      type(TD_DATA_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_MGS = val
   end subroutine


!   set_use_S_window_MOs(val) :: pure
!   ! Set whether to use only MOs in the the single excitation
!   ! list of determinants to construct interacting doubles
!   ! excitations (on top of being within the tol_D energy window).
!      self :: INOUT
!      val  :: BIN, IN
!      .use_S_window_MOs = val
!   end

!  =============
!  Input methods
!  =============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","TD_DATA:read_keywords ... expecting open bracket symbol, {")

      ! Read the open braces {

      ! The following code is inherited from OBJECT
      call read_(stdin,word)

      ! Echo keyword if requested
      if (tonto%keyword_echo) then
         call flush_(stdout)
         call text_(stdout,"keyword found --> "//trim(word))
      end if
      do  ! Loop over input keywords

         ! Read the keyword
         call read_(stdin,word)
         DIE_IF(reverted_(stdin),"TD_DATA:read_keywords ... unexpected end of file while reading list keywords")
         call to_lower_case_(word)

         ! Append any lonely = sign
         if (NOT exhausted_(stdin%buffer)) then
            if (next_item_(stdin)=="=") then
               call skip_next_item_(stdin)
               word = trim(word)//"="
            end if
         end if

         ! Echo keyword if requested
         if (tonto%keyword_echo) then
            call flush_(stdout)
            call text_(stdout,"keyword found --> "//trim(word))
         end if

         ! Exit if keyword is closing brace }
         if (word=="}") exit

         ! Process the keyword 
         call process_keyword_(self,word)
      end do
   end subroutine

   subroutine process_keyword(self,keyword)
   ! Process a command "keyword". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      BIN :: die
      word = keyword
      call to_lower_case_(word)
      select case (word)

      case ("archive_results=   "); call read_archive_results_(self)
      case ("dressing=          "); call read_dressing_(self)
      case ("find_doubles       "); call find_doubles_(self)
      case ("find_singles       "); call find_singles_(self)
      case ("implementation=    "); call read_implementation_(self)
      case ("max_keep=          "); call read_n_keep_(self)
      case ("method=            "); call read_method_(self)
      case ("n_eig=             "); call read_n_evals_(self)
      case ("n_evals=           "); call read_n_evals_(self)
      case ("n_keep=            "); call read_n_keep_(self)
      case ("n_max_show=        "); call read_n_max_show_(self)
      case ("n_states=          "); call read_n_evals_(self)
      case ("omega=             "); call read_omega_(self)
      case ("put_mo_energies    "); call put_MO_energies_(self)
      case ("put_mos            "); call put_MOs_(self)
      case ("put_options        "); call put_options_(self)
      case ("put_doubles        "); call put_doubles_(self)
      case ("put_evecs          "); call put_evecs_(self)
      case ("put_singles        "); call put_singles_(self)
      case ("show_iterations=   "); call read_show_iterations_(self)
      case ("show_evecs=        "); call read_show_evecs_(self)
      case ("tol_d=             "); call read_tol_D_(self)
      case ("tol_davidson=      "); call read_tol_Davidson_(self)
      case ("tol_s=             "); call read_tol_S_(self)
      case ("use_ivo_energies=  "); call read_use_IVO_energies_(self)
      case ("use_m_0=           "); call read_use_M_0_(self)
   ! case ("use_s_window_mos=  "); .read_use_S_window_MOs

      ! Documentation
      case ("help?              "); call docu_help_
   ! case ("find_doubles?      "); TD_DATA:docu_find_doubles(die)
      case ("find_singles?      "); call docu_find_singles_(die)
      case ("max_keep=?         "); call docu_n_keep_(die)
      case ("method=?           "); call docu_method_(die)
      case ("n_evals=?          "); call docu_n_evals_(die)
      case ("n_keep=?           "); call docu_n_keep_(die)
      case ("n_max_show=?       "); call docu_n_max_show_(die)
      case ("omega=?            "); call docu_omega_(die)
      case ("put_doubles?       "); call docu_put_doubles_(die)
      case ("put_options?       "); call docu_put_options_(die)
      case ("put_singles?       "); call docu_put_singles_(die)
      case ("tol_d=?            "); call docu_tol_D_(die)
      case ("tol_davidson=?     "); call docu_tol_Davidson_(die)
      case ("tol_s=?            "); call docu_tol_S_(die)
   ! case ("use_s_window_mos=? "); TD_DATA:docu_use_S_window_MOs(die)

      case default; allocate(tonto%known_keywords(40))
      tonto%known_keywords(1) = "archive_results=   "
      tonto%known_keywords(2) = "dressing=          "
      tonto%known_keywords(3) = "find_doubles       "
      tonto%known_keywords(4) = "find_singles       "
      tonto%known_keywords(5) = "implementation=    "
      tonto%known_keywords(6) = "max_keep=          "
      tonto%known_keywords(7) = "method=            "
      tonto%known_keywords(8) = "n_eig=             "
      tonto%known_keywords(9) = "n_evals=           "
      tonto%known_keywords(10) = "n_keep=            "
      tonto%known_keywords(11) = "n_max_show=        "
      tonto%known_keywords(12) = "n_states=          "
      tonto%known_keywords(13) = "omega=             "
      tonto%known_keywords(14) = "put_mo_energies    "
      tonto%known_keywords(15) = "put_mos            "
      tonto%known_keywords(16) = "put_options        "
      tonto%known_keywords(17) = "put_doubles        "
      tonto%known_keywords(18) = "put_evecs          "
      tonto%known_keywords(19) = "put_singles        "
      tonto%known_keywords(20) = "show_iterations=   "
      tonto%known_keywords(21) = "show_evecs=        "
      tonto%known_keywords(22) = "tol_d=             "
      tonto%known_keywords(23) = "tol_davidson=      "
      tonto%known_keywords(24) = "tol_s=             "
      tonto%known_keywords(25) = "use_ivo_energies=  "
      tonto%known_keywords(26) = "use_m_0=           "
      tonto%known_keywords(27) = "help?              "
      tonto%known_keywords(28) = "find_singles?      "
      tonto%known_keywords(29) = "max_keep=?         "
      tonto%known_keywords(30) = "method=?           "
      tonto%known_keywords(31) = "n_evals=?          "
      tonto%known_keywords(32) = "n_keep=?           "
      tonto%known_keywords(33) = "n_max_show=?       "
      tonto%known_keywords(34) = "omega=?            "
      tonto%known_keywords(35) = "put_doubles?       "
      tonto%known_keywords(36) = "put_options?       "
      tonto%known_keywords(37) = "put_singles?       "
      tonto%known_keywords(38) = "tol_d=?            "
      tonto%known_keywords(39) = "tol_davidson=?     "
      tonto%known_keywords(40) = "tol_s=?            "
      call unknown_(tonto,word,"TD_DATA:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine docu_help()
   ! Documentation.
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"================")
      call text_(stdout,"Help for TD_DATA")
      call text_(stdout,"================")

   ! TD_DATA:docu_find_doubles
      call docu_find_singles_
      call docu_n_keep_
      call docu_method_
      call docu_n_evals_
      call docu_omega_
      call docu_put_doubles_
      call docu_put_options_
      call docu_put_singles_
      call docu_tol_D_
      call docu_tol_Davidson_
      DIE("TD_DATA:docu_help ... end of help documentation")
   end subroutine


! A

   subroutine read_archive_results(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_archive_results_(self,val)
   end subroutine


! D

   subroutine read_dressing(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_dressing_(self,val)
   end subroutine


! I

   subroutine read_implementation(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_implementation_(self,val)
   end subroutine


! M

   subroutine docu_n_keep(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"n_keep= INT    [32]")
      call text_(stdout,"max_keep= INT  [32]")
      call flush_(stdout)
      call text_(stdout,". The maximum number of trial eigenvectors to keep when")
      call text_(stdout,"  using the Davidson method to obtain eigenstates of the")
      call text_(stdout,"  response equations.")
      call flush_(stdout)
      call text_(stdout,". See Crouzeix et al  (1994) SIAM J Sci Comput 15_p. 62-76")
      if (present(die)) stop
   end subroutine

   subroutine read_n_keep(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_n_keep_(self,val)
   end subroutine

   subroutine docu_method(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"method= STR  ['rtdhf-singlet']")
      call flush_(stdout)
      call text_(stdout,". The kind of mean-field method used in the response equations")
      call flush_(stdout)
      call text_(stdout,". Allowed values: 'utdhf', 'rtdhf-singlet', 'rtdhf-triplet'")
      if (present(die)) stop
   end subroutine

   subroutine read_method(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_method_(self,val)
   end subroutine


! N

   subroutine docu_n_evals(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"n_eig=")
      call text_(stdout,"n_evals=")
      call text_(stdout,"n_states= INT [4]")
      call flush_(stdout)
      call text_(stdout,". The number of excitated state eigenvalues wanted")
      call text_(stdout,"  when solving the CIS/TD response equations via the")
      call text_(stdout,"  iterative Davidson method")
      call flush_(stdout)
      call text_(stdout,". Excitation energies are sorted from smallest first")
      call flush_(stdout)
      call text_(stdout,". Must be less than the number of single excitation")
      call text_(stdout,"  states used in the model")
      if (present(die)) stop
   end subroutine

   subroutine read_n_evals(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_n_evals_(self,val)
   end subroutine

   subroutine docu_n_max_show(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"n_max_show= INT  [25]")
      call flush_(stdout)
      call text_(stdout,". The number of states i.e. selected singles, doubles, or")
      call text_(stdout,"  excitation energies to display in the output")
      call flush_(stdout)
      call text_(stdout,". A zero value means show all.")
      if (present(die)) stop
   end subroutine

   subroutine read_n_max_show(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_n_max_show_(self,val)
   end subroutine


! S

   subroutine read_show_iterations(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_show_iterations_(self,val)
   end subroutine

   subroutine read_show_evecs(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_show_evecs_(self,val)
   end subroutine


! O

   subroutine docu_omega(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"omega= REAL  [0.000]")
      call flush_(stdout)
      call text_(stdout,". An energy above the ground state of a particular excited")
      call text_(stdout,"  state, used to determine a list of double excitations")
      call text_(stdout,"  (whose energy lies within a certain energy tolerance")
      call text_(stdout,"  tol_D of that state, the doubles energies being calculated")
      call text_(stdout,"  by Koopman's theorem orbital energy differences) and which")
      call text_(stdout,"  are used to 'dress' or 'correct' the mean-field response")
      call text_(stdout,"  equations for the energy of that particular excited state.")
      call flush_(stdout)
      call text_(stdout,". See Maitra et al (2004) JCP 13 p. 5932.")
      call text_(stdout,"  See Mazur et al (2009) J Comp Chem 30 p. 811-7")
      call flush_(stdout)
      call text_(stdout,". See also tol_D=")
      if (present(die)) stop
   end subroutine

   subroutine read_omega(self)
   ! Generic read and set a quantity with units
      type(TD_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_omega_(self,val)
   end subroutine


! T

   subroutine docu_tol_D(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"tol_D= REAL   [0.01]")
      call flush_(stdout)
      call text_(stdout,". The energy tolerance to select interacting double excited")
      call text_(stdout,"  determinants from a given excitation energy, specified by")
      call text_(stdout,"  the omega= option.")
      call flush_(stdout)
      call text_(stdout,". See Maitra et al (2004) JCP 13 p. 5932.")
      call text_(stdout,"  See Mazur et al (2009) J Comp Chem 30 p. 811-7")
      if (present(die)) stop
   end subroutine

   subroutine read_tol_D(self)
   ! Generic read and set a quantity with units
      type(TD_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_tol_D_(self,val)
   end subroutine

   subroutine docu_tol_Davidson(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"tol_Davidson= REAL")
      call flush_(stdout)
      call text_(stdout,". The whole-state convergence criteria used in the iterative")
      call text_(stdout,"  Davidson method used to obtain the excitation energies.")
      call flush_(stdout)
      call text_(stdout,". See also max_keep= and n_evals=")
      if (present(die)) stop
   end subroutine

   subroutine read_tol_Davidson(self)
   ! Generic read and set a quantity with units
      type(TD_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_tol_Davidson_(self,val)
   end subroutine

   subroutine docu_tol_S(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"tol_S= REAL [0.75]")
      call flush_(stdout)
      call text_(stdout,". A tolerance to exclude single determinant excitations")
      call text_(stdout,"  whose excitation energy is too large (from the ground")
      call text_(stdout,"  state determinant (according to Koopman's theorem orbital")
      call text_(stdout,"  energy differences).")
      if (present(die)) stop
   end subroutine

   subroutine read_tol_S(self)
   ! Generic read and set a quantity with units
      type(TD_DATA_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_tol_S_(self,val)
   end subroutine


! U

   subroutine read_use_IVO_energies(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_IVO_energies_(self,val)
   end subroutine

   subroutine read_use_M_0(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_M_0_(self,val)
   end subroutine

   subroutine read_use_MGS(self)
   ! Generic read and set
      type(TD_DATA_TYPE), INOUT :: self
      BIN :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_use_M_0_(self,val)
   end subroutine


!   docu_use_S_window_MOs(die) :: selfless, private
!   ! Documentation.
!      die :: BIN, optional, IN
!      stdout.flush
!      stdout.text("tol_S= REAL [0.75]")
!      stdout.flush
!      stdout.text(". A tolerance to exclude single determinant excitations")
!      stdout.text("  whose excitation energy is too large (from the ground")
!      stdout.text("  state determinant (according to Koopman's theorem orbital")
!      stdout.text("  energy differences).")
!      if (present(die)) stop
!   end

!   read_use_S_window_MOs :: get_from(OBJECT:read_and_set, VAL_TYPE?=>BIN, SET?=>.set_use_S_window_MOs), private
!   ! Generic read and set
!   end

!  ============
!  TD calc info
!  ============

   pure function is_restricted(self) result (res)
   ! Return TRUE if the method is *only* a S=0 state
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = includes_(self%method,"r-")
   end function

   pure function is_unrestricted(self) result (res)
   ! Return TRUE if the method is *only* a S=0 state
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = includes_(self%method,"u-")
   end function

   PURE function is_singlet_calc(self) result (res)
   ! Return TRUE if the method is *only* a S=0 state
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = includes_(self%method,"-s=0") AND NOT includes_(self%method,"-s=0&1")
   end function

   PURE function is_triplet_calc(self) result (res)
   ! Return TRUE if the method is *only* a S=1 state
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = includes_(self%method,"-s=1")
   end function

   PURE function is_s0_and_s1_calc(self) result (res)
   ! Return TRUE if the method *both* an S=0 and S=1 state.
   ! This test is used to append results.
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = includes_(self%method,"-s=0&1")
   end function

   PURE function do_sqrt_evals(self) result (res)
   ! Return TRUE if TD eigenvalues should be square-rooted.
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = includes_(self%method,"-tdhf")
   end function

   PURE function is_an_SS_method(self) result (res)
   ! Return TRUE if the method is a state-slected method.
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = includes_(self%method,"-ss")
   end function

   PURE function is_CIS(self) result (res)
   ! Return TRUE if it is a CIS/Tamm-Dancoff calculation
   ! which does not require an off-diagonal "B" matrix.
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = includes_(self%method,"-cis")
   end function

   PURE function is_Tamm_Dancoff(self) result (res)
   ! Return TRUE if it is a CIS/Tamm-Dancoff calculation
   ! which does not require an off-diagonal "B" matrix.
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = includes_(self%method,"-cis")
   end function

   pure function has_states(self) result (res)
   ! Return TRUE if the states have been calculated.
   ! It is assumed that these are singlet states and
   ! if a triplet calculation is run afterwards, those
   ! states are appended.
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = allocated(self%eval) AND allocated(self%evec)
   end function

   PURE function is_M0_singlet(self,i) result (res)
   ! Return TRUE if vector "i" is an M=0 singlet.
      type(TD_DATA_TYPE), IN :: self
      INT, IN :: i
      BIN :: res
      INT :: d1,d3,n
   ENSURE(self%use_M_0,"TD_DATA:is_M0_singlet ... not an M=0 calculation")
   ENSURE(allocated(self%evec),"TD_DATA:is_M0_singlet ... no state evec's")
   ENSURE(i>0 AND i<=size(self%V,2),"TD_DATA:is_M0_singlet ... state index not in allowed range")
      d1 = size(self%evec,1)/4
      d3 = d1*3
      n = index_of_largest_value_(self%evec(1:d1,i))
      res = same_sign_as_(self%evec(d3+n,i),self%evec(n,i))
   end function


!  ====================
!  State selection info
!  ====================

   pure function no_of_singles(self) result (res)
   ! The number of single excitations which satisfy the tol_S
   ! excitation window.
      type(TD_DATA_TYPE), IN :: self
      INT :: res
      INT :: i,a
      REAL :: val
      if (equals_(self%tol_S,ZERO)) then
         res = self%n_vir*self%n_occ
      else
         res = 0
         do i = self%n_occ,1,-1
            do a = 1,self%n_vir
               val = self%Ev(a)-self%Eo(i)
               if (val<self%tol_S) then
                  res = res+1
               end if
            end do
         end do
      end if
   end function

   PURE function r_O_singles_str(self) result (res)
   ! List of occupied MOs in the singles-excitation window.
   ! Restricted version.
      type(TD_DATA_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: d,i,omo
   ENSURE(allocated(self%single),"TD_DATA:r_O_singles_str ... no single's array")
      d = size(self%single,1)
      call create_(res,d)
      do i = 1,d
         omo = self%single(i,2)-self%n_occ
         res(i) = trim(to_str_(omo))
      end do
   end function

   PURE function r_V_singles_str(self) result (res)
   ! List of occupied MOs in the singles-excitation window.
   ! Restricted version.
      type(TD_DATA_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: d,i,vmo
   ENSURE(allocated(self%single),"TD_DATA:r_V_singles_str ... no single's array")
      d = size(self%single,1)
      call create_(res,d)
      do i = 1,d
         vmo = self%single(i,1)
         res(i) = trim(to_str_(vmo))
      end do
   end function

   PURE function u_O_singles_str(self) result (res)
   ! List of occupied MOs in the singles-excitation window.
   ! Unrestricted version.
      type(TD_DATA_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: d,d4,i,omo
   ENSURE(allocated(self%single),"TD_DATA:u_O_singles_str ... no single's array")
      d = size(self%single,1)
      d4 = 4*d
      call create_(res,d4)
      res = " "
      do i = 1,d4,4
         omo = self%single((i+3)/4,2)-self%n_occ
         res(i) = trim(to_str_(omo))
      end do
   end function

   PURE function u_V_singles_str(self) result (res)
   ! List of occupied MOs in the singles-excitation window.
   ! Unrestricted version.
      type(TD_DATA_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: d,d4,i,vmo
   ENSURE(allocated(self%single),"TD_DATA:u_V_singles_str ... no single's array")
      d = size(self%single,1)
      call create_(res,d4)
      res = " "
      do i = 1,d4,4
         vmo = self%single((i+3)/4,1)
         res(i) = trim(to_str_(vmo))
      end do
   end function


!  =======================
!  State selection methods
!  =======================

   subroutine docu_find_singles(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"find_singles")
      call flush_(stdout)
      call text_(stdout,". Find and sort the single-excitations into increasing")
      call text_(stdout,"  Koopman's energy order.")
      if (present(die)) stop
   end subroutine

   PURE subroutine find_singles(self)
   ! Sorts single excitations by smallest eigenvalue energy
   ! difference, and keep pairs in the single(:,1:2) array.
      type(TD_DATA_TYPE), INOUT :: self
      INT :: i,a,n
      REAL :: val
      VEC(INT,:), allocatable :: ind

      ! Below cannot be undone.
      if (self%use_IVO_energies) then
         ENSURE(allocated(self%Ei),"TD_DATA:find_singles ... no IVO energies allocated")
         self%Ev = self%Ei
      end if

      ! Can proceed?
      n = no_of_singles_(self)
      if (n==0) return

      ! Leaky !!!!!!!!!!!
      call create_(self%deltaE,n)
      call create_(self%single,n,2)
      !!!!!!!!!!!!!!!!!!!

      ! Get delta E and singles indices.
      ! Loop over occupieds from HOMO down and virtuals 
      ! from LUMO up so the energy difference is roughly 
      ! in increasing Koopmans energy order, but in the 
      ! end the energies are sorted.
      if (is_zero_(self%tol_S)) then

         ! No window - get all
         n = 0
         do i = self%n_occ,1,-1
            do a = 1,self%n_vir
               val = self%Ev(a)-self%Eo(i)
               n = n+1
               self%deltaE(n) = val
               self%single(n,1) = a
               self%single(n,2) = i
            end do
         end do
      else

         ! Use window
         n = 0
         do i = self%n_occ,1,-1
            do a = 1,self%n_vir
               val = self%Ev(a)-self%Eo(i)
               if (val<self%tol_S) then
                  n = n+1
                  self%deltaE(n) = val
                  self%single(n,1) = a
                  self%single(n,2) = i
               end if
            end do
         end do
      end if

      ! Sort into increasing E difference
      call create_(ind,n)
      call quick_sort_(self%deltaE,ind)

      ! Reorder
      self%deltaE = self%deltaE(ind)
      self%single(:,1) = self%single(ind,1)
      self%single(:,2) = self%single(ind,2)

      ! Set min & max OMO and VMO
      self%min_OMO = minval(self%single(:,2))
      self%max_VMO = maxval(self%single(:,1))
   end subroutine

   subroutine docu_put_singles(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"put_singles")
      call flush_(stdout)
      call text_(stdout,". Display table of the sorted singles states")
      if (present(die)) stop
   end subroutine

   subroutine put_singles(self)
   ! Print out the sorted singles states.
      type(TD_DATA_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: arrow
      INT :: n,m
      n = no_of_singles_(self)
      m = self%n_max_show
      call flush_(stdout)
      call text_(stdout,"=================")
      call text_(stdout,"CI singles window")
      call text_(stdout,"=================")
      call flush_(stdout)
      call text_(stdout,". Single excitations S are selected in an energy window")
      call text_(stdout,"  so that:")
      call flush_(stdout)
      call text_(stdout,"     |E_S| < tol_S,")
      call flush_(stdout)
      call text_(stdout,"  where E_S is the singles excitation energy calculated")
      call text_(stdout,"  by Koopman's theorem i.e. orbital energy differences,")
      call flush_(stdout)
      call text_(stdout,". Single excitation S is specified by:")
      call text_(stdout,"  - I -> A where I < 1 are occupied MOs; 0 is the HOMO")
      call text_(stdout,"  -        where A > 0 are virtual  MOs; 1 is the LUMO")
      call flush_(stdout)
      call text_(stdout,". Improved virtual MO energies may be used, if requested,")
      call text_(stdout,"  and if VMO canonicalisation was used in the SCF.")
      call flush_(stdout)
      call text_(stdout,"Singles window")
      call show_(stdout,"  tol_S             =",self%tol_S)
      call show_(stdout,"  tol_S / eV        =",self%tol_S*EV_PER_HARTREE)
      call show_(stdout,"  No. of singles    =",n)
      call flush_(stdout)
      call text_(stdout,"Orbital range")
      call show_(stdout,"  Min. occupied MO  =",self%min_OMO-self%n_occ+1)
      call show_(stdout,"  Max. virtual  MO  =",self%max_VMO)
      call flush_(stdout)
      call show_(stdout,"Use IVO energies?   =",self%use_IVO_energies)
      call flush_(stdout)
      call show_(stdout,"No. of to display   =",m)
      call flush_(stdout)
      DIE_IF(n==0,"TD_DATA:put_singles ... no single excitations in the window")
      if (m>0) n = min(n,m)
      call create_(arrow,n)
      arrow = "->"
      call create_(table,5)
      call set_heading_(table(1),"I")
      call set_values_(table(1),self%single(1:n,2)-self%n_occ)
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"->")
      call set_width_(table(2),3)
      call set_values_(table(2),arrow)
      call set_heading_(table(3),"A")
      call set_values_(table(3),self%single(1:n,1))
      call set_heading_(table(4),"Ev-Eo")
      call set_subhead_(table(4)," /au")
      call set_real_precision_(table(4),3)
      call set_width_(table(4),8)
      call set_values_(table(4),self%deltaE(1:n))
      call set_heading_(table(5),"Ev-Eo")
      call set_subhead_(table(5)," /eV")
      call set_real_precision_(table(5),3)
      call set_width_(table(5),8)
      call set_values_(table(5),self%deltaE(1:n)*EV_PER_HARTREE)
      call put_(table,label_rows=TRUE)
      call destroy_(table)
      call destroy_(arrow)
   end subroutine


!  ================
!  Doubles dressing
!  ================

   function no_of_doubles(self) result (res)
   ! The number of double excitations within energy .tol_D
   ! of excitation energy .omega.
      type(TD_DATA_TYPE), IN :: self
      INT :: res
   ENSURE(allocated(self%single),"TD_DATA:no_of_doubles ... no single's array")

      ! Exit?
      res = no_of_singles_(self)
      if (res==0) return
      select case (self%dressing)
      case ("mazur "); res = no_of_doubles_Mazur_(self)
      case ("window"); res = no_of_doubles_window_(self)
      case default; allocate(tonto%known_keywords(2))
      tonto%known_keywords(1) = "mazur "
      tonto%known_keywords(2) = "window"
      call unknown_(tonto,self%dressing,"TD_DATA:no_of_doubles")
      deallocate(tonto%known_keywords)
      end select
   end function

   PURE function no_of_doubles_window(self) result (res)
   ! The number of double excitations within energy .tol_D
   ! of excitation energy .omega.
      type(TD_DATA_TYPE), IN :: self
      INT :: res
      INT :: n,d
      INT :: i,a,ai
      INT :: j,b,bj
      REAL :: Eai,Ebj,val
   ENSURE(allocated(self%single),"TD_DATA:no_of_doubles_window ... no single's array")
   ENSURE(self%omega>ZERO,"TD_DATA:no_of_doubles_window ... window frequency omega is zero!")
   ENSURE(self%tol_D>ZERO,"TD_DATA:no_of_doubles_window ... window tolerance tol_D is zero!")

      ! Exit?
      res = no_of_singles_(self)
      if (res==0) return

      ! List of allowed occs and virs
      d = size(self%single,1)
      n = 0
      do ai = 1,d
         a = self%single(ai,1)
         i = self%single(ai,2)
         Eai = self%deltaE(ai)
         do bj = 1,ai
            b = self%single(bj,1)
            j = self%single(bj,2)
            Ebj = self%deltaE(bj)
            val = Eai+Ebj
            if (abs(val-self%omega)<self%tol_D AND val<self%tol_S) then
               n = n+1
            end if
         end do
      end do
      res = n
   end function

   PURE function no_of_doubles_Mazur(self) result (res)
   ! The number double excitations in single double excitation 
   ! determinant method. One per eigenvalue.
      type(TD_DATA_TYPE), IN :: self
      INT :: res
   ENSURE(allocated(self%single),"TD_DATA:no_of_doubles_Mazur ... no single's array")
   ENSURE(allocated(self%eval),"TD_DATA:no_of_doubles_Mazur ... no eigenvalues eval")

      ! Exit?
      res = no_of_singles_(self)
      if (res==0) return
      res = size(self%eval)
   end function

   subroutine find_doubles(self)
   ! Find double excitations in S window, within .tol_D
   ! of the chosen excitation energy .omega. Also find
   ! all singles parents for each double.
      type(TD_DATA_TYPE), INOUT :: self
   ENSURE(allocated(self%single),"TD_DATA:find_doubles ... no single's array")
   ENSURE(allocated(self%deltaE),"TD_DATA:find_doubles ... no deltaE array")
      select case (self%dressing)
      case ("mazur "); call find_doubles_Mazur_(self)
      case ("window"); call find_doubles_window_(self)
      case default; allocate(tonto%known_keywords(2))
      tonto%known_keywords(1) = "mazur "
      tonto%known_keywords(2) = "window"
      call unknown_(tonto,self%dressing,"TD_DATA:find_doubles")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   PURE subroutine find_doubles_window(self)
   ! Find double excitations in S window, within .tol_D
   ! of the chosen excitation energy .omega. Also find
   ! all singles parents for each double.
      type(TD_DATA_TYPE), INOUT :: self
      INT :: d,n
      INT :: ai,a,i
      INT :: bj,b,j
      REAL :: Eai,Ebj,val
   ENSURE(allocated(self%single),"TD_DATA:find_doubles_window ... no single's array")
   ENSURE(allocated(self%deltaE),"TD_DATA:find_doubles_window ... no deltaE array")
   ENSURE(self%omega>ZERO,"TD_DATA:find_doubles_window ... target excitation energy omega is zero!")
   ENSURE(self%tol_D>ZERO,"TD_DATA:find_doubles_window ... tol_D is zero!")

      ! Proceed?
      n = no_of_doubles_window_(self)
      if (n==0) return

      ! Generate Ds in the tol_D window.
      ! Sorted naturally in increasing BJ order
      ! which corresponds to the class label.
      call create_(self%double,n,2)  ! Double excitations
      call create_(self%delEab,n)  ! Koopman's energies
      d = size(self%single,1)
      n = 0
      do ai = 1,d
         a = self%single(ai,1)
         i = self%single(ai,2)
         Eai = self%deltaE(ai)
         do bj = ai,d  ! NOTE
            b = self%single(bj,1)
            j = self%single(bj,2)
            Ebj = self%deltaE(bj)
            val = Eai+Ebj
            if (abs(val-self%omega)<self%tol_D AND val<self%tol_S) then
               n = n+1
               self%delEab(n) = val-self%omega  ! Note sign
               self%double(n,1) = ai
               self%double(n,2) = bj
            end if
         end do
      end do

      ! Make the S-parents
      call make_S_list_(self)
      if (n>0) then
         ENSURE(all(self%S_list<=n),"TD_DATA:find_doubles_window ... bad S_list")
      end if
   end subroutine

   PURE subroutine find_doubles_Mazur(self)
   ! Find the double excitation closest in energy to the
   ! excited state of interest.
      type(TD_DATA_TYPE), INOUT :: self
      INT :: ai,a,i
      INT :: bj,b,j
      INT :: d,n
      REAL :: Eai,Ebj,val
      REAL :: eval,emin
   ENSURE(allocated(self%single),"TD_DATA:find_doubles_Mazur ... no single's array")
   ENSURE(allocated(self%deltaE),"TD_DATA:find_doubles_Mazur ... no deltaE array")
   ENSURE(allocated(self%eval),"TD_DATA:find_doubles_Mazur ... no eval array")
      n = size(self%eval)
      d = size(self%single,1)

      ! For each eval find  ...
      call create_(self%double,n,2)  ! Double excitation with
      call create_(self%delEab,n)  ! Closest Koopman's energy

      ! Double D for each eigenvalue.
      do n = 1,size(self%eval)
         eval = self%eval(n)
         emin = huge(ONE)  ! Initialise
         do ai = 1,d
            a = self%single(ai,1)
            i = self%single(ai,2)
            Eai = self%deltaE(ai)
            do bj = ai,d  ! NOTE: bj > ai
               b = self%single(bj,1)
               j = self%single(bj,2)
               Ebj = self%deltaE(bj)
               val = Eai+Ebj
               if (abs(val-eval)<abs(emin)) then
                  emin = val-eval
                  self%delEab(n) = emin  ! Note sign
                  self%double(n,1) = ai
                  self%double(n,2) = bj
               end if
            end do
         end do
      end do

      ! Make the S-parents
      call make_S_list_(self)
   end subroutine

   PURE subroutine make_S_list(self)
   ! Make S_list parent array which gives the first and last 
   ! indices where parent single "bj" has different "ai" values. 
   ! These parents are used to generate Fock matrix elements 
   ! and may be used to make a new singles-singles CIS block.
   ! NOTE: this also works for Mazur style one-double correction
   ! to each eigenvalue.
      type(TD_DATA_TYPE), INOUT :: self
      INT :: n_S,f,l,i
      VEC(INT,:), allocatable :: cnt
   ENSURE(allocated(self%double),"TD_DATA:make_S_list ... no double array")

      ! Count distinct singles-parent classes
      call make_repetition_count_(self%double(:,1),cnt,n_S)
      call create_(self%S_list,n_S,2)
      l = 0
      i = 0
      do
         f = l+1
         l = l+cnt(f)
         i = i+1
         self%S_list(i,1) = f
         self%S_list(i,2) = l
         if (i==n_S) exit
      end do
      call destroy_(cnt)
   end subroutine

   subroutine docu_put_doubles(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"put_doubles")
      call flush_(stdout)
      call text_(stdout,". Display table of the double-excitation detetrminants")
      call text_(stdout,"  within the omega-window used to correct the TD response")
      call text_(stdout,"  equations.")
      if (present(die)) stop
   end subroutine

   subroutine put_doubles(self)
   ! Print out the interacting double excitations
      type(TD_DATA_TYPE), IN :: self
      select case (self%dressing)
   ! case ("mazur ");   .put_doubles_Mazur
      case ("mazur "); call put_doubles_window_(self)
      case ("window"); call put_doubles_window_(self)
      case default; allocate(tonto%known_keywords(2))
      tonto%known_keywords(1) = "mazur "
      tonto%known_keywords(2) = "window"
      call unknown_(tonto,self%dressing,"TD_DATA:put_doubles")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine put_doubles_window(self)
   ! Print out the interacting double excitations
      type(TD_DATA_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: arrow
      VEC(STR(len=STR_SIZE),:), allocatable :: S_par
      INT :: n
   ENSURE(allocated(self%single),"TD_DATA:put_doubles_window ... No single array")
   ENSURE(allocated(self%double),"TD_DATA:put_doubles_window ... No single array")
   ENSURE(allocated(self%S_list),"TD_DATA:put_doubles_window ... No S_list array")
      call flush_(stdout)
      call text_(stdout,"=================")
      call text_(stdout,"CI doubles window")
      call text_(stdout,"=================")
      call flush_(stdout)
      call text_(stdout,". Double excitations D are selected in an energy")
      call text_(stdout,"  window such that:")
      call flush_(stdout)
      call text_(stdout,"   |E_D - omega| < tol_D,")
      call flush_(stdout)
      call text_(stdout,"  where E_D is D's Koopmans excitation energy,")
      call text_(stdout,"  and omega is a user-specified target energy,")
      call text_(stdout,"  usually a particular excitation energy, and")
      call text_(stdout,"  tol_D is a user-specified tolerance. Also")
      call text_(stdout,"  D must still lie with the singles window i.e.")
      call text_(stdout,"  we must still have: |E_D| < tol_S")
      call flush_(stdout)
      call text_(stdout,". Double D is made by two singles:")
      call text_(stdout,"  - I -> A label the 1st excitation")
      call text_(stdout,"  - J -> B label the 2nd excitation")
      call text_(stdout,"  where:")
      call text_(stdout,"  - I, J < 1 are occupied MOs; 0 is the HOMO")
      call text_(stdout,"  - A, B > 0 are virtual  MOs; 1 is the LUMO")
      call flush_(stdout)
      call text_(stdout,". Different classes of Ds coming from a particular")
      call text_(stdout,"  single excitation parent P (J -> B) are starred.")
      call text_(stdout,"  Each P is used to generate Fock matrix elements,")
      call text_(stdout,"  and to make another CIS block for that class.")
      call flush_(stdout)
      call text_(stdout,"Dressing energy")
      call show_(stdout,"  omega            =",self%omega)
      call show_(stdout,"  omega / eV       =",self%omega*EV_PER_HARTREE)
      call flush_(stdout)
      n = no_of_doubles_(self)
      call text_(stdout,"Doubles window")
      call show_(stdout,"  tol_D            =",self%tol_D)
      call show_(stdout,"  tol_D / eV       =",self%tol_D*EV_PER_HARTREE)
      call show_(stdout,"  No. of doubles   =",n)
      call show_(stdout,"  No. of classes   =",size(self%S_list,1))
      call flush_(stdout)
      call text_(stdout,"Singles window")
      call show_(stdout,"  tol_S            =",self%tol_S)
      call show_(stdout,"  tol_S / eV       =",self%tol_S*EV_PER_HARTREE)
      call show_(stdout,"  No. of singles   =",size(self%single,1))
      call flush_(stdout)
      call flush_(stdout)
      if (n==0) return
      call create_(arrow,n)
      arrow = "->"
      call create_(S_par,n)
      S_par = " "
      S_par(self%S_list(:,1)) = "*"
      call create_(table,11)
      call set_heading_(table(1),"AI")
      call set_heading_(table(2),"BJ")
      call set_heading_(table(3),"P?")
      call set_heading_(table(4),"I")
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"->")
      call set_width_(table(5),3)
      call set_heading_(table(6),"A")
      call set_heading_(table(7),"J")
      call set_column_spacing_(table(8),0)
      call set_heading_(table(8),"->")
      call set_width_(table(8),3)
      call set_heading_(table(9),"B")
      call set_heading_(table(10),"E_D")
      call set_subhead_(table(10),"/eV")
      call set_real_precision_(table(10),2)
      call set_heading_(table(11),"E_D-w")
      call set_subhead_(table(11),"/eV")
      call set_real_precision_(table(11),2)
      call set_values_(table(1),self%double(:,2))  ! AI
      call set_values_(table(2),self%double(:,1))  ! BJ
      call set_values_(table(3),S_par)
      call set_values_(table(4),self%single(self%double(:,2),2)-self%n_occ)  ! I
      call set_values_(table(5),arrow)
      call set_values_(table(6),self%single(self%double(:,2),1))  ! A
      call set_values_(table(7),self%single(self%double(:,1),2)-self%n_occ)  ! J
      call set_values_(table(8),arrow)
      call set_values_(table(9),self%single(self%double(:,1),1))  ! B
      call set_values_(table(10),(self%delEab+self%omega)*EV_PER_HARTREE)  ! + sign correct
      call set_values_(table(11),self%delEab*EV_PER_HARTREE)
      call put_(table,label_rows=TRUE)
      call destroy_(table)
      call destroy_(S_par)
      call destroy_(arrow)
   end subroutine

   subroutine put_doubles_Mazur(self)
   ! Print out the interacting double excitations
      type(TD_DATA_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: arrow
      INT :: n
   ENSURE(allocated(self%single),"TD_DATA:put_doubles_Mazur ... No single array")
   ENSURE(allocated(self%double),"TD_DATA:put_doubles_Mazur ... No single array")
      call flush_(stdout)
      call text_(stdout,"=================")
      call text_(stdout,"CI doubles window")
      call text_(stdout,"=================")
      call flush_(stdout)
      call text_(stdout,". The double excitations D whose energy is closest")
      call text_(stdout,"  to an existing CIS eigenvalue is selected.")
      call flush_(stdout)
      call text_(stdout,". Double D is made by two singles:")
      call text_(stdout,"  - I -> A label the 1st excitation")
      call text_(stdout,"  - J -> B label the 2nd excitation")
      call text_(stdout,"  where:")
      call text_(stdout,"  - I, J < 1 are occupied MOs; 0 is the HOMO")
      call text_(stdout,"  - A, B > 0 are virtual  MOs; 1 is the LUMO")
      n = no_of_doubles_(self)
      call flush_(stdout)
      call show_(stdout,"  No. of evals     =",self%n_evals)
      call show_(stdout,"  No. of doubles   =",n)
      call flush_(stdout)
      if (n==0) return
      call create_(arrow,n)
      arrow = "->"
      call create_(table,10)
      call set_heading_(table(1),"AI")
      call set_heading_(table(2),"BJ")
      call set_heading_(table(3),"I")
      call set_column_spacing_(table(4),0)
      call set_heading_(table(4),"->")
      call set_width_(table(4),3)
      call set_heading_(table(5),"A")
      call set_heading_(table(6),"J")
      call set_column_spacing_(table(7),0)
      call set_heading_(table(7),"->")
      call set_width_(table(7),3)
      call set_heading_(table(8),"B")
      call set_heading_(table(9),"E_D")
      call set_subhead_(table(9),"/eV")
      call set_real_precision_(table(9),2)
      call set_heading_(table(10),"E_D-w")
      call set_subhead_(table(10),"/eV")
      call set_real_precision_(table(10),2)
      call set_values_(table(1),self%double(:,2))  ! AI
      call set_values_(table(2),self%double(:,1))  ! BJ
      call set_values_(table(3),self%single(self%double(:,2),2)-self%n_occ)  ! I
      call set_values_(table(4),arrow)
      call set_values_(table(5),self%single(self%double(:,2),1))  ! A
      call set_values_(table(6),self%single(self%double(:,1),2)-self%n_occ)  ! J
      call set_values_(table(7),arrow)
      call set_values_(table(8),self%single(self%double(:,1),1))  ! B
      call set_values_(table(9),(self%delEab+self%eval)*EV_PER_HARTREE)  ! + sign correct
      call set_values_(table(10),self%delEab*EV_PER_HARTREE)
      call put_(table,label_rows=TRUE)
      call destroy_(table)
      call destroy_(arrow)
   end subroutine


!  ==================
!  Davidson iteration
!  ==================

! Inquiry

   PURE function converged(self) result (res)
   ! Return TRUE if the Davidson procedure is done i.e. either
   ! converged or gone on for too long ... and, it must also
   ! have executed the requested minimum number of times ...
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = residual_(self)<self%tol_Davidson AND self%iteration>=self%min_iterations AND self%iteration<=self%max_iterations
   end function

   pure function collapse(self) result (res)
   ! Collapse the Davidson subspace down to one set.
      type(TD_DATA_TYPE), IN :: self
      BIN :: res
      res = self%n_trial==self%n_keep
   end function

   pure function fst_trial_vector(self) result (res)
   ! Return the index of the first trial vector column
   ! in the current Davidson iteration.
      type(TD_DATA_TYPE), IN :: self
      INT :: res
      INT :: n,b
      n = self%n_evals
      b = self%n_trial
      res = n*(b-1)+1
   end function

   pure function lst_trial_vector(self) result (res)
   ! Return the index of the last trial vector column
   ! in the current Davidson iteration.
      type(TD_DATA_TYPE), IN :: self
      INT :: res
      INT :: n,b
      n = self%n_evals
      b = self%n_trial
      res = n*b
   end function

   pure function best_trial_vector_set(self) result (res)
   ! Get the trial set with the best residuals.
   ! NOTE: can't be used yet.
      type(TD_DATA_TYPE), IN :: self
      INT :: res
      res = index_of_minimum_(self%max_res)
   end function

   PURE function residual(self) result (res)
   ! Return current maximum of the Ritz-vector residuals.
   ! NOTE: .max_res must be set externally beforehand!
      type(TD_DATA_TYPE), IN :: self
      REAL :: res
   ENSURE(allocated(self%max_res),"TD_DATA:residual ... max_res not allocated")
      res = self%max_res(self%n_trial)
   end function

   PURE subroutine set_max_res(self,val)
   ! Set the maximum residual vector length for this iteration.
      type(TD_DATA_TYPE), INOUT :: self
      REAL, IN :: val
      INT :: i
   ENSURE(allocated(self%max_res),"TD_DATA:set_max_res ... no max_res")
      i = self%n_trial
      self%max_res(i) = val
   end subroutine


!  =================
!  Iteration control
!  =================

   subroutine initialize_Davidson(self,name)
   ! Initialise the Davidson iterations
   ! Use archive "name" for any initial vectors.
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: name
      INT :: d0,d1,d2,d3
      INT :: d,n,n4,s
      INT :: n_max
      type(ARCHIVE_TYPE) :: archive
      self%iteration = 0
      self%n_trial = 0

      ! Dimensions
      n = self%n_evals
      d = size(self%single,1)  ! "d" <= nv*no
      if (is_unrestricted_(self)) then
         d = 4*d
      end if

      ! Evals & evecs (leaky)
      call create_(self%eval,n)
      call create_(self%evec,d,n)

      ! Max. residual vector lengths (leaky)
      call create_(self%max_res,self%n_keep)
      self%max_res = ZERO

      ! Temporaries for CIS part
      n = self%n_evals
      n_max = n*self%n_keep
      call create_(self%V,d,n_max)  ! Davidson trial vector columns
      call create_(self%W,d,n_max)  ! W = A.V in MO basis
      call create_(self%R,d)  ! Residual vector

      ! Initial vectors
      ! The single-excitations states should be well-ordered
      ! e.g. in increasing energy and with sensible shape.
      ! Consider DFT, non-canonical VMOs, or Fermi-Amaldi.
      if (is_restricted_(self)) then
         call to_unit_mx_(self%V(:,1:n))
      else

         ! Zero
         self%V = ZERO
         call set_(archive,name,"CIS_evec,u")
         if (exists_(archive) AND self%archive_results) then

            ! Read archive, make labels
            call read_(archive,self%evec)
            self%V(:,1:n) = self%evec
            call set_(archive,name,"CIS_eval,u")
            call read_(archive,self%eval)
            call make_u_Slab_(self)
         ! .put_evecs
         ! .find_doubles
         ! .put_doubles
         else
            if (NOT self%use_M_0) then

               ! Initialise S=0 & S=1
               ! shape(V) is really (d,4,n*n_keep)
               d = size(self%single,1)
               d0 = 0
               do s = 1,n,4
                  d0 = d0+1
                  d1 = d0+d
                  d2 = d1+d
                  d3 = d2+d
                  self%V(d0,s) = ONE  ! aa
                  self%V(d1,s+1) = ONE  ! ba
                  self%V(d2,s+2) = ONE  ! ab
                  self%V(d3,s+3) = ONE  ! bb
               end do
            else
               d = size(self%single,1)
               d0 = 0
               do s = 1,n,2  ! Double above case
                  d0 = d0+1  ! 2,3,4 ...
                  d1 = d0+d
                  d2 = d1+d
                  d3 = d2+d
                  self%V(d0,s) = ONE  ! aa
                  self%V(d3,s+1) = ONE  ! bb
               end do
            end if
         end if
      end if

      ! Davidson temporaries for CIS part
      ! --- V or J & K in AO basis
      if (is_restricted_(self)) then
         call create_(self%Vao,n,self%n_bf,self%n_bf)  ! Density matrices
         call create_(self%Aao,n,self%n_bf,self%n_bf)  ! A.V in AO basis
      else
         n4 = 4*n
         call create_(self%Vao,n4,self%n_bf,self%n_bf)
         call create_(self%Jao,n4,self%n_bf,self%n_bf)  ! J & K bits of A.V
         call create_(self%Kao,n4,self%n_bf,self%n_bf)
      end if

      ! Set table headings
      call set_D_table_headings_(self)
   end subroutine

   pure subroutine update_iteration(self)
   ! Update the SCF iteration
      type(TD_DATA_TYPE), INOUT :: self
      self%iteration = self%iteration+1
      self%n_trial = self%n_trial+1  ! Always incremented
   end subroutine

   pure subroutine reset_subspace(self)
   ! Reset the Davidson subspace to one trial vector set.
      type(TD_DATA_TYPE), INOUT :: self
      INT :: n

      ! Keep last vectors
      n = size(self%evec,2)
      self%V(:,1:n) = self%evec

      ! Keep last error
      self%max_res(1) = self%max_res(self%n_trial)
      self%max_res(2:) = ZERO

      ! One trial set
      self%n_trial = 1
   end subroutine

   PURE subroutine finalize_iteration(self)
   ! Clean up memory for Davidson procedure
      type(TD_DATA_TYPE), INOUT :: self

      ! Temporaries
      if (is_restricted_(self)) then
         call destroy_(self%Aao)
         call destroy_(self%Vao)
      else
         call destroy_(self%Kao)
         call destroy_(self%Jao)
         call destroy_(self%Vao)
      end if
      call destroy_(self%R)
      call destroy_(self%W)
      call destroy_(self%V)
   end subroutine

   subroutine store_results(self,name)
   ! Store the results, make table Slab info.
   ! NOTE: merging of S=0 and S=1 
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: name
      if (is_restricted_(self)) then
         call store_r_results_(self,name)
         call make_r_Slab_(self)
      else
         call store_u_results_(self,name)
         call make_u_Slab_(self)
      end if
   end subroutine

   subroutine store_r_results(self,name)
   ! Save results in core and on disk.
   ! In core needed for later merging of S=0 and S=1 results.
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: name
      type(ARCHIVE_TYPE) :: archive

      ! Store singlets
      if (is_singlet_calc_(self)) then
         self%eval0 = self%eval
         self%evec0 = self%evec
      end if

      ! Store triplets
      if (is_triplet_calc_(self)) then
         self%eval1 = self%eval
         self%evec1 = self%evec
      end if
      if (NOT self%archive_results) return

      ! Archive singlets
      if (is_singlet_calc_(self)) then
         call set_(archive,name,"CIS_eval_S=0,r")
         call write_(archive,self%eval0)
         call set_(archive,name,"CIS_evec_S=0,r")
         call write_(archive,self%evec0)
      end if

      ! Archive triplets
      if (is_triplet_calc_(self)) then
         call set_(archive,name,"CIS_eval_S=1,r")
         call write_(archive,self%eval1)
         call set_(archive,name,"CIS_evec_S=1,r")
         call write_(archive,self%evec1)
      end if
   end subroutine

   PURE subroutine make_r_Slab(self)
   ! Make restricted Slab labels for output table.
      type(TD_DATA_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: ord
      INT :: n,i
      n = size(self%evec,2)
      if (is_singlet_calc_(self)) then
         call create_(self%Slab,n)  ! Labels
         ord = [(i,i = 1,n)]
         self%Slab = spread("S",1,n)//to_str_(ord)
         call destroy_(ord)
      end if
      if (is_triplet_calc_(self)) then
         call create_(self%Slab,n)  ! Labels
         ord = [(i,i = 1,n)]
         self%Slab = spread("T",1,n)//to_str_(ord)
         call destroy_(ord)
      end if
   end subroutine

   subroutine store_u_results(self,name)
   ! Store unrestricted results on disk only.
   ! Clean up, store the results, with sorting if needed.
   ! UNRESTRICTED CASE.
      type(TD_DATA_TYPE), INOUT :: self
      STR(len=*), IN :: name
      type(ARCHIVE_TYPE) :: archive
      if (NOT self%archive_results) return

      ! Archive
      call set_(archive,name,"CIS_evec,u")
      call write_(archive,self%evec)
      call set_(archive,name,"CIS_eval,u")
      call write_(archive,self%eval)
   end subroutine

   subroutine make_u_Slab(self)
   ! Make unrestricted Slab labels for output table.
      type(TD_DATA_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: cnt
      VEC(REAL,:), allocatable :: eval
      INT :: n,i,n_kind
      INT :: k1,k2,k3,nt,ns
      n = size(self%evec,2)
      call create_(self%Slab,n)  ! Labels
      self%Slab = " "
      if (NOT self%use_M_0) then
         eval = self%eval*EV_PER_HARTREE
         call make_repetition_count_(eval,cnt,n_kind,eps=TOL(3))
         k1 = 0
         k2 = 0
         k3 = 0
         do i = 1,n
            select case (cnt(i))
            case (0)
               self%Slab(i) = " "
            case (1)
               k1 = k1+1
               self%Slab(i) = "S"//trim(to_str_(k1))
            case (2)
               k2 = k2+1
               self%Slab(i) = "D"//trim(to_str_(k2))
            case (3)
               k3 = k3+1
               self%Slab(i) = "T"//trim(to_str_(k3))
            end select
         end do
         call destroy_(cnt)
         call destroy_(eval)
      else
         ns = 0
         nt = 0
         do i = 1,n
            if (is_M0_singlet_(self,i)) then
               ns = ns+1
               self%Slab(i) = "S"//trim(to_str_(ns))
            else
               nt = nt+1
               self%Slab(i) = "T"//trim(to_str_(nt))
            end if
         end do
      end if
   end subroutine

   PURE subroutine merge_S01_results(self)
   ! Merge S=0 and S=1 results ready for printing
   ! RESTART BECOMES IMPOSSIBLE IN THIS CASE.
      type(TD_DATA_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: ind
      VEC(INT,:), allocatable :: ord
      INT :: d,n,i
   ENSURE(is_s0_and_s1_calc_(self),"TD_DATA:merge_S01_results ... not a S=0&1 calc")
      n = size(self%evec,2)
      self%eval = [self%eval0,self%eval1]
      d = size(self%evec,1)
      call create_(self%evec,d,2*n)
      self%evec(:,1:n) = self%evec0
      self%evec(:,n+1:) = self%evec1

      ! Set S values
      call create_(self%S,2*n)
      self%S(1:n) = 0
      self%S(n+1:) = 1

      ! State order
      call create_(self%Slab,2*n)
      call create_(ord,2*n)

      ! Singlet lower in energy
      ord(1:n) = [(i,i = 1,n)]
      ord(n+1:) = [(i,i = 1,n)]
      self%Slab(1:n) = spread("S",1,n)//to_str_(ord(1:n))
      self%Slab(n+1:) = spread("T",1,n)//to_str_(ord(n+1:))
      call destroy_(ord)

      ! Sort
      call create_(ind,2*n)
      call quick_sort_(self%eval,ind)

      ! Reorder
      self%S = self%S(ind)
      self%eval = self%eval(ind)
      self%evec = self%evec(:,ind)
      self%Slab = self%Slab(ind)
      call destroy_(ind)
   end subroutine


!  =======================
!  Efficient A x V product
!  =======================

   PURE subroutine make_u_diag_AV(self,AV,V)
   ! Do the *unrestricted* diag(A).V matrix product on the
   ! single-excitation trial vectors "V" and put the result
   ! into "AV". Will assume non-canonical VMOs if the VV
   ! sector of the Fock matrix .Fv is allocated.
   ! NOTE: if the G state is included nothing need be done
   ! if an initial vector V (1 0 0 ... )^T is used, it should
   ! remain unmofied if no S or D matrix elements are used.
      type(TD_DATA_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: AV
      MAT(REAL,:,:), IN :: V
      INT :: ai,a,i
      INT :: bj,b,j
      INT :: d,d2,d3,n
      REAL :: Eai,Ei,Fab
   ENSURE(is_same_shape_as_(AV,V),"TD_DATA:make_u_diag_AV ... inconsistent shape, V and AV")
   ENSURE(size(AV,2)==self%n_evals,"TD_DATA:make_u_diag_AV ... wrong dim2, AV")
   ENSURE(size(AV,1)==4*size(self%single,1),"TD_DATA:make_u_diag_AV ... wrong dim1, AV")

      ! NOTE: d refers to the *restricted* case.
      ! The unrestricted case is *FOUR* times as big.
      ! The spin order in V is: aa, ba, ab, bb for each "ai"
      d = size(self%single,1)
      d2 = 2*d
      d3 = 3*d
      if (NOT allocated(self%Fv)) then

         ! Canonical VMOs
         do n = 1,size(V,2)
            do ai = 1,d
               Eai = self%deltaE(ai)
               AV(ai,n) = Eai*V(ai,n)  ! aa
               AV(d+ai,n) = Eai*V(d+ai,n)  ! ba
               AV(d2+ai,n) = Eai*V(d2+ai,n)  ! ab
               AV(d3+ai,n) = Eai*V(d3+ai,n)  ! bb
            end do
         end do
      else

         ! Noncanonical VMO case
         do n = 1,size(V,2)
            do ai = 1,d
               a = self%single(ai,1)
               i = self%single(ai,2)
               Ei = -self%Eo(i)
               AV(ai,n) = Ei*V(ai,n)  ! aa
               AV(d+ai,n) = Ei*V(d+ai,n)  ! bb
               AV(d2+ai,n) = Ei*V(d2+ai,n)  ! ab
               AV(d3+ai,n) = Ei*V(d3+ai,n)  ! bb
               do bj = 1,d
                  b = self%single(bj,1)
                  j = self%single(bj,2)
                  if (i/=j) cycle
                  Fab = self%Fv(a,b)
                  AV(ai,n) = AV(ai,n)+Fab*V(bj,n)  ! aa
                  AV(d+ai,n) = AV(d+ai,n)+Fab*V(d+bj,n)  ! ba
                  AV(d2+ai,n) = AV(d2+ai,n)+Fab*V(d2+bj,n)  ! ab
                  AV(d3+ai,n) = AV(d3+ai,n)+Fab*V(d3+bj,n)  ! bb
               end do
            end do
         end do
      end if
   end subroutine

   PURE subroutine make_r_diag_AV(self,AV,V)
   ! Do the diag(A).V matrix product on single-excitation
   ! trial vectors "V" and put the result into "AV".
   ! Can cope with non-canonical VMOs if the VV sector of
   ! the Fock matrix .Fv is allocated.
      type(TD_DATA_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: AV
      MAT(REAL,:,:), IN :: V
      INT :: ai,a,i
      INT :: bj,b,j
      INT :: d,n
   ENSURE(is_same_shape_as_(AV,V),"TD_DATA:make_r_diag_AV ... inconsistent shape, V and AV")
   ENSURE(size(AV,2)==self%n_evals,"TD_DATA:make_r_diag_AV ... wrong dim2, AV")
   ENSURE(size(AV,1)==size(self%single,1),"TD_DATA:make_r_diag_AV ... wrong dim1, AV")

      ! Sizes
      d = size(self%single,1)
      if (NOT allocated(self%Fv)) then

         ! Canonical VMOs
         do n = 1,size(V,2)
            do ai = 1,d
               AV(ai,n) = self%deltaE(ai)*V(ai,n)
            end do
         end do
      else

         ! Noncanonical VMO case
         do n = 1,size(V,2)
            do ai = 1,d
               a = self%single(ai,1)
               i = self%single(ai,2)
               AV(ai,n) = -self%Eo(i)*V(ai,n)
               do bj = 1,d
                  b = self%single(bj,1)
                  j = self%single(bj,2)
                  if (i/=j) cycle
                  AV(ai,n) = AV(ai,n)+self%Fv(a,b)*V(bj,n)
               end do
            end do
         end do
      end if
   end subroutine


!   change_u_V_to_AO(Vao,V) :: PURE
!   ! ***************************************
!   ! NOTE: Only the _r_ code is used, below.
!   ! ***************************************
!   ! Back transform *unrestricted* Davidson trial vectors
!   ! "V" into symmetric (n_bf x n_bf) AO-sized density mx's
!   ! "Vao", for each excited state eigenvalue. Needed to get
!   ! the 2e-part of the A.V Davidson step in CIS/TD eqns.
!   ! NOTE: "V" is 4 times the size of the restricted case.
!   !       Due to the the spin blocks: aa, ba, ab bb.
!   ! NOTE: the result does *not* accumulate.
!      self :: IN
!      Vao  :: MAT3{REAL}, OUT
!      V    :: MAT{REAL}, IN
!
!   ENSURE(Vao.dim1==4*.n_evals,"inconsistent dim1, Vao")
!
!      d,d2,d3,d4 :: INT
!      ai,a,i :: INT
!      no, lo :: INT
!      n, p,q :: INT
!      val :: REAL
!      W :: MAT3{REAL}@
!
!      ! Sizes & limits
!      d  = .single.dim1 - 1 ! Remove G
!      d2 = 2*d
!      d3 = 3*d
!      d4 = 4*d
!
!      no = .n_occ
!      lo = .min_OMO
!
!      ! Single index transform temporary
!      ! Last index stores spin blocks.
!      W.create([1,.n_bf],[lo,no],[1,4*.n_evals])
!      W = ZERO
!
!      ! Transform "a" index of "V" -> "W"
!      do n  = 1, .n_evals
!      do p  = 1, .n_bf
!      do ai = 2, d+1
!         a = .single(ai,1)
!         i = .single(ai,2)
!         ! a -> p
!         val = .MOv(p,a)
!         W(p,i,n  ) = W(p,i,n  ) + val*V(   ai,n) ! aa
!         W(p,i,n+1) = W(p,i,n+1) + val*V(d +ai,n) ! ba
!         W(p,i,n+2) = W(p,i,n+2) + val*V(d2+ai,n) ! ab
!         W(p,i,n+3) = W(p,i,n+3) + val*V(d3+ai,n) ! bb
!      end
!      end
!      end
!
!      ! Transform "i" index of "W" -> "Vao"
!      Vao = ZERO
!      do n = 1, .n_evals
!      do i = lo, no
!      do p = 1, .n_bf
!      do q = 1, .n_bf
!         ! i -> q
!         val = .MOo(q,i)
!         Vao(n  ,p,q) = Vao(n  ,p,q) + val*W(p,i,n  )
!         Vao(n+1,p,q) = Vao(n+1,p,q) + val*W(p,i,n+1)
!         Vao(n+2,p,q) = Vao(n+2,p,q) + val*W(p,i,n+2)
!         Vao(n+3,p,q) = Vao(n+3,p,q) + val*W(p,i,n+3)
!      end
!      end
!      end
!      end
!
!      ! Clean up
!      W.destroy
!
!   end

   PURE subroutine change_r_V_to_AO(self,Vao,V)
   ! Back transform trial eigenvectors "V" into symmetric
   ! (n_bf x n_bf) AO sized density matrices "Vao", for
   ! each excited state eigenvalue. Needed to calculate
   ! the 2e-part of the A.V Davidson step in CIS/TD eqns.
   ! NOTE: the result does *not* accumulate.
      type(TD_DATA_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: Vao
      MAT(REAL,:,:), IN :: V
      INT :: ai,a,i
      INT :: d,no,lo
      INT :: n,p,q
      MAT3(REAL,:,:,:), allocatable :: W
   ENSURE(size(Vao,1)==size(V,2),"TD_DATA:change_r_V_to_AO ... inconsistent dim1, Vao")

      ! Sizes & limits
      d = size(self%single,1)
      no = self%n_occ
      lo = self%min_OMO

      ! Single-index transform temporary
      call create_(W,[1,self%n_bf],[lo,no],[1,size(V,2)])
      W = ZERO

      ! Transform "a" index of "V" -> "W"
      do n = 1,size(V,2)
         do p = 1,self%n_bf
            do ai = 1,d
               a = self%single(ai,1)
               i = self%single(ai,2)
         ! a -> p
               W(p,i,n) = W(p,i,n)+self%MOv(p,a)*V(ai,n)
            end do
         end do
      end do

      ! Transform "i" index of "W" -> "Vao"
      Vao = ZERO
      do i = lo,no
         do p = 1,self%n_bf
            do q = 1,self%n_bf
               do n = 1,size(Vao,1)

         ! i -> q
                  Vao(n,p,q) = Vao(n,p,q)+self%MOo(q,i)*W(p,i,n)
               end do
            end do
         end do
      end do

      ! Clean up
      call destroy_(W)
   end subroutine

   PURE subroutine change_u_V_to_MO(self,V,Jao,Kao)
   ! Transform and *ACCUMULATE* AO-basis "Jao" and "Kao"
   ! matrices from the 2e part of the CIS equations into
   ! the correct spin blocks of the unrestricted Davidson
   ! trial eigenvectors "V" in single-excitation MO basis.
   ! Accumulation is needed so as to add onto the 1e term.
   ! NOTE:
   ! For each eigenvalue "n" in V(:,n) the spin blocks for 
   ! the first index "ai" are contiguous in the order:
   ! aa, ba, ab, bb, each block comprising "d" elements.
   ! The n=1 G state is not considered for V.
      type(TD_DATA_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: V
      MAT3(REAL,:,:,:), IN :: Jao
      MAT3(REAL,:,:,:), IN :: Kao
      INT :: d0,d1,d2,d3
      INT :: n1,n2,n3,n4
      INT :: d,no,lo
      INT :: ai,a,i
      INT :: n,p,q
      MAT3(REAL,:,:,:), allocatable :: WJ,WK
      REAL :: Jaa,Jbb
      REAL :: Kaa,Kbb
      REAL :: Kba,Kab
      REAL :: val
   ENSURE(size(Jao,1)==4*size(V,2),"TD_DATA:change_u_V_to_MO ... incompatible dim1 & dim2, Jao & V")
   ENSURE(size(Jao,1)==4*self%n_evals,"TD_DATA:change_u_V_to_MO ... wrong dim1, Jao")
   ENSURE(size(Jao,2)==self%n_bf,"TD_DATA:change_u_V_to_MO ... wrong dim2, Jao")
   ENSURE(size(Jao,3)==self%n_bf,"TD_DATA:change_u_V_to_MO ... wrong dim3, Jao")
   ENSURE(is_same_shape_as_(Kao,Jao),"TD_DATA:change_u_V_to_MO ... Kao inconsistent shape with Jao")

      ! Sizes
      d = size(self%single,1)
      no = self%n_occ
      lo = self%min_OMO
      n1 = size(V,2)
      n2 = 2*n1
      n3 = 3*n1
      n4 = 4*n1

      ! Single-index transform temporaries
      call create_(WJ,[1,n4],[1,self%n_bf],[lo,no])
      call create_(WK,[1,n4],[1,self%n_bf],[lo,no])
      WJ = ZERO
      WK = ZERO

      ! Transform "q" index
      do i = lo,no
         do q = 1,self%n_bf
            val = self%MOo(q,i)
            do p = 1,self%n_bf
               do n = 1,n4
                  Jaa = Jao(n,p,q)*val
                  Kaa = Kao(n,p,q)*val
                  WJ(n,p,i) = WJ(n,p,i)+Jaa
                  WK(n,p,i) = WK(n,p,i)+Kaa
               end do
            end do
         end do
      end do

      ! Transform "p" index, *accumulate* in "V"
      ! For each eigenvalue "n" in V(:,n) the spin blocks for 
      ! the first index "ai" (starting at 2, since 1 is the G 
      ! state) are contiguous in the order aa, ba, ab, bb,
      ! each block comprising "d" elements
      do p = 1,self%n_bf
         do ai = 1,d
            a = self%single(ai,1)
            i = self%single(ai,2)
            d0 = ai
            d1 = d+d0
            d2 = d+d1
            d3 = d+d2
            val = self%MOv(p,a)
            do n = 1,n1
               Jaa = WJ(n,p,i)*val
               Jbb = WJ(n3+n,p,i)*val
               Kaa = WK(n,p,i)*val
               Kba = WK(n1+n,p,i)*val
               Kab = WK(n2+n,p,i)*val
               Kbb = WK(n3+n,p,i)*val
               V(d0,n) = V(d0,n)+Jaa-Kaa+Jbb  ! (ai)_aa
               V(d1,n) = V(d1,n)-Kba  ! (ai)_ba
               V(d2,n) = V(d2,n)-Kab  ! (ai)_ab
               V(d3,n) = V(d3,n)+Jbb-Kbb+Jaa  ! (ai)_bb
            end do
         end do
      end do

      ! Clean up
      call destroy_(WK)
      call destroy_(WJ)
   end subroutine

   PURE subroutine change_r_V_to_MO(self,V,Vao)
   ! Transform and *accumulate* AO-basis matrices "Vao" back
   ! into Davidson trial eigenvectors "V" in the state basis.
      type(TD_DATA_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: V
      MAT3(REAL,:,:,:), IN :: Vao
      INT :: ai,a,i
      INT :: d,no,lo
      INT :: n,p,q
      MAT3(REAL,:,:,:), allocatable :: W
   ENSURE(size(Vao,1)==size(V,2),"TD_DATA:change_r_V_to_MO ... incompatible dim1 & dim2, Vao & V")
   ENSURE(size(Vao,1)==self%n_evals,"TD_DATA:change_r_V_to_MO ... wrong dim1, Vao")
   ENSURE(size(Vao,2)==self%n_bf,"TD_DATA:change_r_V_to_MO ... wrong dim2, Vao")
   ENSURE(size(Vao,3)==self%n_bf,"TD_DATA:change_r_V_to_MO ... wrong dim3, Vao")

      ! Sizes
      d = size(self%single,1)
      no = self%n_occ
      lo = self%min_OMO

      ! Transform "a" index
      call create_(W,[1,size(V,2)],[1,self%n_bf],[lo,no])
      W = ZERO

      ! Transform "q" index
      do i = lo,no
         do q = 1,self%n_bf
            do p = 1,self%n_bf
               do n = 1,size(V,2)
                  W(n,p,i) = W(n,p,i)+Vao(n,p,q)*self%MOo(q,i)
               end do
            end do
         end do
      end do

      ! Transform "p" index
      do n = 1,size(V,2)
         do p = 1,self%n_bf
            do ai = 1,d
               a = self%single(ai,1)
               i = self%single(ai,2)
               V(ai,n) = V(ai,n)+W(n,p,i)*self%MOv(p,a)
            end do
         end do
      end do

      ! Clean up
      call destroy_(W)
   end subroutine


!   change_AV_to_MO(AV,V) :: PURE
!   ! Transform the (n_bf x n_bf) matrices "AV" and *accumulate*
!   ! into trial eigenvectors "V" in the state basis.
!      self :: IN
!      AV :: MAT3{REAL}, IN
!      V  :: MAT{REAL},  INOUT
!
!   ENSURE(AV.dim2==.n_bf,   "wrong dim2, AV")
!   ENSURE(AV.dim2==AV.dim3, "AV not square in dim2 & dim3")
!   ENSURE(AV.dim1==.n_evals,"inconsistent dim1, AV")
!   ENSURE(AV.dim1==V.dim2,  "inconsistent dim2, V")
!
!      ai,a,i :: INT
!      d,no,n :: INT
!      lo, hv :: INT
!      C :: MAT{REAL}@
!
!      ! Sizes & limits
!      d  = .single.dim1
!      no = .n_occ
!      lo = .min_OMO
!      hv = .max_VMO
!
!      ! Single excitations
!      C.create([1,hv],[lo,no])
!
!      do n = 1,.n_evals
!
!         ! AV (MO)  -> C_ai(MO)
!         AV(n,:,:).change_basis_to(C,.MOv(:,1:hv),.MOo(:,lo:no))
!
!         ! C_ai(MO) -> V
!         do ai = 1,d
!            a = .single(ai,1)
!            i = .single(ai,2)
!            V(ai,n) = V(ai,n) + C(a,i)
!         end
!
!      end
!
!      ! Clean up
!      C.destroy
!
!   end

!  ======================
!  State selection output
!  ======================

   subroutine put_banner(self)
   ! Prints out the coupled-perturbed banner
      type(TD_DATA_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"============================")
      call text_(stdout,"CIS/TD excited state options")
      call text_(stdout,"============================")
      call flush_(stdout)
      call put_options_(self)
      call put_MO_energies_(self)
      call put_singles_(self)
   ! .put_doubles
   end subroutine

   subroutine docu_put_options(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call text_(stdout,"put_options")
      call flush_(stdout)
      call text_(stdout,". Display a summary of options used to perform the")
      call text_(stdout,"  response equation calculations")
      if (present(die)) stop
   end subroutine

   subroutine put_options(self)
   ! Prints summary of what options being used.
      type(TD_DATA_TYPE), IN :: self
      INT :: n
      call flush_(stdout)
      call show_(stdout,"Method              = ",self%method)
      call show_(stdout,"Implementation      = ",self%implementation)
      call show_(stdout,"Use IVO energies?   = ",self%use_IVO_energies)
      call show_(stdout,"Use M=0 states?     = ",self%use_M_0)
      call flush_(stdout)
      call text_(stdout,"No. of electrons")
      call show_(stdout,"  n_occ             = ",self%n_occ)
      call show_(stdout,"  n_vir             = ",self%n_vir)
      call show_(stdout,"  n_occ x n_vir     = ",self%n_occ*self%n_vir)
      n = no_of_singles_(self)
      call flush_(stdout)
      call text_(stdout,"Davidson options")
      call show_(stdout,"  n_evals wanted    = ",self%n_evals)
      call show_(stdout,"  n_keep  blocks    = ",self%n_keep)
      call show_(stdout,"  No. determinants  = ",n)
      call show_(stdout,"  Max. iterations   = ",self%max_iterations)
      call show_(stdout,"  Convergence tol   = ",self%tol_Davidson)
      call flush_(stdout)
      call text_(stdout,"Singles window")
      call show_(stdout,"  tol_S             =",self%tol_S)
      call show_(stdout,"  tol_S / eV        =",self%tol_S*EV_PER_HARTREE)
      if (self%implementation/="in-core" OR is_an_SS_method_(self)) then
         call show_(stdout,"  No. of singles    =",n)
      end if
      call flush_(stdout)
      call text_(stdout,"Orbital range")
      call show_(stdout,"  Min. occupied MO  =",self%min_OMO-self%n_occ+1)
      call show_(stdout,"  Max. virtual  MO  =",self%max_VMO)
      call flush_(stdout)
      call text_(stdout,"Dressing energy")
      call show_(stdout,"  omega             =",self%omega)
      call show_(stdout,"  omega / eV        =",self%omega*EV_PER_HARTREE)
      call flush_(stdout)
      call text_(stdout,"Doubles window")
      call show_(stdout,"  tol_D             =",self%tol_D)
      call show_(stdout,"  tol_D / eV        =",self%tol_D*EV_PER_HARTREE)
      call flush_(stdout)
   end subroutine

   subroutine put_MO_energies(self)
   ! Prints out the MO energies. May make use of .single
   ! excitation energy to reduce output.
      type(TD_DATA_TYPE), IN :: self
      INT :: no,lo,hv
      call flush_(stdout)
      call text_(stdout,"===========")
      call text_(stdout,"MO energies")
      call text_(stdout,"===========")
      if (NOT allocated(self%single)) then

         ! No S window info
         call flush_(stdout)
         call text_(stdout,"OMO energies:")
         call put_(stdout,self%Eo)
         hv = min(size(self%Ev),10)  ! Limit output
         call flush_(stdout)
         call text_(stdout,"VMO energies:")
         call put_(stdout,self%Ev(1:hv))
      else

         ! Use S window to reduce output
         no = self%n_occ
         lo = self%min_OMO
         hv = min(self%max_VMO,10)  ! Limit output
         call flush_(stdout)
         call text_(stdout,". Only the topmost HOMO, and lowest LUMO, energies")
         call text_(stdout,"  are written below, those in the S window.")
         call flush_(stdout)
         call text_(stdout,". All energies are in ascending order")
         call flush_(stdout)
         call show_(stdout,"  No. of OMOs       = ",self%n_occ)
         call show_(stdout,"  Min. occupied MO  =",self%min_OMO-self%n_occ+1)
         call show_(stdout,"  Max. virtual  MO  =",self%max_VMO)
         call flush_(stdout)
         call text_(stdout,"OMO energies:")
         call put_(stdout,self%Eo(lo:no))
         call flush_(stdout)
         call text_(stdout,"VMO energies:")
         call put_(stdout,self%Ev(1:hv))
      end if
   end subroutine

   subroutine put_MOs(self)
   ! Print out the occupied MOs.
      type(TD_DATA_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"===")
      call text_(stdout,"MOs")
      call text_(stdout,"===")
      call flush_(stdout)
      call text_(stdout,"Occupied MOs:")
      call put_(stdout,self%MOo)
   end subroutine


!  ===============
!  Davidson output
!  ===============

   subroutine put_results(self)
   ! Print out the interacting double excitations
      type(TD_DATA_TYPE), INOUT :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n
   ENSURE(allocated(self%eval),"TD_DATA:put_results ... no eval's")
      call flush_(stdout)
      call text_(stdout,"============================")
      call text_(stdout,"CIS/TD excited state results")
      call text_(stdout,"============================")
      call put_options_(self)
      n = size(self%eval)
      if (n==0) return
      if (allocated(self%Slab)) then; call create_(table,3)
      else; call create_(table,2)
      end if

      ! Print table
      call set_heading_(table(1),"Delta E")
      call set_subhead_(table(1),"    /au")
      call set_real_precision_(table(1),4)
      call set_width_(table(1),10)
      call set_values_(table(1),self%eval(1:n))
      call set_heading_(table(2),"Delta E")
      call set_subhead_(table(2),"    /eV")
      call set_real_precision_(table(2),2)
      call set_width_(table(2),8)
      call set_values_(table(2),self%eval(1:n)*EV_PER_HARTREE)
      if (allocated(self%Slab)) then

      ! A singlet & triplet calculation
         call set_heading_(table(3)," State")
         call set_subhead_(table(3),"  spin")
         call set_width_(table(3),3)
         call set_values_(table(3),self%Slab(1:n))
      end if
      call put_(table,label_rows=TRUE)
      call destroy_(table)
   end subroutine

   PURE subroutine set_D_table_headings(self)
   ! Set up the Davidson table
      type(TD_DATA_TYPE), INOUT :: self
      INT :: i,n
      STR(len=STR_SIZE) :: state
      n = self%n_evals

      ! Set table headings
      call create_(self%table,n+2)
      call set_heading_(self%table(1),"Iter")
      call force_width_(self%table(1),5)
      call set_heading_(self%table(2),"Resid")
      call set_subhead_(self%table(2)," norm")
      call set_real_precision_(self%table(2),4)
      call set_width_(self%table(2),9)
      if (is_unrestricted_(self)) then; state = "?"
      else if (is_singlet_calc_(self)) then; state = "S"
      else; state = "T"
      end if
      do i = 1,n
         if (allocated(self%Slab)) then
            call set_heading_(self%table(i+2),trim(self%Slab(i)))
         else
            call set_heading_(self%table(i+2),trim(state)//trim(to_str_(i)))
         end if
         call set_width_(self%table(i+2),7)
         call set_real_precision_(self%table(i+2),2)
      end do
   end subroutine

   subroutine put_D_table_header(self)
   ! Prints out the Davidson header, and table heading.
      type(TD_DATA_TYPE), IN :: self
      INT :: d
   ENSURE(allocated(self%table),"TD_DATA:put_D_table_header ... no table created")
      d = no_of_singles_(self)
      call flush_(stdout)
      call text_(stdout,"=====================")
      call text_(stdout,"Davidson diagonaliser")
      call text_(stdout,"=====================")
      call flush_(stdout)
      call text_(stdout,". State energies are in eV")
      call flush_(stdout)
      call text_(stdout,". n_keep is the number of trial eigenvector sets to")
      call text_(stdout,"  keep in the block Davidson procedure.")
      call flush_(stdout)
      call text_(stdout,". n_occ*n_vir is the maximum size of the Davidson matrix")
      call text_(stdout,"  but a fewer of determinants are used if state selected")
      call text_(stdout,"  method is used.")
      call flush_(stdout)
      call text_(stdout,". Convergence decided by the maximum of the absolute")
      call text_(stdout,"  norm of the residual of each eigenvector.")
   ! stdout.flush
   ! stdout.text(". For unrestricted calcs the no. of eigenvalues is")
   ! stdout.text("  four times more than asked, to allow for triplets.")
      call flush_(stdout)
      call text_(stdout,"No. of electrons")
      call show_(stdout,"  n_occ             = ",self%n_occ)
      call show_(stdout,"  n_vir             = ",self%n_vir)
      call show_(stdout,"  n_occ x n_vir     = ",self%n_occ*self%n_vir)
      call flush_(stdout)
      call text_(stdout,"Davidson options")
      call show_(stdout,"  n_evals wanted    = ",self%n_evals)
      call show_(stdout,"  n_keep  blocks    = ",self%n_keep)
      call show_(stdout,"  No. determinants  = ",size(self%single,1))
      call show_(stdout,"  Max. iterations   = ",self%max_iterations)
      call show_(stdout,"  Convergence tol.  = ",self%tol_Davidson)

      ! Put the table
      if (self%show_iterations) call put_headings_(self%table)
   end subroutine

   subroutine put_D_table_body_and_footer(self)
   ! Print out the Davidson table body for the current iteration.
   ! If converged, print out footer as well.
      type(TD_DATA_TYPE), INOUT :: self
      INT :: i
   ENSURE(allocated(self%table),"TD_DATA:put_D_table_body_and_footer ... no table created")
   ENSURE(allocated(self%eval),"TD_DATA:put_D_table_body_and_footer ... no eigenvalue")

      ! Actually show output?
      if (self%show_iterations) then

      ! Set table body (leaky)
         call set_values_(self%table(1),[self%iteration],auto=FALSE)
         call set_values_(self%table(2),[self%max_res(self%n_trial)],auto=FALSE)
   ! .table(3).set_values([.collapse])
         do i = 1,self%n_evals
            call set_values_(self%table(i+2),[self%eval(i)*EV_PER_HARTREE])
         end do

      ! Put out the body
         call put_body_(self%table)

      ! Clear columns for next time (UNleaky!)
         call clear_columns_(self%table)
      end if
      if (converged_(self)) then
         call put_footer_(self%table)
         call flush_(stdout)
         if (converged_(self)) then; call text_(stdout,"* * * Converged * * *")
         else; call text_(stdout,"* * * NOT CONVERGED * * *")
         end if
         call put_footer_(self%table)
         call flush_(stdout)
      end if
   end subroutine

   subroutine put_evecs(self)
   ! Print out the eigenvectors
      type(TD_DATA_TYPE), IN :: self
   ENSURE(allocated(self%single),"TD_DATA:put_evecs ... no single states")
   ENSURE(allocated(self%evec),"TD_DATA:put_evecs ... evec unallocated")
      if (NOT self%show_evecs) return
      if (NOT is_unrestricted_(self)) then; call put_r_evecs_(self)
      else; call put_u_evecs_(self)
      end if
   end subroutine

   subroutine put_r_evecs(self)
   ! Print out the eigenvectors
      type(TD_DATA_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: arrow
      VEC(STR(len=STR_SIZE),:), allocatable :: Is,As
      INT :: wI,wA
      MAT(REAL,:,:), allocatable :: evec
      INT :: d,n,i
      REAL :: val
      STR(len=STR_SIZE) :: num
   ENSURE(allocated(self%single),"TD_DATA:put_r_evecs ... no single states")
   ENSURE(allocated(self%evec),"TD_DATA:put_r_evecs ... evec unallocated")
      call flush_(stdout)
      call text_(stdout,"===================")
      call text_(stdout,"Eigenvector columns")
      call text_(stdout,"===================")
      call flush_(stdout)
      d = size(self%evec,1)
      call create_(Is,d)
      call create_(As,d)
      call create_(arrow,d)
      Is = r_O_singles_str_(self)
      As = r_V_singles_str_(self)
      arrow = "->"
      call get_max_str_length_(Is,wI,spaces=0)
      call get_max_str_length_(As,wA,spaces=0)
      n = size(self%eval)
      call create_(table,4+n)
      call set_column_spacing_(table(1),0)
      call set_heading_(table(1),"I")
      call set_width_(table(1),wI+1)
      call set_values_(table(1),Is)
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"->")
      call set_width_(table(2),3)
      call set_values_(table(2),arrow)
      call set_column_spacing_(table(3),0)
      call set_heading_(table(3),"A")
      call set_width_(table(3),wA+1)
      call set_values_(table(3),As)
      call set_heading_(table(4),"Ev-Eo")
      call set_subhead_(table(4),"  /eV")
      call set_real_precision_(table(4),2)
      call set_width_(table(4),5)
      call set_values_(table(4),self%deltaE*EV_PER_HARTREE)
      do i = 1,n
         val = self%eval(i)*EV_PER_HARTREE
         num = trim(to_str_(val,"f10.2"))
         call set_heading_(table(4+i),self%Slab(i))
         call set_subhead_(table(4+i),trim(num))
         call set_real_precision_(table(4+i),4)
         call set_width_(table(4+i),9)
         call set_values_(table(4+i),self%evec(:,i))
      end do
      call put_(table,label_rows=TRUE)
      call destroy_(table)
      call destroy_(evec)
      call destroy_(arrow)
      call destroy_(As)
      call destroy_(Is)
   end subroutine

   subroutine put_u_evecs(self)
   ! Print out the eigenvectors
      type(TD_DATA_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: Is,As
      VEC(STR(len=STR_SIZE),:), allocatable :: arrow
      VEC(REAL,:), allocatable :: delEv
      VEC(STR(len=STR_SIZE),:), allocatable :: delE
      INT :: wI,wA
      MAT(REAL,:,:), allocatable :: evec
      INT :: d,d4,n,i
      REAL :: val
      STR(len=STR_SIZE) :: num
   ENSURE(allocated(self%single),"TD_DATA:put_u_evecs ... no single states")
   ENSURE(allocated(self%evec),"TD_DATA:put_u_evecs ... evec unallocated")
      call flush_(stdout)
      call text_(stdout,"===================")
      call text_(stdout,"Eigenvector columns")
      call text_(stdout,"===================")
      call flush_(stdout)
      if (NOT self%use_M_0) then
         call text_(stdout,"The four numbers per excitation correspond to:")
         call flush_(stdout)
         call text_(stdout,"  I alpha -> A alpha i.e. aa")
         call text_(stdout,"  I alpha -> A beta  i.e. ab")
         call text_(stdout,"  I beta  -> A alpha i.e. ba")
         call text_(stdout,"  I beta  -> A beta  i.e. bb")
         call flush_(stdout)
         call text_(stdout,"The ab and ba terms are spin flip excitations and make")
         call text_(stdout,"M = -1 and M = +1 triplet components.")
         call flush_(stdout)
         call text_(stdout,"The aa and bb terms make M = 0 components for singlet and")
         call text_(stdout,"triplet with same sign combinations being the former.")
      else
         call text_(stdout,"The two non-zero numbers per excitation correspond to")
         call flush_(stdout)
         call text_(stdout,". I alpha -> A alpha i.e. aa")
         call text_(stdout,". I beta  -> A beta  i.e. bb")
         call flush_(stdout)
         call text_(stdout,"These generate the M = 0 components for singlet and")
         call text_(stdout,"triplet, with same sign combinations being a singlet.")
      end if
      call flush_(stdout)
      n = min(size(self%eval),self%n_max_show)
      d4 = size(self%evec,1)
      d = d4/4
      ENSURE(d4==d*4,"TD_DATA:put_u_evecs ... evec dim1 not a multiple of 4")
      call create_(Is,d4)
      call create_(As,d4)
      call create_(delE,d4)
      call create_(delEv,d4)
      call create_(arrow,d4)
      Is = u_O_singles_str_(self)
      As = u_V_singles_str_(self)
      arrow = " "
      arrow(1:d4:4) = "->"
      delEv = self%deltaE*EV_PER_HARTREE
      delE = " "
      delE(1:d4:4) = to_str_(delEv,"f",5,2)
      call get_max_str_length_(Is,wI,spaces=0)
      call get_max_str_length_(As,wA,spaces=0)

      ! Swap order back
      call create_(evec,d4,n)
      if (self%method=="u-cis") then

         ! Order: is aa, ab, ba, bb
         evec = self%evec(:,1:n)
      else

         ! Put to:  aa, ab, ba, bb
         evec(1:d4:4,:) = self%evec(1:d,:)
         evec(2:d4:4,:) = self%evec(d+1:d*2,:)
         evec(3:d4:4,:) = self%evec(d*2+1:d*3,:)
         evec(4:d4:4,:) = self%evec(d*3+1:d*4,:)
      end if
      call create_(table,4+n)
      call set_column_spacing_(table(1),0)
      call set_heading_(table(1),"I")
      call set_width_(table(1),wI+1)
      call set_values_(table(1),Is)
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"->")
      call set_width_(table(2),3)
      call set_values_(table(2),arrow)
      call set_column_spacing_(table(3),0)
      call set_heading_(table(3),"A")
      call set_width_(table(3),wA+1)
      call set_values_(table(3),As)
      call set_heading_(table(4),"Ev-Eo")
      call set_subhead_(table(4),"  /eV")
      call set_width_(table(4),5)
      call set_values_(table(4),delE)
      do i = 1,n
         val = self%eval(i)*EV_PER_HARTREE
         num = trim(to_str_(val,"f10.2"))
         call set_heading_(table(4+i),self%Slab(i))
         call set_subhead_(table(4+i),trim(num))
         call set_real_precision_(table(4+i),4)
         call set_width_(table(4+i),9)
         call set_values_(table(4+i),evec(:,i))
      end do

      ! No rows
      call put_(table)
      call destroy_(table)
      call destroy_(evec)
      call destroy_(arrow)
      call destroy_(delEv)
      call destroy_(delE)
      call destroy_(As)
      call destroy_(Is)
   end subroutine

end module
