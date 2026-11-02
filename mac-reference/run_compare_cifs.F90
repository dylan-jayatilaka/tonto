! Copyright (C) Dylan Jayatilaka, 1999
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
! $Id: run_compare_cifs $
program run_COMPARE_CIFS

#  include "macros"

   include "run_compare_cifs.use"

   implicit none

   type(COMMAND_LINE_TYPE) :: command_line
   STR(len=STR_SIZE) :: refcif,cmpcif,head,label
   STR(len=1) :: op,cl
   type(ATOM_TYPE), PTR :: ref_atom,cmp_atom
   type(MOLECULE_TYPE), allocatable, TARGET :: ref,cmp
   INT :: n_atom,a,n_H_atoms,n_X_atoms,n
   MAT(REAL,3,3) :: UI1,UI2
   VEC(REAL,6) :: rADP_vector,rADP_errors
   VEC(REAL,6) :: cADP_vector,cADP_errors
   VEC(REAL,6) :: ADP_abs,ADP_ab2
   VEC(REAL,6) :: ADP_dfs,ADP_df2
   VEC(REAL,6) :: ADP_sig,ADP_siv
   VEC(REAL,3) :: ADP_rat,ADP_ra2
   REAL :: rUiso_val,cUiso_val
   REAL :: rUiso_err,cUiso_err
   REAL :: Uiso_abs
   REAL :: Uiso_dfs,Uiso_df2
   REAL :: Uiso_sig,Uiso_siv
   REAL :: Uiso_rat,Uiso_ra2
   REAL :: rMani_val,cMani_val
   REAL :: rMani_err,cMani_err
   REAL :: Mani_abs
   REAL :: Mani_dfs,Mani_df2
   REAL :: Mani_sig,Mani_siv
   REAL :: Mani_rat,Mani_ra2
   REAL :: RMSD
   REAL :: tmp,fac
   REAL :: S12
   VEC(REAL,3) :: rpos,cpos,rerr,cerr
   MAT(REAL,3,3) :: Ai,C3
   VEC(STR(len=1),3) :: xyz
   INT :: k

   ! Create Tonto system object
   ! Initialise MPI parallel stuff too.
   TONTO_CREATE

   ! Initialise standard I/O files.
   call start_timing_(std_time)
   call create_std_err
   call create_stdout

   ! Errors go to the screen with the rest, not to a file of their own
   call redirect_std_err_to(stdout)
   xyz = ["x","y","z"]

   ! Initialize miscellaneous ...
   call set_indices_(GAUSSIAN_DATA_L_MAX)

   ! Get the command line
   call process_options_(command_line)
   DIE_IF(NOT has_arguments_(command_line),"run_COMPARE_CIFS:run_COMPARE_CIFS ... Usage: compare_cifs --ref <reference-cif> <compared-cif>")
   DIE_IF(NOT command_line%n_args==1,"run_COMPARE_CIFS:run_COMPARE_CIFS ... Usage: compare_cifs --ref <reference-cif> <compared-cif>")
   DIE_IF(NOT has_options_(command_line),"run_COMPARE_CIFS:run_COMPARE_CIFS ... Usage: compare_cifs --ref <reference-cif> <compared-cif>")
   DIE_IF(NOT command_line%n_options==1,"run_COMPARE_CIFS:run_COMPARE_CIFS ... Usage: compare_cifs --ref <reference-cif> <compared-cif>")
   DIE_IF(command_line%option(1)/="ref","run_COMPARE_CIFS:run_COMPARE_CIFS ... Usage: compare_cifs --ref <reference-cif> <compared-cif>")

   ! Get options
   cmpcif = command_line%arg(1)
   refcif = command_line%option_value(1)
   head = filename_head_(cmpcif)

   ! Create the molecules and process the CIFs
   call create_(ref)
   call create_(cmp)
   call create_(ref%cif)
   call create_(cmp%cif)
   call set_file_name_(ref%cif,refcif)
   call set_file_name_(cmp%cif,cmpcif)
   call process_CIF_(ref)
   call process_CIF_(cmp)

   ! NOTE: it does not check for CIF2 at this point, which can cause
   ! errors.

   ! Sanity checks
   DIE_IF(NOT ref%n_atom==cmp%n_atom,"run_COMPARE_CIFS:run_COMPARE_CIFS ... different no. of atoms in each CIF")
   DIE_IF(NOT same_as_(ref%atom(:)%atomic_number,cmp%atom(:)%atomic_number),"run_COMPARE_CIFS:run_COMPARE_CIFS ... atom order is different in each CIF")

   ! Write the heavy atom data files
   n_atom = ref%n_atom

   ! Script banner
   call flush_(stdout)
   call text_(stdout,"# ============")
   call text_(stdout,"# Compare CIFs")
   call text_(stdout,"# ============")
   call flush_(stdout)
   call text_(stdout,"# This program compares CIF geometries and ADPs and produces")
   call text_(stdout,"# this gnuplot command file suitable for plotting.")
   call flush_(stdout)
   call show_(stdout,"# CIF file to compare   = ",cmpcif)
   call show_(stdout,"# CIF file reference    = ",refcif)

   ! Save & reset precisions
   call save_style_(stdout)
   call set_real_width_(stdout,25)
   call set_real_precision_(stdout,16)

   ! Heavy atom positions
   call redirect_(stdout,trim(head)//".non_h_atoms.fcoords")
   do a = 1,n_atom
      ref_atom => ref%atom(a)
      cmp_atom => cmp%atom(a)
      if (ref_atom%atomic_number<=1) cycle
      label = ref_atom%label

      ! Fractional coordinates and their esds, from the cartesian ones
      rpos = ref_atom%pADP_vector(1:3)
      cpos = cmp_atom%pADP_vector(1:3)
      call change_into_fractional_(ref%crystal%unit_cell,rpos)
      call change_into_fractional_(cmp%crystal%unit_cell,cpos)
      Ai = ref%crystal%unit_cell%inverse_mx
      do k = 1,3
         rerr(k) = sqrt(sum((Ai(k,:)*ref_atom%pADP_errors(1:3))**2))
      end do
      if (allocated(ref_atom%covariance_mx)) then
         C3 = matmul(Ai,matmul(ref_atom%covariance_mx(1:3,1:3),transpose(Ai)))
         do k = 1,3
            rerr(k) = sqrt(max(C3(k,k),ZERO))
         end do
      end if
      Ai = cmp%crystal%unit_cell%inverse_mx
      do k = 1,3
         cerr(k) = sqrt(sum((Ai(k,:)*cmp_atom%pADP_errors(1:3))**2))
      end do
      if (allocated(cmp_atom%covariance_mx)) then
         C3 = matmul(Ai,matmul(cmp_atom%covariance_mx(1:3,1:3),transpose(Ai)))
         do k = 1,3
            cerr(k) = sqrt(max(C3(k,k),ZERO))
         end do
      end if
      do k = 1,3
         call put_(stdout,trim(label)//xyz(k))
         call put_(stdout,rpos(k))
         call put_(stdout,rerr(k))
         call put_(stdout,cpos(k))
         call put_(stdout,cerr(k))
         call flush_(stdout)
      end do
   end do
   call revert_(stdout)

   ! H atom positions
   call redirect_(stdout,trim(head)//".the_h_atoms.fcoords")
   do a = 1,n_atom
      ref_atom => ref%atom(a)
      cmp_atom => cmp%atom(a)
      if (ref_atom%atomic_number>1) cycle
      label = ref_atom%label

      ! Fractional coordinates and their esds, from the cartesian ones
      rpos = ref_atom%pADP_vector(1:3)
      cpos = cmp_atom%pADP_vector(1:3)
      call change_into_fractional_(ref%crystal%unit_cell,rpos)
      call change_into_fractional_(cmp%crystal%unit_cell,cpos)
      Ai = ref%crystal%unit_cell%inverse_mx
      do k = 1,3
         rerr(k) = sqrt(sum((Ai(k,:)*ref_atom%pADP_errors(1:3))**2))
      end do
      if (allocated(ref_atom%covariance_mx)) then
         C3 = matmul(Ai,matmul(ref_atom%covariance_mx(1:3,1:3),transpose(Ai)))
         do k = 1,3
            rerr(k) = sqrt(max(C3(k,k),ZERO))
         end do
      end if
      Ai = cmp%crystal%unit_cell%inverse_mx
      do k = 1,3
         cerr(k) = sqrt(sum((Ai(k,:)*cmp_atom%pADP_errors(1:3))**2))
      end do
      if (allocated(cmp_atom%covariance_mx)) then
         C3 = matmul(Ai,matmul(cmp_atom%covariance_mx(1:3,1:3),transpose(Ai)))
         do k = 1,3
            cerr(k) = sqrt(max(C3(k,k),ZERO))
         end do
      end if
      do k = 1,3
         call put_(stdout,trim(label)//xyz(k))
         call put_(stdout,rpos(k))
         call put_(stdout,rerr(k))
         call put_(stdout,cpos(k))
         call put_(stdout,cerr(k))
         call flush_(stdout)
      end do
   end do

   ! Heavy atom ADPs
   call revert_(stdout)
   call redirect_(stdout,trim(head)//".non_h_atoms.ADPs")

   ! Absolute, absolute-square & difference sum
   ! For each ADP
  ! ADP_dfs = ZERO; ADP_df2 = ZERO
  ! ADP_abs = ZERO; ADP_ab2 = ZERO
  ! ADP_sig = ZERO; ADP_siv = ZERO
  ! ADP_rat = ZERO; ADP_ra2 = ZERO

  ! Uiso_dfs = ZERO
  ! Uiso_df2 = ZERO
  ! Uiso_abs = ZERO
  ! Uiso_siv = ZERO
  ! Uiso_sig = ZERO
  ! Uiso_rat = ZERO
  ! Uiso_ra2 = ZERO

  ! Mani_dfs = ZERO
  ! Mani_df2 = ZERO
  ! Mani_abs = ZERO
  ! Mani_siv = ZERO
  ! Mani_sig = ZERO
  ! Mani_rat = ZERO
  ! Mani_ra2 = ZERO
   n_X_atoms = 0
   fac = ANGSTROM_PER_BOHR
   fac = fac*fac
   do a = 1,n_atom
      ref_atom => ref%atom(a)
      cmp_atom => cmp%atom(a)
      if (ref_atom%atomic_number<=1) cycle
      n_X_atoms = n_X_atoms+1
      label = ref_atom%label
      rUiso_val = ref_atom%U_iso*fac
      rUiso_err = ref_atom%U_iso_error*fac
      cUiso_val = cmp_atom%U_iso*fac
      cUiso_err = cmp_atom%U_iso_error*fac

!      RMSD = sqrt((cUiso_val-rUiso_val)**2/((cUiso_err)**2 + (rUiso_err)**2))
      call put_(stdout,trim(label)//":Uiso")
      call put_(stdout,rUiso_val)
      call put_(stdout,rUiso_err)
      call put_(stdout,cUiso_val)
      call put_(stdout,cUiso_err)
!      stdout.put(RMSD)
      call flush_(stdout)
   end do
   do a = 1,n_atom
      ref_atom => ref%atom(a)
      cmp_atom => cmp%atom(a)
      if (ref_atom%atomic_number<=1) cycle
      label = ref_atom%label

      ! Note the factor squared here comes from the Mani value already
      ! including the factor, so this needs to be accounted for
      rMani_val = ref_atom%M_ani*fac
      rMani_err = ref_atom%M_ani_error*fac
      cMani_val = cmp_atom%M_ani*fac
      cMani_err = cmp_atom%M_ani_error*fac

!      RMSD = (cMani_val-rMani_val)/rMani_val*100
      call put_(stdout,trim(label)//":Mani")
      call put_(stdout,rMani_val)
      call put_(stdout,rMani_err)
      call put_(stdout,cMani_val)
      call put_(stdout,cMani_err)
!      stdout.put(RMSD)
      call flush_(stdout)
   end do
!      rADP_vector = ref_atom.pADP_vector(4:9)
!      rADP_errors = ref_atom.pADP_errors(4:9)
!      cADP_vector = cmp_atom.pADP_vector(4:9)
!      cADP_errors = cmp_atom.pADP_errors(4:9)
!      rADP_vector.convert_to("angstrom^2")
!      rADP_errors.convert_to("angstrom^2")
!      cADP_vector.convert_to("angstrom^2")
!      cADP_errors.convert_to("angstrom^2")
!
!    ! where (abs(rADP_errors)<TOL(7)) rADP_errors = ONE
!    ! where (abs(cADP_errors)<TOL(7)) cADP_errors = ONE
!
!      stdout.put(trim(label)//"Uxx")
!      stdout.put(rADP_vector(1))
!      stdout.put(rADP_errors(1))
!      stdout.put(cADP_vector(1))
!      stdout.put(cADP_errors(1))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uyy")
!      stdout.put(rADP_vector(2))
!      stdout.put(rADP_errors(2))
!      stdout.put(cADP_vector(2))
!      stdout.put(cADP_errors(2))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uzz")
!      stdout.put(rADP_vector(3))
!      stdout.put(rADP_errors(3))
!      stdout.put(cADP_vector(3))
!      stdout.put(cADP_errors(3))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uxy")
!      stdout.put(rADP_vector(4))
!      stdout.put(rADP_errors(4))
!      stdout.put(cADP_vector(4))
!      stdout.put(cADP_errors(4))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uxz")
!      stdout.put(rADP_vector(5))
!      stdout.put(rADP_errors(5))
!      stdout.put(cADP_vector(5))
!      stdout.put(cADP_errors(5))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uyz")
!      stdout.put(rADP_vector(6))
!      stdout.put(rADP_errors(6))
!      stdout.put(cADP_vector(6))
!      stdout.put(cADP_errors(6))
!      stdout.flush
   do a = 1,n_atom
      ref_atom => ref%atom(a)
      cmp_atom => cmp%atom(a)
      if (ref_atom%atomic_number<=1) cycle
      label = ref_atom%label
      call to_inverse_of_(UI1,ref_atom%ADP_tensor)
      call to_inverse_of_(UI2,cmp_atom%ADP_tensor)
      S12 = S12_with_ADP_(UI1,UI2)
      call put_(stdout,trim(label)//"S12:")
      call put_(stdout,S12)
      call flush_(stdout)
   end do
   n = n_X_atoms

  ! stdout.flush
  ! stdout.text("# U_iso figures of merit:")
  ! tmp = Uiso_dfs/n
  ! stdout.show("# < Del U_iso >             =",tmp)
  ! stdout.show("# error in above            =",sqrt(Uiso_df2/n-tmp*tmp))

  ! tmp = Uiso_abs/n
  ! stdout.show("# <|Del U_iso|>             =",tmp)
  ! stdout.show("# error in above            =",sqrt(Uiso_df2/n-tmp**2))

  ! tmp = Uiso_df2/n
  ! stdout.show("# <(Del U_iso)^2>^{1/2}     =",sqrt(Uiso_df2/n))
  ! stdout.show("# error in above            =",sqrt(Uiso_df2/n-tmp**2))

  ! tmp = Uiso_siv/n
  ! stdout.show("# <(Del U_iso/sig)^2>^{1/2} =",sqrt(Uiso_sig/n))
  ! stdout.show("# error in above            =",sqrt(Uiso_sig/n-tmp**2))

  ! stdout.flush
  ! stdout.text("# M_ani figures of merit:")
  ! tmp = Mani_dfs/n
  ! stdout.show("# < Del M_ani >             =",tmp)
  ! stdout.show("# error in above            =",sqrt(Mani_df2/n-tmp*tmp))

  ! tmp = Mani_abs/n
  ! stdout.show("# <|Del M_ani|>             =",tmp)
  ! stdout.show("# error in above            =",sqrt(Mani_df2/n-tmp**2))

  ! tmp = Mani_df2/n
  ! stdout.show("# <(Del M_ani)^2>^{1/2}     =",sqrt(Mani_df2/n))
  ! stdout.show("# error in above            =",sqrt(Mani_df2/n-tmp**2))

  ! tmp = Mani_siv/n
  ! stdout.show("# <(Del M_ani/sig)^2>^{1/2} =",sqrt(Mani_sig/n))
  ! stdout.show("# error in above            =",sqrt(Mani_sig/n-tmp**2))

   ! Hydrogen atom ADPs
   call revert_(stdout)
   call redirect_(stdout,trim(head)//".the_h_atoms.ADPs")
!
!   ! Absolute, absolute-square & difference sum
!   ! For each ADP
!   ADP_dfs = ZERO; ADP_df2 = ZERO
!   ADP_abs = ZERO; ADP_ab2 = ZERO
!   ADP_sig = ZERO; ADP_siv = ZERO
!   ADP_rat = ZERO; ADP_ra2 = ZERO
!
!   n_H_atoms = 0
!
   do a = 1,n_atom
      ref_atom => ref%atom(a)
      cmp_atom => cmp%atom(a)
      if (ref_atom%atomic_number>1) cycle
      label = ref_atom%label
      rUiso_val = ref_atom%U_iso*fac
      rUiso_err = ref_atom%U_iso_error*fac
      cUiso_val = cmp_atom%U_iso*fac
      cUiso_err = cmp_atom%U_iso_error*fac

!      RMSD = sqrt((cUiso_val-rUiso_val)**2/((cUiso_err)**2 + (rUiso_err)**2))
      call put_(stdout,trim(label)//":Uiso")
      call put_(stdout,rUiso_val)
      call put_(stdout,rUiso_err)
      call put_(stdout,cUiso_val)
      call put_(stdout,cUiso_err)
!      stdout.put(RMSD)
      call flush_(stdout)
   end do
   do a = 1,n_atom
      ref_atom => ref%atom(a)
      cmp_atom => cmp%atom(a)
      if (ref_atom%atomic_number>1) cycle
      n_H_atoms = n_H_atoms+1
      label = ref_atom%label

      ! Note the factor squared here comes from the Mani value already
      ! including the factor, so this needs to be accounted for
      rMani_val = ref_atom%M_ani*fac
      rMani_err = ref_atom%M_ani_error*fac
      cMani_val = cmp_atom%M_ani*fac
      cMani_err = cmp_atom%M_ani_error*fac

!      RMSD = (cMani_val-rMani_val)/rMani_val*100
      call put_(stdout,trim(label)//":Mani")
      call put_(stdout,rMani_val)
      call put_(stdout,rMani_err)
      call put_(stdout,cMani_val)
      call put_(stdout,cMani_err)
!      stdout.put(RMSD)
      call flush_(stdout)
   end do
   do a = 1,n_atom
      ref_atom => ref%atom(a)
      cmp_atom => cmp%atom(a)
      if (ref_atom%atomic_number>1) cycle
      label = ref_atom%label

    ! where (abs(rADP_errors)<TOL(7)) rADP_errors = ONE
    ! where (abs(cADP_errors)<TOL(7)) cADP_errors = ONE

!      stdout.put(trim(label)//"Uxx")
!      stdout.put(rADP_vector(1))
!      stdout.put(rADP_errors(1))
!      stdout.put(cADP_vector(1))
!      stdout.put(cADP_errors(1))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uyy")
!      stdout.put(rADP_vector(2))
!      stdout.put(rADP_errors(2))
!      stdout.put(cADP_vector(2))
!      stdout.put(cADP_errors(2))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uzz")
!      stdout.put(rADP_vector(3))
!      stdout.put(rADP_errors(3))
!      stdout.put(cADP_vector(3))
!      stdout.put(cADP_errors(3))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uxy")
!      stdout.put(rADP_vector(4))
!      stdout.put(rADP_errors(4))
!      stdout.put(cADP_vector(4))
!      stdout.put(cADP_errors(4))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uxz")
!      stdout.put(rADP_vector(5))
!      stdout.put(rADP_errors(5))
!      stdout.put(cADP_vector(5))
!      stdout.put(cADP_errors(5))
!      stdout.flush
!
!      stdout.put(trim(label)//"Uyz")
!      stdout.put(rADP_vector(6))
!      stdout.put(rADP_errors(6))
!      stdout.put(cADP_vector(6))
!      stdout.put(cADP_errors(6))
!      stdout.flush
      call to_inverse_of_(UI1,ref_atom%ADP_tensor)
      call to_inverse_of_(UI2,cmp_atom%ADP_tensor)
      S12 = S12_with_ADP_(UI1,UI2)
      call put_(stdout,trim(label)//"S12:")
      call put_(stdout,S12)
      call flush_(stdout)
   end do

   ! Include S/R12 values here Below is the routing from Mat Real.
   ! Need to pass in the inverse of the two ADP matrices.:

!   n = 6*n_H_atoms
!
!   stdout.flush
!   stdout.text("# U_ij figures of merit:")
!   tmp = sum(ADP_dfs)/n
!   stdout.show("# < Del U_ij >             =",tmp)
!   stdout.show("# error in above           =",sqrt(sum(ADP_df2)/n-tmp*tmp))
!
!   tmp = sum(ADP_abs)/n
!   stdout.show("# <|Del U_ij|>             =",tmp)
!   stdout.show("# error in above           =",sqrt(sum(ADP_ab2)/n-tmp**2))
!
!   tmp = sum(ADP_abs)/n
!   stdout.show("# <(Del U_ij)^2>^{1/2}     =",sqrt(sum(ADP_ab2)/n))
!   stdout.show("# error in above           =",sqrt(sum(ADP_ab2)/n-tmp**2))
!
!   tmp = sum(ADP_siv)/n
!   stdout.show("# <(Del U_ij/sig)^2>^{1/2} =",sqrt(sum(ADP_sig)/n))
!   stdout.show("# error in above           =",sqrt(sum(ADP_sig)/n-tmp**2))
!
!   n = 3*n_H_atoms
!
!   stdout.text("# U_ii figures of merit:")
! ! stdout.show("# < Del U_ii >             =",sum(ADP_dfs(1:3))/n)
! ! stdout.show("# <|Del U_ii|>             =",sum(ADP_abs(1:3))/n)
! ! stdout.show("# <(Del U_ii)^2>^{1/2}     =",sqrt(sum(ADP_ab2(1:3))/n))
! ! stdout.show("# <(Del U_ii/sig)^2>^{1/2} =",sqrt(sum(ADP_sig(1:3))/n))
!   tmp = sum(ADP_rat)/n
!   stdout.show("# <U_ii^X/U_ii^N>          =",tmp)
!   stdout.show("# Error in above           =",sqrt(sum(ADP_ra2)/n-tmp**2))
!
!   stdout.revert
!
!   ! Now make the gnuplot command file for histogram plots
!   ! "ga12.standard.h_atoms.xyz_data"
!
!   op = achar(91) ! open square-becaket
!   cl = achar(93) ! close square-bracket
!
!   stdout.redirect("compare.gnuplot")
!
!   stdout.text('# This gnuplot script compares two sets of CIF data.')
!   stdout.text('#')
!   stdout.text('# To use type:')
!   stdout.text('#    gnuplot -e "data='//"'<data-file>'"//'; title='//"'<title>'"//'" compare.gnuplot')
!   stdout.text('#')
!   stdout.text('# <data-file> has repeated lines of format:')
!   stdout.text('#    varname x dx y dy')
!   stdout.text('#')
!   stdout.text('# varname is the string label of the variable used as the x label.')
!   stdout.text('# x (resp. dx) is the reference data (resp. its error) and likewise for y.')
!   stdout.text('#')
!   stdout.text('# A histogram of deviation (y - x) divided by the compound mean is made.')
!   stdout.text('# The relative precision dy/dx is also displayed. A separate plot of the')
!   stdout.text('# the frequency distribution is also made. Its assumed the reference data')
!   stdout.text('# x (dx) are for neutrons, and the y (dy) data is for X-rays.')
!   stdout.text('# Change this script if you need to.')
!   stdout.text('psfile = data . "_histogram.ps"')
!   stdout.text('set output psfile')
!   stdout.text('set terminal postscript eps color enhanced')
!   stdout.text('set ylabel "Deviation (compound-{/Symbol s} units)"')
!   stdout.text('set xlabel title offset 0,-2')
!   stdout.text('set nokey')
!   stdout.text('set style fill solid 0.4')
!   stdout.text('set style histogram clustered')
!   stdout.text('set style data histograms')
!   stdout.text('set xtics 1')
!   stdout.text('set ytics 1')
!   stdout.text('set yrange '//op//'-10:10'//cl)
!   stdout.text('set grid ytics lc rgb "#7f7f7f" lw 1 lt 0')
!   stdout.text('set grid xtics lc rgb "#7f7f7f" lw 1 lt 0')
!   stdout.text('set xtics rotate')
!   stdout.text('sigma(x,y) = sqrt(x**2+y**2)')
!   stdout.text('dify(x,d,y,e) = (y - x)/sigma(d,e)')
!   stdout.text('plot \')
!   stdout.text('data using (dify($2,$3,$4,$5)):xticlabel(1)')
!   stdout.text('#')
!   stdout.text('psfile = data . "_frequency.ps"')
!   stdout.text('set output psfile')
!   stdout.text('set terminal postscript eps color enhanced')
!   stdout.text('set ylabel "Frequency"')
!   stdout.text('set xlabel "Deviation (compound-{/Symbol s} units)" offset 0,0')
!   stdout.text('set nokey')
!   stdout.text('set style fill solid 0.4')
!   stdout.text('set style histogram clustered')
!   stdout.text('set style data histograms')
!   stdout.text('set xrange '//op//'-6:6'//cl)
!   stdout.text('set yrange '//op//'0:10'//cl)
!   stdout.text('set xtics 1')
!   stdout.text('set ytics 1')
!   stdout.text('set xtics norotate')
!   stdout.text('set grid xtics lc rgb "#7f7f7f" lw 1 lt 0')
!   stdout.text('set grid ytics lc rgb "#7f7f7f" lw 1 lt 0')
!   stdout.text('sigma(x,y) = sqrt(x**2+y**2)')
!   stdout.text('dify(x,d,y,e) = (y - x)/sigma(d,e)')
!   stdout.text('bin(x,s) = s*floor(x/s)')
!   stdout.text('bw = 1.0')
!   stdout.text('set boxwidth bw*0.6')
!   stdout.text('plot \')
!   stdout.text('data using (bin(dify($2,$3,$4,$5),bw)+bw/2.0):(1.0) smooth frequency with boxes')
   call revert_(stdout)

   ! Unsave stdout settings
   call unsave_style_(stdout)

   ! Clean-up
   call destroy_(cmp)
   call destroy_(ref)

   ! Clean-up files
   call destroy_(std_err)
   call destroy_(stdout)

   ! Clean-up tonto system
   TONTO_DESTROY

end program
