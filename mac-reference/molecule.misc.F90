!==================================================================
!
! MOLECULE.MISC: 
!
! An object representation of a molecule.
! These are miscellaneous uncategorized procedures.
!
! Copyright (C) Dylan Jayatilaka, 2023
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
module MOLECULE_MISC_MODULE

#  include "macros"

   include "molecule.misc.use"

   implicit none

   include "molecule.misc.int"


contains

!  =========
!  Gradients
!  =========

!   optimize_geometry
!   ! Optimise the Hartree-Fock SCF energy gradient.
!      k,i :: INT
!      p :: VEC{REAL}@
!      energy :: REAL
!      self :: target
!      p.create(3*.n_atom)
!      k = 0
!      do i = 1,.n_atom
!         p(k+1) = .atom(i).position(1)
!         p(k+2) = .atom(i).position(2)
!         p(k+3) = .atom(i).position(3)
!         k = k + 3
!      end
!     .set_saved_self
!      call minimize_BFGS(HF_energy,HF_energy_gradient,p,energy,TOL(3),TOL(3),TOL(1),25)
!      stdout.show("Optimised energy =",energy)
!      stdout.text("Optimised geometry:")
!      stdout.put(p)
!      p.destroy
!   end
!
!   HF_energy(p) result (res) :: selfless
!   ! Evaluates the total HF SCF energy (including nuclear)
!      p :: VEC{REAL}
!     res :: REAL
!     self :: MOLECULE@
!   ENSURE(.n_atom*3==p.dim,"wrong size for p")
!     .set_from_saved_self
!     if (NOT .atom.coordinate_vector.same_as(p,tol=TOL(9))) then
!        .atom.set_positions_from_vector(p)
!        .delete_SCF_integrals
!        .HF
!     end
!     res = .SCF_energy
!   end
!
!   the_HF_energy result (res)
!   ! Evaluates the total HF SCF energy (including nuclear)
!     res :: REAL
!     .SCF_data.output = FALSE
!     .delete_SCF_integrals
!     .HF
!     res = .SCF_electronic_energy(.density_mx)
!     .SCF_data.output = TRUE
!   end
!
!   the_e2_energy result (res)
!   ! Evaluates the total HF SCF energy (including nuclear)
!     res :: REAL
!   ! .SCF_data.output = FALSE
!   ! .delete_SCF_integrals
!   ! .HF
!     .delete_SCF_integrals
!     .make_fock_mx(core=FALSE)
!     res = .SCF_electronic_energy(.density_mx,core=FALSE)
!   ! .SCF_data.output = TRUE
!   end
!
!   HF_energy_gradient(p) result (g) :: selfless
!   !  Evaluate the HF SCF gradient at geometry "g" and return the
!   !  answer as a vector "res"
!       p :: VEC{REAL}
!       g :: VEC{REAL}(size(p))
!      i,k :: INT
!      self :: MOLECULE@
!   ENSURE(.n_atom*3==p.dim,"wrong size for p")
!      .set_from_saved_self
!      if (NOT .atom.coordinate_vector.same_as(p,tol=TOL(9))) then
!        .atom.set_positions_from_vector(p)
!        .delete_SCF_integrals
!        .HF
!      end
!      g = ZERO
!      .add_S_gradient_term(g)
!      .add_T_gradient_term(g)
!      .add_NA_gradient_term(g)
!      .add_2e_gradient_term(g)
!   end
!
!   HF_energy_gradient
!   ! Calculate the Hartree-Fock SCF energy gradient.
!      g :: VEC{REAL}@
!      P :: MAT{REAL}@
!      delta, val :: REAL
!   ENSURE(.density_mx.r.allocated,"no density matrix")
!      stdin.read(delta)
!      g.create(3*.n_atom)
!      g = ZERO
!      .add_S_gradient_term(g)
!      .make_fd_gradient(S_energy,delta)
!      .add_T_gradient_term(g)
!      .make_fd_gradient(T_energy,delta)
!      .add_NA_gradient_term(g)
!      .make_fd_gradient(NA_energy,delta)
!      .add_2e_gradient_term(g)
!      .make_fd_gradient(e2_energy,delta)
!      .make_fd_gradient(the_e2_energy,delta)
!      stdout.text("Energy gradient:")
!      stdout.put(g)
!      val = .e2_energy
!      stdout.show("e2_energy     =",val)
!      val = .the_e2_energy
!      stdout.show("the_e2_energy =",val)
!      g.destroy
!      .make_fd_gradient(the_HF_energy,delta)
!   end
!
!   energy_gradient(gradient)
!   ! Calculate the Hartree-Fock SCF energy gradient.
!       gradient :: VEC{REAL}
!       g :: VEC{REAL}@
!       P :: MAT{REAL}@
!   ENSURE(.density_mx.r.allocated,"no density matrix")
!      g.create(3*.n_atom)
!      g = ZERO
!      .add_S_gradient_term(g)
!      .add_T_gradient_term(g)
!      .make_fd_gradient(T_energy)
!      .add_NA_gradient_term(g)
!      .make_fd_gradient(NA_energy)
!      .add_2e_gradient_term(g)
!      .make_fd_gradient(e2_energy)
!      gradient = gradient + g
!      g.destroy
!   end
!
!   make_e_weighted_density_mx(D) :: private
!   ! Make the eigenvalue weighted density matrix used in the overlap
!   ! derivative energy gradient term
!     D :: MAT{REAL}
!     X,MO :: MAT{REAL}@
!     eigenvalues :: VEC{REAL}@
!     eig_archive,MO_archive :: ARCHIVE
!     eig_archive.set(.name,"orbital_energies",genre="restricted")
!     MO_archive.set(.name,"molecular_orbitals",genre="restricted")
!   ENSURE(eig_archive.exists,"no eigenvalues")
!   ENSURE(MO_archive.exists,"no molecular orbitals")
!     X.create(.n_bf,.n_bf)
!     MO.create(.n_bf,.n_bf)
!     eigenvalues.create(.n_bf)
!     MO_archive.read(MO)
!     eig_archive.read(eigenvalues)
!     eigenvalues = TWO*eigenvalues
!     eigenvalues(.n_e/2+1:) = ZERO
!     X.to_product_with_diagonal(MO,eigenvalues)
!     D.to_product_of(X,MO,transpose_b=TRUE)
!     eigenvalues.destroy
!     MO.destroy
!     X.destroy
!   end
!
!   add_S_gradient_term(gradient) :: private
!   ! Make the overlap derivative contribution to the SCF gradient by contracting
!   ! The overlap derivatives with the densitu matrix.
!     gradient :: VEC{REAL}
!     g :: VEC{REAL}@
!     SAx,SAy,SAz, D :: MAT{REAL}@
!     q,fa,la,fb,lb,atom_a,atom_b, ax,ay,az,bx,by,bz :: INT
!     val_x,val_y,val_z :: REAL
!     sh :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.allocated,  "no atom list")
!     g.create(3*.n_atom); g = ZERO
!     D.create(.n_bf,.n_bf)
!     .make_e_weighted_density_mx(D) ! includes factor of 2
!     do q=1,.n_shell_pairs
!       .get_shell_pair(sh,q,fa,la,fb,lb,atom_a,atom_b)
!       if (atom_a/=atom_b) then
!       SAx.create(sh.a.n_comp,sh.b.n_comp)
!       SAy.create(sh.a.n_comp,sh.b.n_comp)
!       SAz.create(sh.a.n_comp,sh.b.n_comp)
!       sh.make_S_1st_deriv_ints(SAx,SAy,SAz)
!       val_x = SAx.trace_product_with(D(fb:lb,fa:la))
!       val_y = SAy.trace_product_with(D(fb:lb,fa:la))
!       val_z = SAz.trace_product_with(D(fb:lb,fa:la))
!       SAz.destroy; SAy.destroy; SAx.destroy
!       ax = 3*atom_a - 2; ay = 3*atom_a - 1; az = 3*atom_a
!       bx = 3*atom_b - 2; by = 3*atom_b - 1; bz = 3*atom_b
!       g(ax) = g(ax) + val_x ; g(ay) = g(ay) + val_y ; g(az) = g(az) + val_z
!       g(bx) = g(bx) - val_x ; g(by) = g(by) - val_y ; g(bz) = g(bz) - val_z
!       end
!       sh.destroy_ptr_part
!     end
!     D.destroy
!     stdout.flush
!     stdout.text("S gradient term:")
!     stdout.put(-TWO*g)
!     gradient = gradient - TWO*g ! factor TWO for upper half of shell pairs
!     g.destroy
!   end
!
!   add_T_gradient_term(gradient) :: private
!   ! Add the kinetic derivative contribution to the SCF "gradient" by contracting
!   ! The kinetic integral derivatives with the densitu matrix.
!     gradient :: VEC{REAL}
!     g :: VEC{REAL}@
!     TAx,TAy,TAz, P :: MAT{REAL}@
!     q,fa,la,fb,lb,atom_a,atom_b, ax,ay,az,bx,by,bz :: INT
!     val_x,val_y,val_z :: REAL
!     arch :: ARCHIVE
!     sh :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.allocated,  "no atom list")
!     arch.set(.name,"density_mx",genre="restricted")
!   ENSURE(arch.exists,"no density")
!     g.create(3*.n_atom); g = ZERO
!     P.create(.n_bf,.n_bf)
!     arch.read(P)
!     do q=1,.n_shell_pairs
!       .get_shell_pair(sh,q,fa,la,fb,lb,atom_a,atom_b)
!       if (atom_a/=atom_b) then
!       TAx.create(sh.a.n_comp,sh.b.n_comp)
!       TAy.create(sh.a.n_comp,sh.b.n_comp)
!       TAz.create(sh.a.n_comp,sh.b.n_comp)
!       sh.make_T_1st_deriv_ints(TAx,TAy,TAz)
!       val_x = TAx.trace_product_with(P(fb:lb,fa:la))
!       val_y = TAy.trace_product_with(P(fb:lb,fa:la))
!       val_z = TAz.trace_product_with(P(fb:lb,fa:la))
!       TAz.destroy; TAy.destroy; TAx.destroy
!       ax = 3*atom_a - 2; ay = 3*atom_a - 1; az = 3*atom_a
!       bx = 3*atom_b - 2; by = 3*atom_b - 1; bz = 3*atom_b
!       g(ax) = g(ax) + val_x ; g(ay) = g(ay) + val_y ; g(az) = g(az) + val_z
!       g(bx) = g(bx) - val_x ; g(by) = g(by) - val_y ; g(bz) = g(bz) - val_z
!       end
!       sh.destroy_ptr_part
!     end
!     P.destroy
!     stdout.flush
!     stdout.text("T gradient:")
!     stdout.put(TWO*g)
!     gradient = gradient + TWO*g ! factor for upper half of shell pairs
!     g.destroy
!   end
!
!   add_NA_gradient_term(gradient) :: private
!   ! Add the nuclear attraction derivative contribution to the SCF "gradient" by contracting
!   ! the nuclear attraction integral derivatives with the densitu matrix. This term includes
!   ! the Helmann-Feynman contribution.
!     gradient :: VEC{REAL}
!     g :: VEC{REAL}@
!     NAx,NAy,NAz, NBx,NBy,NBz, P :: MAT{REAL}@
!     q,fa,la,fb,lb,atom_a,atom_b,c, ax,ay,az,bx,by,bz,cx,cy,cz :: INT
!     val_ax,val_ay,val_az, val_bx,val_by,val_bz, val_cx,val_cy,val_cz, Z, fac :: REAL
!     arch :: ARCHIVE
!     sh :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.allocated,  "no atom list")
!     arch.set(.name,"density_mx",genre="restricted")
!   ENSURE(arch.exists,"no density")
!     g.create(3*.n_atom); g = ZERO
!     P.create(.n_bf,.n_bf)
!     arch.read(P)
!     do q=1,.n_shell_pairs
!       .get_shell_pair(sh,q,fa,la,fb,lb,atom_a,atom_b)
!       NAx.create(sh.a.n_comp,sh.b.n_comp)
!       NAy.create(sh.a.n_comp,sh.b.n_comp)
!       NAz.create(sh.a.n_comp,sh.b.n_comp)
!       NBx.create(sh.a.n_comp,sh.b.n_comp)
!       NBy.create(sh.a.n_comp,sh.b.n_comp)
!       NBz.create(sh.a.n_comp,sh.b.n_comp)
!       fac = TWO; if (fa==fb) fac = ONE
!       do c=1,.n_atom
!         if (atom_a==c AND atom_b==c) cycle
!         sh.make_NA_1st_deriv_ints(NAx,NAy,NAz,NBx,NBy,NBz,.atom(c).position)
!         Z = - .atom(c).atomic_number * fac ! for upper half of shell pair
!         val_ax = Z * NAx.trace_product_with(P(fb:lb,fa:la))
!         val_ay = Z * NAy.trace_product_with(P(fb:lb,fa:la))
!         val_az = Z * NAz.trace_product_with(P(fb:lb,fa:la))
!         val_bx = Z * NBx.trace_product_with(P(fb:lb,fa:la))
!         val_by = Z * NBy.trace_product_with(P(fb:lb,fa:la))
!         val_bz = Z * NBz.trace_product_with(P(fb:lb,fa:la))
!         ax = 3*atom_a - 2; ay = 3*atom_a - 1; az = 3*atom_a
!         bx = 3*atom_b - 2; by = 3*atom_b - 1; bz = 3*atom_b
!         cx = 3*c      - 2; cy = 3*c      - 1; cz = 3*c
!         val_cx = -(val_ax + val_bx)
!         val_cy = -(val_ay + val_by)
!         val_cz = -(val_az + val_bz)
!         g(ax) = g(ax) + val_ax ; g(ay) = g(ay) + val_ay ; g(az) = g(az) + val_az
!         g(bx) = g(bx) + val_bx ; g(by) = g(by) + val_by ; g(bz) = g(bz) + val_bz
!         g(cx) = g(cx) + val_cx ; g(cy) = g(cy) + val_cy ; g(cz) = g(cz) + val_cz
!       end
!       NBz.destroy; NBy.destroy; NBx.destroy
!       NAz.destroy; NAy.destroy; NAx.destroy
!       sh.destroy_ptr_part
!     end
!     P.destroy
!     stdout.flush
!     stdout.text("NA gradient:")
!     stdout.put(g)
!     gradient = gradient + g
!     g.destroy
!   end
!
!   add_2e_gradient_term(gradient) :: private
!   ! Add the two electron derivative contributions to the "gradient".
!     gradient :: VEC{REAL}
!     g :: VEC{REAL}@
!     P :: MAT{REAL}@
!     max_I,max_P :: VEC{REAL}@
!     sh :: SHELL4
!     AA,BB,CC :: MAT5{REAL}@
!     ab,cd, sa,sb,sc,sd, atom_a,atom_b,atom_c,atom_d :: INT
!     a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld :: INT
!     ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz :: INT
!     fac,P_dc,P_db,P_cb,P_ba,P_da,P_ca,cutoff,dens,P_max,IP_max :: REAL
!     AAx,AAy,AAz,BBx,BBy,BBz,CCx,CCy,CCz,DDx,DDy,DDz,SSx,SSy,SSz :: REAL
!     skip :: BIN
!     ab_same,cd_same,ac_same :: BIN
!     arch :: ARCHIVE
!     arch.set(.name,"density_mx",genre="restricted")
!   ENSURE(arch.exists,"no density")
!     g.create(3*.n_atom); g = ZERO
!     P.create(.n_bf,.n_bf)
!     arch.read(P)
!     cutoff = .SCF_data.eri_schwarz_cutoff
!     max_I.create(.n_shell_pairs)
!     max_P.create(.n_shell_pairs)
!     .make_max_abab_integrals(max_I)
!     .make_max_density_elements(max_P,P)
!     P_max = maxval(max_P)
!     IP_max = maxval(max_I) * P_max
!     do ab = 1, .n_shell_pairs
!       if (max_I(ab)*IP_max < cutoff)  cycle                         ! Rough Schwarz test, but quick.
!       .get_shell_pair_indices(ab,sa,sb,fa,la,fb,lb,atom_a,atom_b)   ! a & b shell indices.
!       ax = 3*atom_a-2; ay = 3*atom_a-1; az = 3*atom_a
!       bx = 3*atom_b-2; by = 3*atom_b-1; bz = 3*atom_b
!       ab_same = atom_a==atom_b
!       .set_shell_quartet_ab(sh,sa,sb)
!       do cd = 1,ab
!         if (max_I(ab)*max_I(cd)*P_max < cutoff)  cycle              ! Rough Schwarz test
!         .get_shell_pair_indices(cd,sc,sd,fc,lc,fd,ld,atom_c,atom_d) ! c & d shell indices.
!         cx = 3*atom_c-2; cy = 3*atom_c-1; cz = 3*atom_c
!         dx = 3*atom_d-2; dy = 3*atom_d-1; dz = 3*atom_d
!         cd_same = atom_c==atom_d
!         ac_same = atom_a==atom_c
!         if (ab_same AND cd_same AND ac_same) cycle ! by translational invariance
!         ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
!         skip = .schwarz_inequality_test(cutoff,ab,cd,sa,sb,sc,sd,max_P,max_I)
!         if (skip) cycle
!         .set_shell_quartet_cd(sh,sc,sd)
!         ! Evaluate the integrals' coincidence factors
!         fac = ONE
!         if (sa==sb)            fac = HALF * fac
!         if (sc==sd)            fac = HALF * fac
!         if (sa==sc AND sb==sd) fac = HALF * fac
!         AA.create(fa,la,fb,lb,fc,lc,fd,ld,1,3)
!         BB.create(fa,la,fb,lb,fc,lc,fd,ld,1,3)
!         CC.create(fa,la,fb,lb,fc,lc,fd,ld,1,3)
!         sh.make_ERI_derivatives(AA,BB,CC)
!         AA = fac*AA; BB = fac*BB; CC = fac*CC
!         do d = fd,ld
!         do c = fc,lc
!           P_dc = P(d,c)
!           do b = fb,lb
!             P_db = P(d,b)
!             P_cb = P(c,b)
!             do a = fa,la
!                P_ba = P(b,a)
!                P_da = P(d,a)
!                P_ca = P(c,a)
!                dens = FOUR*P_ba*P_dc - P_ca*P_db - P_da*P_cb
!                AAx = AA(a,b,c,d,1)*dens; AAy = AA(a,b,c,d,2)*dens; AAz = AA(a,b,c,d,3)*dens
!                BBx = BB(a,b,c,d,1)*dens; BBy = BB(a,b,c,d,2)*dens; BBz = BB(a,b,c,d,3)*dens
!                CCx = CC(a,b,c,d,1)*dens; CCy = CC(a,b,c,d,2)*dens; CCz = CC(a,b,c,d,3)*dens
!                DDx = -(AAx + BBx + CCx)
!                DDy = -(AAy + BBy + CCy)
!                DDz = -(AAz + BBz + CCz)
!                g(ax) = g(ax) + AAx; g(ay) = g(ay) + AAy; g(az) = g(az) + AAz
!                g(bx) = g(bx) + BBx; g(by) = g(by) + BBy; g(bz) = g(bz) + BBz
!                g(cx) = g(cx) + CCx; g(cy) = g(cy) + CCy; g(cz) = g(cz) + CCz
!                g(dx) = g(dx) + DDx; g(dy) = g(dy) + DDy; g(dz) = g(dz) + DDz
!             end
!           end
!         end
!         end
!         CC.destroy
!         BB.destroy
!         AA.destroy
!         sh.destroy_cd
!       end
!       sh.destroy_ab
!     end
!     max_P.destroy
!     max_I.destroy
!     P.destroy
!     stdout.flush
!     stdout.text("2e gradient:")
!     stdout.put(g)
!     gradient = gradient + g
!     g.destroy
!   end
!
!   make_fd_gradient(func,delta) :: private
!   ! Make the finite difference gradient "g" of the value of function "func"
!   ! with respect to nuclear perturbations
!      interface
!         func(self) result (res)
!            self :: MOLECULE
!            res :: REAL
!         end
!      end
!      delta :: REAL, optional
!      g,p :: VEC{REAL}@
!      del,e_p,e_m :: REAL
!      a,i,ai :: INT
!      del = TOL(7)
!      if (present(delta)) del = delta
!      g.create(3*.n_atom)
!      p.create(3*.n_atom)
!      .atom.put_geometry_vector_to(p)
!      do a = 1,.n_atom
!      do i = 1,3
!         ai = 3*(a-1)+i
!         p(ai) = p(ai) + del
!         .atom.set_positions_from_vector(p)
!         .delete_SCF_integrals
!         e_p   = func(self)
!         p(ai) = p(ai) - TWO*del
!         .atom.set_positions_from_vector(p)
!         .delete_SCF_integrals
!         e_m   = func(self)
!         g(ai) = (e_p - e_m)/(TWO*del)
!         p(ai) = p(ai) + del
!    ! stdout.show("   ai =",ai)
!    ! stdout.show("   ep =",e_p)
!    ! stdout.show("   em =",e_m)
!         ! put back geometry
!         .atom.set_positions_from_vector(p)
!      end
!      end
!      p.destroy
!      stdout.flush
!      stdout.text("Finite difference gradient:")
!      stdout.put(g)
!      g.destroy
!   end
!
!   S_energy result (res)
!   ! Make the energy weighted S energy
!     res :: REAL
!     D :: MAT{REAL}@
!     D.create(.n_bf,.n_bf)
!     .make_e_weighted_density_mx(D) ! includes factor of 2
!     .make_overlap_mx
!     res = - .overlap_mx.trace_product_with(D)
!     D.destroy
!   end
!
!   T_energy result (res)
!   ! Make the kinetic energy
!     res :: REAL
!     T :: MAT{REAL}@
!     T.create(.n_bf,.n_bf)
!     .get_kinetic_energy_mx(T)
!     res = .expectation(T)
!     T.destroy
!   end
!
!   NA_energy result (res)
!   ! Make the electron-nuclear attraction energy
!     res :: REAL
!     Z :: MAT{REAL}@
!     Z.create(.n_bf,.n_bf)
!     .get_attraction_nuclear_mx(Z)
!     res = .expectation(Z)
!     Z.destroy
!   end

!  ==================
!  Non orthogonal SCF
!  ==================

!   optimize_orbitals
!   ! Optimise the Hartree-Fock orbitals directly using Fletcher Powell
!      self :: PTR
!
!      energy :: REAL
!      MO :: VEC{REAL}@
!
!      .get_initial_guess
!
!      .MOs.r.shrink(.n_bf,.n_a)
!      MO.create(.n_bf*.n_a)
!      MO = reshape(.MOs.r,(/.n_bf*.n_a/))
!
!      .set_saved_self
!      VEC{REAL}:minimize_BFGS(::the_r_HF_energy,::the_r_HF_energy_MO_gradient,MO,energy,TOL(3),TOL(3),step=TOL(2))
!    ! call minimize_FRPR_(the_r_HF_energy,the_r_HF_energy_MO_gradient,MO,energy,TOL(3),TOL(3),step=TOL(2))
!
!      stdout.show("Optimised energy =",energy)
!      stdout.text("Optimised orbitals:")
!      stdout.put(MO)
!
!      MO.destroy
!
!   end
!
!   the_r_HF_energy(MO) result (res) :: selfless
!   ! Make the restricted Hartree-Fock energy from "MO", a vector representation
!   ! of the occupied molecular orbitals. Includes nuclear contribution.
!      MO :: VEC{REAL}, IN
!      res :: REAL
!
!      self :: MOLECULE@
!
!      ! Recover state
!      .set_from_saved_self
!
!      ! Archive MO's
!      .MOs.r = reshape(MO,(/.n_bf,.n_a/))
!      .put_archive(.MOs,"molecular_orbitals")
!
!      ! writes density & occupations to disk
!      .make_non_ortho_SCF_density
!
!      ! Fock matrix
!      .make_fock_mx
!
!      ! Energy
!      res = .SCF_energy
!
!      stdout.show("hf energy =",res)
!
!   end
!
!   r_hf_energy(MO) result (res)
!   ! Make the Hartree-Fock energy without any intermediates.
!   ! Includes nuclear contribution.
!      MO :: MAT{REAL}
!      res :: REAL
!
!      P,N :: MAT{REAL}@
!
!      ! Create
!      P.create(.n_bf,.n_bf)
!      N.create(.n_a ,.n_a)
!
!      ! Writes density to disk
!      .make_r_non_ortho_SCF_density(P,MO,N)
!
!      ! Energy
!      res = .e1_energy(P) + .e2_energy(P) + .nuclear_repulsion_energy
!
!      ! Clean
!      N.destroy
!      P.destroy
!
!      stdout.show("hf energy =",res)
!
!   end
!
!   e1_energy(P) result (res)
!   ! Make the one-electron energy directly from the density matrix "P"
!   ! without constructing any intermediates
!      P   :: MAT{REAL}, target
!      res :: REAL
!
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.allocated,  "no atom list")
!   ENSURE(P.dim1==.n_bf,"wrong size, P")
!   ENSURE(P.is_square,"wrong shape, P")
!
!      T,Pba :: MAT{REAL}@
!      q,fa,la,na,fb,lb,nb, c :: INT
!      fac,val :: REAL
!      sh :: SHELL2
!
!      res = ZERO
!
!      do q = 1,.n_shell_pairs
!
!         ! Shell limits
!         .set_shell2_from(q,sh,fa,la,na,fb,lb,nb)
!
!         ! Density block
!         Pba => P(fb:lb,fa:la)
!
!         ! Density-block weighting
!         fac = TWO
!         if (fa==fb) fac = ONE
!
!         ! Kinetic energy
!         T.create(fa,la,fb,lb)
!         sh.get_kei(T)
!         val = T.trace_product_with(Pba)
!
!         ! Electron-nuclear attraction
!         do c = 1,.n_atom
!            sh.get_nuc(T,.atom(c).mass,.atom(c).position)
!            val = val - .atom(c).nuclear_charge*T.trace_product_with(Pba)
!         end
!
!         ! Add
!         res = res + fac*val
!
!         ! Clean
!         T.destroy
!         sh.destroy_ptr_part
!
!      end
!
!   end
!
!   e2_energy(P) result (res)
!   ! Make the two electron electrostatic energy directly from the density
!   ! matrix "P" without constructing any intermediates.
!      res :: REAL
!      P :: MAT{REAL}
!
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.allocated,  "no atom list")
!   ENSURE(P.dim1==.n_bf,"wrong size, P")
!   ENSURE(P.is_square,"wrong shape, P")
!
!      max_I,max_P :: VEC{REAL}@
!      I :: MAT4{REAL}@
!      sh :: SHELL4
!      ab,cd, sa,sb,sc,sd, atom_a,atom_b,atom_c,atom_d :: INT
!      a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld :: INT
!      fac,P_dc,P_db,P_cb,cutoff,dens,P_max,IP_max :: REAL
!      skip :: BIN
!
!      ! Schwarz-inequality test cutoff
!      cutoff = .SCF_data.eri_schwarz_cutoff
!
!      ! Shell-pair-maximum integrals, density
!      max_I.create(.n_shell_pairs)
!      max_P.create(.n_shell_pairs)
!      .make_max_abab_integrals(max_I)
!      .make_max_density_elements(max_P,P)
!      P_max  = maxval(max_P)
!      IP_max = maxval(max_I) * P_max
!
!      res = ZERO
!
!      do ab = 1, .n_shell_pairs
!
!         ! Rough Schwarz test, but quick.
!         if (max_I(ab)*IP_max < cutoff)  cycle
!
!         ! a & b shell indices.
!         .get_shell_pair_indices(ab,sa,sb,fa,la,fb,lb,atom_a,atom_b)
!         .copy_shell_quartet_ab(sh,sa,sb)
!
!         do cd = 1,ab
!
!            ! Rough Schwarz test
!            if (max_I(ab)*max_I(cd)*P_max < cutoff) cycle
!
!            ! c & d shell indices.
!            .get_shell_pair_indices(cd,sc,sd,fc,lc,fd,ld,atom_c,atom_d)
!
!            ! (ab|cd) < sqrt(ab|ab) * sqrt(cd|cd) test.
!            skip = .schwarz_inequality_test(cutoff,ab,cd,sa,sb,sc,sd,max_P,max_I)
!            if (skip) cycle
!
!            .copy_shell_quartet_cd(sh,sc,sd)
!
!            ! Evaluate the integrals' coincidence factors
!            fac = ONE
!            if (sa==sb)            fac = HALF * fac
!            if (sc==sd)            fac = HALF * fac
!            if (sa==sc AND sb==sd) fac = HALF * fac
!
!            I.create(fa,la,fb,lb,fc,lc,fd,ld)
!            sh.get_ERI(I)
!
!            do d = fd,ld
!            do c = fc,lc
!               P_dc = P(d,c)
!               do b = fb,lb
!                  P_db = P(d,b)
!                  P_cb = P(c,b)
!                  do a = fa,la
!                     dens = FOUR*P(b,a)*P_dc - P(c,a)*P_db - P(d,a)*P_cb
!                     res  = res + fac * I(a,b,c,d) * dens
!                  end
!               end
!               end
!            end
!
!            I.destroy
!            sh.destroy_cd
!
!         end
!
!         sh.destroy_ab
!
!      end
!
!      max_P.destroy
!      max_I.destroy
!
!   end
!
!   the_r_HF_energy_MO_gradient(MO) result (g) :: selfless
!   ! Evaluate the gradient of the restricted Hartree-Fock energy with
!   ! respect to the molecular orbitals, (F - SDF) MO N, where "MO" is
!   ! the matrix of the occupied molecular orbital coefficients.
!      MO :: VEC{REAL}, IN
!      g  :: VEC{REAL}(MO.dim)
!      self :: MOLECULE@
!      .set_from_saved_self
!      g = .r_HF_energy_MO_gradient(reshape(MO,[.n_bf,.n_a]),make_fock=FALSE)
!      ! The fock matrix is not made because in the minimiser the gradient is
!      ! only ever called after the energy routine. This saves a fock build.
!   end
!
!   r_HF_energy_MO_gradient(MO,make_fock) result (g)
!   ! Evaluate the gradient of the restricted Hartree-Fock energy with
!   ! respect to the molecular orbitals, 2(2F - SPF) MO N, where "MO" is
!   ! the matrix of the occupied molecular orbital coefficients.
!   ! If "make_fock" is present and FALSE, the molecular_orbitals are defined
!   ! from "MO", the MO's are archived, and the updated density matrix and
!   ! fock matrix are not made but assumed to be pre-existing.
!      MO :: MAT{REAL}, IN
!      make_fock :: BIN, optional
!
!   ENSURE(.SCF_data.using_MO_gradient_update,"not allowed")
!   ENSURE(MO.dim2==.n_a,"wrong dimension 2, MO")
!
!      g :: VEC{REAL}(.n_bf*.n_a)
!      do_fock :: BIN
!      W,X,N,S,g1 :: MAT{REAL}@
!      norm :: REAL
!      archive :: ARCHIVE
!
!      do_fock = TRUE
!      if (present(make_fock)) do_fock = make_fock
!
!      if (do_fock) then ! this assumes that a previous energy was called
!        .MOs.r = MO
!        .put_archive(.MOs,"molecular_orbitals")
!        .make_non_ortho_SCF_density ! writes density & occupations to disk
!        .make_fock_mx
!      end
!
!      g1.create(.n_bf,.n_a)
!      X.create(.n_bf,.n_bf)
!      N.create(.n_a ,.n_a )
!      S.create(.n_bf,.n_bf)
!      W.create(.n_bf,.n_bf)
!      .make_overlap_mx
!
!      W.to_product_of(.overlap_mx,.density_mx.r)
!      X = -TWO*.fock_mx.r
!      X.plus_product_of(W,.fock_mx.r)
!      W.destroy
!      X = -X
!      S.create(.n_bf,.n_a)
!
!      archive.set(.name,"non_ortho_occupation_mx",genre="restricted")
!      archive.read(N)
!
!      S.to_product_of(MO,N)
!      N.destroy
!      g1.to_scaled_product_of(X,S,fac=TWO)
!      S.destroy
!      X.destroy
!
!      norm = g1.trace_product_with(transpose(g1))
!      stdout.show("hf energy gradient =",norm)
!      g = reshape(g1,(/size(g)/))
!      g1.destroy
!
!   end
!
!   make_non_ortho_SCF_density :: leaky
!   ! Make the non orthogonal density matrix from the molecular orbitals.
!   ! NOTE: the final computed density matrix is written to an archive.
!   ! NOTE: if any old density matrix exists, it is saved in an old archive.
!   ! NOTE: the non orthogonal occupation matrix i.e. the back tranform of
!   ! the inverse of the MO overlap matrix is written to an archive.
!
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.MOs.allocated, "no molecular orbitals")
!
!      N :: MAT{REAL}@
!      orb_kind :: STR
!      archive :: ARCHIVE
!
!      ! Determine the kind of density matrix to be made
!      if (.SCF_data.deallocated) then; orb_kind = .MOs.spinorbital_genre
!      else;                           orb_kind = .SCF_data.spinorbital_genre
!      end
!
!      ! Create space for the right kind of density matrix, or save old density matrix
!      if (NOT .density_mx.is_allocated_with_genre(orb_kind)) then
!         .density_mx.create(orb_kind)
!      else
!         .old_density_mx.destroy
!         .old_density_mx.create_copy(.density_mx)
!      end
!
!      ! Now determine the kind of SCF (if any) associated with the density matrix
!      if (.SCF_data.deallocated) then; orb_kind = .MOs.guess_SCF_kind
!      else;                           orb_kind = .SCF_data.SCF_kind
!      end
!
!      archive.set(.name,"non_ortho_occupation_mx")
!
!      select case (orb_kind)
!
!         case ("rhf","restricted_hartree_fock","xray_rhf","noninteracting-group-rhf")
!
!            ENSURE(.MOs.is_allocated_with_genre("restricted"),"no MO's")
!            ENSURE(.spin_multiplicity==1,"this is not a singlet state")
!
!            N.create(.n_a,.n_a)
!            .make_r_non_ortho_SCF_density(.density_mx.r,.MOs.r,N)
!
!
!            archive.set_genre("restricted")
!            archive.write(N)
!            N.destroy
!
!         case default
!            DIE("unknown SCF kind, "//trim(orb_kind))
!
!      end
!
!      .put_archive(.density_mx,"density_mx")
!
!   end
!
!   make_r_non_ortho_SCF_density(P,MO,N) :: leaky
!   ! Make the restricted non-orthogonal density matrix "P" from the molecular
!   ! orbitals "MO". The cofactor/occupation matrix "N" is also returned if
!   ! required. This density includes a factor of two.
!      P,MO,N :: MAT{REAL}
!
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(P.is_square,"wrong size, P")
!   ENSURE(N.is_square,"wrong size, N")
!   ENSURE(P.dim1==.n_bf,"wrong size, P")
!   ENSURE(N.dim1==.n_a ,"wrong size, N")
!   ENSURE(.spin_multiplicity==1,"this is not a singlet state")
!   ENSURE(MO.dim2==.n_a,"wrong size, N")
!
!      .make_overlap_mx
!      .overlap_mx.change_basis_to(N,MO)
!    ! stdout.text("N^-1:")
!    ! stdout.put(N)
!      N.to_inverse_of(N)
!    ! stdout.text("N:")
!    ! stdout.put(N)
!      N.back_transform_to(P,MO)
!      P = TWO*P
!
!   end

!   make_r_fock_mx(F,P,direct,core,r12,test)
!   ! Make a new restricted Fock matrix "F" from the density matrix "P".
!   ! If present and TRUE , "direct" means calculate integrals on the fly
!   ! If present and FALSE, "core" removes the core matrix contribution
!   ! If present and FALSE, "r12"  removes the two electron contribution
!     direct,core,r12,test :: BIN, optional
!     P,F :: MAT{REAL}
!     J,K :: MAT{REAL}@
!     do_direct,add_core,add_r12,do_test :: BIN
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(F.dim1==.n_bf,"wrong shape, F")
!   ENSURE(P.dim1==.n_bf,"wrong shape, P")
!   ENSURE(F.is_square,"wrong shape, P")
!   ENSURE(P.is_square,"wrong shape, P")
!     do_direct= FALSE
!     do_test= FALSE
!     add_core = TRUE
!     add_r12  = TRUE
!     if (present(direct)) do_direct = direct
!     if (present(test))   do_test  = test
!     if (present(core))   add_core = core
!     if (present(r12))    add_r12  = r12
!     if (add_r12) then
!        J.create(.n_bf,.n_bf)
!        K.create(.n_bf,.n_bf)
!        if (do_direct) then; .make_r_JK_direct(J,K,P)
!        else;                .make_r_JK_disk(J,K,P)
!        end
!        F = J - HALF*K
!        K.destroy
!        J.destroy
!     else
!        F = ZERO
!     end
!     if (add_core) .add_core_hamiltonian(F)
!   end

!  =============
!  B field stuff
!  =============

!   make_pcc_b_field
!   !
!      n_pt,i,j,n, k :: INT
!      aa,bb,cc :: REAL
!      pt :: MAT{REAL}@
!      dma :: MAT3{CPX}@
!      re,im :: REAL
!      n_pt = 59*75
!    ! n_pt = 25
!      pt.create(n_pt,3)
!      aa=7.4/sqrt(2.0)
!    ! aa=0.4/sqrt(2.0)
!      bb=0.2/sqrt(2.0)
!      n = 0
!      do i=1,75
!         cc=5.4
!    !    cc=0.6
!         do j=1,59
!            n = n+1
!            cc = cc - 0.2
!            pt(n,1)= aa
!            pt(n,2)=-aa
!            pt(n,3)= cc
!         end
!         aa = aa-bb
!      end
!      dma.create(160,160,4)
!      open(unit=99,file='opdens.c',form='formatted',status='old')
!      do i=1,160
!      do j=1,160
!         do k=1,4
!            read(99,*) re,im
!            dma(i,j,k) = cmplx(re,im,kind=CPX_KIND)
!         enddo
!      enddo
!      enddo
!      close(99)
!      .density_mx.destroy("general_complex")
!      .density_mx.create(160,"general_complex")
!      .density_mx.gc.set_aa_block_to(dma(:,:,1))
!      .density_mx.gc.set_ab_block_to(dma(:,:,2))
!      .density_mx.gc.set_ba_block_to(dma(:,:,3))
!      .density_mx.gc.set_bb_block_to(dma(:,:,4))
!      dma.destroy
!      .make_b_field_grid(pt)
!      pt.destroy
!   end
!
!   make_b_field_grid(pt)
!   ! Make the total b_field "b" on ".plot_grid"
!      pt :: MAT{REAL}
!      archive :: ARCHIVE
!      b,w :: MAT{REAL}@
!      complex :: BIN
!      n_pt :: INT
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .density_mx.allocated, "no density matrix")
!      complex = .density_mx.spinorbital_genre == "general_complex"
!   ENSURE(complex, "no general complex density matrix")
!      .make_spin_b_field_grid(pt)
!      .make_current_b_field_grid(pt)
!      n_pt = size(pt,1)
!      b.create(n_pt,3)
!      w.create(n_pt,3)
!      archive.set(.name,"spin_b_field_density_grid")
!      archive.read(b)
!      b = 0
!      archive.set(.name,"current_b_field_density_grid")
!      archive.read(w)
!      b = b + w
!      w.destroy
!      archive.set(.name,"b_field_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1)
!      archive.set(.name,"b_fie7d_norm_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, norm=TRUE)
!      archive.set(.name,"b_field_density_grid,normalized,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, normalize=TRUE)
!      b.destroy
!   end
!
!   make_spin_b_field_grid(pt)
!   ! Make the b_field due to the spin density on ".plot_grid"
!      pt :: MAT{REAL}
!      archive :: ARCHIVE
!       b :: MAT{REAL}@
!      complex :: BIN
!      n_pt :: INT
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .density_mx.allocated, "no density matrix")
!      complex = .density_mx.spinorbital_genre == "general_complex"
!   ENSURE(complex, "no general complex density matrix")
!      n_pt = size(pt,1)
!      b.create(n_pt,3)
!      .make_spin_b_field_grid(b,.density_mx.gc,pt)
!      archive.set(.name,"spin_b_field_density_grid")
!      archive.write(b)
!      archive.set(.name,"spin_b_field_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1)
!      archive.set(.name,"spin_b_field_norm_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, norm=TRUE)
!      archive.set(.name,"spin_b_field_density_grid,normalized,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, normalize=TRUE)
!      b.destroy
!   end
!
!   make_spin_b_field_grid(bb,dens,pt)
!   ! Make the b_field "bb" due to the spin density on ".plot_grid" using complex general
!   ! AO density matrix "dens"
!      bb,pt :: MAT{REAL}
!      dens :: MAT{CPX}, target
!      Daa,Dba,Dbb :: MAT{CPX}@
!      Mxx,Mxy,Mxz :: MAT{REAL}@
!      Myx,Myy,Myz :: MAT{REAL}@
!      Mzx,Mzy,Mzz :: MAT{REAL}@
!      q,a,b,fa,fb,la,lb,k, n_pt :: INT
!      fac :: REAL
!      shell :: SHELL2
!
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.allocated,  "no atom list")
!
!      n_pt = size(pt,1)
!      bb = ZERO
!      do q = 1,.n_shell_pairs
!        .get_shell_pair(q,shell,a,b,fa,la,fb,lb)
!        Daa => dens(fb:lb,fa:la) ! alpha-alpha block
!        fb = fb + .n_bf
!        lb = lb + .n_bf
!        Dba => dens(fb:lb,fa:la) ! beta-alpha block
!        fa = fa + .n_bf
!        la = la + .n_bf
!        Dbb => dens(fb:lb,fa:la) ! beta-beta block
!        fac = ONE; if (a==b) fac=HALF
!        Mxx.create(shell.a.n_comp,shell.b.n_comp)
!        Mxy.create(shell.a.n_comp,shell.b.n_comp)
!        Mxz.create(shell.a.n_comp,shell.b.n_comp)
!        Myx.create(shell.a.n_comp,shell.b.n_comp)
!        Myy.create(shell.a.n_comp,shell.b.n_comp)
!        Myz.create(shell.a.n_comp,shell.b.n_comp)
!        Mzx.create(shell.a.n_comp,shell.b.n_comp)
!        Mzy.create(shell.a.n_comp,shell.b.n_comp)
!        Mzz.create(shell.a.n_comp,shell.b.n_comp)
!        do k = 1,n_pt
!          shell.make_magnetic_S_ints(Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz,pt(k,:))
!          bb(k,1) = bb(k,1) + Mxx.trace_of_product( real(Dba))
!          bb(k,1) = bb(k,1) + Mxy.trace_of_product(aimag(Dba))
!          bb(k,1) = bb(k,1) + fac*Mxz.trace_of_product(real(Daa))
!          bb(k,1) = bb(k,1) - fac*Mxz.trace_of_product(real(Dbb))
!          bb(k,2) = bb(k,2) + Myx.trace_of_product( real(Dba))
!          bb(k,2) = bb(k,2) - Myy.trace_of_product(aimag(Dba))
!          bb(k,2) = bb(k,2) + fac*Myz.trace_of_product(real(Daa))
!          bb(k,2) = bb(k,2) - fac*Myz.trace_of_product(real(Dbb))
!          bb(k,3) = bb(k,3) + Mzx.trace_of_product( real(Dba))
!          bb(k,3) = bb(k,3) + Mzy.trace_of_product(aimag(Dba))
!          bb(k,3) = bb(k,3) + fac*Mzz.trace_of_product(real(Daa))
!          bb(k,3) = bb(k,3) - fac*Mzz.trace_of_product(real(Dbb))
!        end
!        Mzz.destroy; Mzy.destroy; Mzx.destroy
!        Myz.destroy; Myy.destroy; Myx.destroy
!        Mxz.destroy; Mxy.destroy; Mxx.destroy
!        shell.destroy_ptr_part
!      end
!      bb.zero_small_values(TOL(10))
!      fac = -G_FACTOR*HALF
!      bb = fac*bb
!   end
!
!   make_current_b_field_grid(pt)
!   ! Make the b_field due to the current density on ".plot_grid"
!      pt :: MAT{REAL}
!      archive :: ARCHIVE
!      b :: MAT{REAL}@
!      complex :: BIN
!      n_pt :: INT
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.density_mx.allocated, "no density matrix")
!      .make_ao_density_mx
!      complex = .density_mx.spinorbital_genre.includes("complex")
!   ENSURE(complex, "no complex density matrix")
!      n_pt = size(pt,1)
!      b.create(n_pt,3)
!      .make_current_b_field_grid(b,.density_mx.r_complex,pt)
!      archive.set(.name,"current_b_field_density_grid")
!      archive.write(b)
!      archive.set(.name,"current_b_field_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1)
!      archive.set(.name,"current_b_field_norm_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, norm=TRUE)
!      archive.set(.name,"current_b_field_density_grid,normalized,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, normalize=TRUE)
!      b.destroy
!   end
!
!   make_current_b_field_grid(bb,dens,pt)
!   ! Make the magnetic field B field "bb" due to the paramagnetic current
!   ! density at a set of points "pt" using a complex AO density matrix "dens"
!      bb,pt :: MAT{REAL}
!      dens  :: MAT{CPX}, target
!      Dba   :: MAT{CPX}*
!      Jx,Jy,Jz :: MAT{REAL}@
!      q,a,b,fa,fb,la,lb,k, n_pt :: INT
!      fac :: REAL
!      shell :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.allocated,  "no atom list")
!      n_pt = size(pt,1)
!      bb = ZERO
!      do q = 1,.n_shell_pairs
!        .get_shell_pair(q,shell,a,b,fa,la,fb,lb)
!        Dba => dens(fb:lb,fa:la)
!        fac = ONE; if (a==b) fac=HALF
!        Jx.create(shell.a.n_comp,shell.b.n_comp)
!        Jy.create(shell.a.n_comp,shell.b.n_comp)
!        Jz.create(shell.a.n_comp,shell.b.n_comp)
!        do k = 1,n_pt
!          shell.make_magnetic_jp_ints(Jx,Jy,Jz,pt(k,:))
!          bb(k,1) = bb(k,1) + Jx.trace_of_product(aimag(Dba))
!          bb(k,2) = bb(k,2) + Jy.trace_of_product(aimag(Dba))
!          bb(k,3) = bb(k,3) + Jz.trace_of_product(aimag(Dba))
!        end
!        Jz.destroy; Jy.destroy; Jx.destroy
!        shell.destroy_ptr_part
!      end
!   !  bb.zero_small_values(TOL(10))
!      bb = -HALF*bb
!   end
!
!   make_spin_b_field_grid(bb,pt,Dx,Dy,Dz)
!   ! Make the magnetic B field "bb" due to the spin density at a set of points
!   ! given in "pt" using three real symmetric AO density matrices "Dx", "Dy" and
!   ! "Dz" corresponding to the Sx, Sy and Sz densities. Note: the contribution
!   ! is added to whatever is already in bb.
!      bb :: MAT{REAL}, OUT
!      pt :: MAT{REAL}, IN
!      Dx,Dy,Dz :: MAT{REAL}@
!      DD :: MAT3{REAL}@
!      M :: MAT4{REAL}@, target
!      Mxx,Mxy,Mxz :: MAT{REAL}*
!      Myx,Myy,Myz :: MAT{REAL}*
!      Mzx,Mzy,Mzz :: MAT{REAL}*
!      q,a,b,fa,fb,la,lb,n, n_pt, i,j,k :: INT
!      eps :: MAT3{REAL}(3,3,3)
!      fac :: REAL
!      shell :: SHELL2
!
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.allocated,"no atom list")
!   ENSURE(pt.dim2==3,"no basis set")
!   ENSURE(Dx.dim1==.n_bf AND Dx.is_square,"wrong shape for Dx")
!   ENSURE(Dy.dim1==.n_bf AND Dy.is_square,"wrong shape for Dy")
!   ENSURE(Dz.dim1==.n_bf AND Dz.is_square,"wrong shape for Dz")
!
!      n_pt = size(pt,1)
!      bb = ZERO
!      do q = 1,.n_shell_pairs
!        .get_shell_pair(shell,q,a,b,fa,la,fb,lb)
!        DD.create(shell.a.n_comp,shell.b.n_comp,3)
!        DD(:,:,1) = Dx(fb:lb,fa:la)
!        DD(:,:,2) = Dy(fb:lb,fa:la)
!        DD(:,:,3) = Dz(fb:lb,fa:la)
!        fac = ONE; if (fa==fb) fac=HALF
!        M.create(shell.a.n_comp,shell.b.n_comp,3,3)
!        Mxx => M(:,:,1,1); Mxy => M(:,:,1,2); Mxz => M(:,:,1,3)
!        Myx => M(:,:,2,1); Myy => M(:,:,2,2); Myz => M(:,:,2,3)
!        Mzx => M(:,:,3,1); Mzy => M(:,:,3,2); Mzz => M(:,:,3,3)
!        do n = 1,n_pt
!          shell.make_magnetic_S_ints(Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz,pt(n,:))
!          do i = 1,3
!          do j = 1,3
!          do k = 1,3
!             eps(i,j,k) = M(:,:,i,j).trace_product_with(DD(:,:,k))
!          end
!          end
!          end
!          bb(n,1) = bb(n,1) + eps(:,:,1).trace - eps(1,:,:).trace
!          bb(n,2) = bb(n,2) + eps(:,:,2).trace - eps(2,:,:).trace
!          bb(n,3) = bb(n,3) + eps(:,:,3).trace - eps(3,:,:).trace
!        end
!        M.destroy
!        DD.destroy
!        shell.destroy_ptr_part
!      end
!   !  bb.zero_small_values(TOL(10))
!      bb = -bb ! this is -g_e mu_b; the final answer is in units of mu_0/4pi
!   end
!
!   make_pcc_structure_factors
!   !
!      S :: MAT{REAL}@
!      mo,Smo :: MAT{REAL}@
!      in :: TEXTFILE@
!      k_pts :: MAT{REAL}@
!      ftnew,ft :: MAT3{CPX}@
!      W :: MAT{CPX}@
!      ftnew_nabla,ft_nabla :: MAT4{CPX}@
!      shell :: SHELL2@
!      n_refl,q,i,j :: INT
!      fa,la,fb,lb,a,b :: INT
!      fac :: REAL
!      mo.create(160,4)
!      in.create("mo35")
!      in.open_for("read")
!      in.read(mo,by_column=TRUE)
!      stdout.text("mo")
!      stdout.put(mo)
!
!      S.create(160,160)
!      .make_overlap_mx(S)
!
!      Smo.create(4,4)
!      Smo.create(4,4)
!      S.change_basis_using(Smo,mo)
!      stdout.text("Smo")
!      stdout.put(Smo)
!
!      n_refl = .crystal%n_refl
!
!      k_pts.create(n_refl,3)
!      .crystal.xray_data.reflections.make_k_pts(k_pts,.crystal.unit_cell)
!
!      shell.create
!      ftnew.create(4,4,1)
!      ftnew = ZERO
!      do q = 1,.n_shell_pairs
!          .get_shell_pair(q,shell,a,b,fa,la,fb,lb)
!          ft.create(n_refl,shell%a%n_comp,shell%b%n_comp)
!          shell.make_ft(ft,k_pts)
!          fac = ONE
!          if (a==b) fac = HALF
!          do i = 1,n_refl

!                fac* transpose( matmul( transpose(mo(fa:la,:)), matmul(ft(i,:,:),mo(fb:lb,:))) )
!          end
!          ft.destroy
!          shell.destroy_ptr_part
!      end
!
!      do i = 1,n_refl
!         stdout.put(ftnew(:,:,i))
!      end
!      ftnew.destroy
!
!      ftnew_nabla.create(4,4,3,1)
!      ftnew_nabla = ZERO
!      do q = 1,.n_shell_pairs
!          .get_shell_pair(q,shell,a,b,fa,la,fb,lb)
!          ft_nabla.create(n_refl,shell%a%n_comp,shell%b%n_comp,3)
!          shell.make_ft_nabla(ft_nabla,k_pts)
!          fac = ONE
!          if (a==b) fac = HALF
!          do i = 1,n_refl
!          do j = 1,3

!                fac* transpose( matmul( transpose(mo(fa:la,:)), matmul(ft_nabla(i,:,:,j),mo(fb:lb,:))) )
!          end
!          end
!          ft_nabla.destroy
!          shell.destroy_ptr_part
!      end
!
!      do i = 1,n_refl
!      do j = 1,3
!          stdout.show("component =",j)
!          stdout.put(ftnew_nabla(:,:,j,i))
!      end
!      end
!      ftnew_nabla.destroy
!      shell.destroy
!   end
!
!   make_pcc_L_matrices
!   !
!      S :: MAT{REAL}@
!      mo,Smo :: MAT{REAL}@
!      Lx,Ly,Lz :: MAT{REAL}@
!      Lxmo,Lymo,Lzmo :: MAT{REAL}@
!      in :: TEXTFILE@
!      mo.create(160,4)
!      in.create("mo35")
!      in.open_for("read")
!      in.read(mo,by_column=TRUE)
!      stdout.text("mo")
!      stdout.put(mo)
!
!      S.create(160,160)
!      .make_overlap_mx(S)
!
!      Smo.create(4,4)
!      Smo.create(4,4)
!      S.change_basis_using(Smo,mo)
!      stdout.text("Smo")
!      stdout.put(Smo)
!
!      Lx.create(.n_bf,.n_bf); Lx = ZERO
!      Ly.create(.n_bf,.n_bf); Ly = ZERO
!      Lz.create(.n_bf,.n_bf); Lz = ZERO
!      .make_L_matrices(Lx,Ly,Lz)
!
!    ! std_output.text("Lx")
!    ! std_output.put(Lx)
!    ! std_output.text("Ly")
!    ! std_output.put(Ly)
!    ! std_output.text("Lz")
!    ! std_output.put(Lz)
!
!      Lxmo.create(4,4)
!      Lymo.create(4,4)
!      Lzmo.create(4,4)
!      Lx.change_basis_using(Lxmo,mo)
!      Ly.change_basis_using(Lymo,mo)
!      Lz.change_basis_using(Lzmo,mo)
!
!      stdout.text("Lxmo")
!      stdout.put(Lxmo)
!      stdout.text("Lymo")
!      stdout.put(Lymo)
!      stdout.text("Lzmo")
!      stdout.put(Lzmo)
!      S.destroy
!      Smo.destroy
!      Lxmo.destroy; Lymo.destroy; Lzmo.destroy
!      Lx.destroy; Ly.destroy; Lz.destroy
!   end
!
!   test_spin_orbit_B_matrices
!   !
!      Lx,Ly,Lz :: MAT{REAL}@
!      fac :: REAL
!      fac = (TWO*EIGHT*SPEED_OF_LIGHT_AU*SPEED_OF_LIGHT_AU)/G_FACTOR
!      Lx.create(.n_bf,.n_bf)
!      Ly.create(.n_bf,.n_bf)
!      Lz.create(.n_bf,.n_bf)
!      .make_spin_orbit_B_matrices(Lx,Ly,Lz)
!      stdout.text("x")
!      stdout.put( Lx)
!      stdout.text("y")
!      stdout.put( Ly)
!      stdout.text("z")
!      stdout.put( Lz)
!      .make_SOB_matrices(Lx,Ly,Lz)
!      stdout.text("x")
!      stdout.put( Lx)
!      stdout.text("y")
!      stdout.put( Ly)
!      stdout.text("z")
!      stdout.put( Lz)
!      Lx.destroy; Ly.destroy; Lz.destroy
!   end
!
!   make_SOB_matrices(SOBx,SOBy,SOBz)
!   ! Calculate the gauge modified (B field) spin orbit matrices "SOBx" "SOBy" and "SOBz"
!      SOBx,SOBy,SOBz :: MAT{REAL}
!      Qxx,Qxy,Qxz  :: MAT{REAL}@
!      Qyx,Qyy,Qyz  :: MAT{REAL}@
!      Qzx,Qzy,Qzz  :: MAT{REAL}@
!      Qx,Qy,Qz,Qss :: MAT{REAL}@
!      q,c,a,b,fa,la,fb,lb,atom_a,atom_b :: INT
!      fac,Z :: REAL
!      shell :: SHELL2
!      archive :: ARCHIVE
!      debug :: BIN
!      n_a,n_b :: INT
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.allocated,  "no atom list")
!      fac = G_FACTOR/(TWO*EIGHT*(SPEED_OF_LIGHT_AU)**2)
!      SOBx = ZERO; SOBy = ZERO; SOBz = ZERO
!      do q=1,.n_shell_pairs
!         .get_shell_pair(q,shell,fa,la,fb,lb)
!         n_a = shell%a%n_comp; n_b = shell%b%n_comp
!         Qss.create(n_a,n_b)
!         Qx.create(n_a,n_b);  Qy.create(n_a,n_b);  Qz.create(n_a,n_b)
!         Qxx.create(n_a,n_b); Qxy.create(n_a,n_b); Qxz.create(n_a,n_b)
!         Qyx.create(n_a,n_b); Qyy.create(n_a,n_b); Qyz.create(n_a,n_b)
!         Qzx.create(n_a,n_b); Qzy.create(n_a,n_b); Qzz.create(n_a,n_b)
!         do c = 1,.n_atom
!            Z = .atom(c)%atomic_number
!            shell.make_spin_orbit_B_ints(Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz, .atom(c)%pos, .gauge_origin)
!            Qss = Qxx + Qyy + Qzz
!            Qx = Qss*.B_field(1) - Qxx*.B_field(1) - Qxy*.B_field(2) - Qxz*.B_field(3)
!            Qy = Qss*.B_field(2) - Qyx*.B_field(1) - Qyy*.B_field(2) - Qyz*.B_field(3)
!            Qz = Qss*.B_field(3) - Qzx*.B_field(1) - Qzy*.B_field(2) - Qzz*.B_field(3)
!            SOBx(fa:la,fb:lb) = SOBx(fa:la,fb:lb) - Z*Qx
!            SOBy(fa:la,fb:lb) = SOBy(fa:la,fb:lb) - Z*Qy
!            SOBz(fa:la,fb:lb) = SOBz(fa:la,fb:lb) - Z*Qz
!         end
!         shell.destroy_ptr_part
!         Qzz.destroy; Qzy.destroy; Qzx.destroy
!         Qyz.destroy; Qyy.destroy; Qyx.destroy
!         Qxz.destroy; Qxy.destroy; Qxx.destroy
!         Qz.destroy;  Qy.destroy;  Qx.destroy;  Qss.destroy
!      end
!      SOBx.make_symmetric
!      SOBy.make_symmetric
!      SOBz.make_symmetric
!    ! archive.set(.name,"SOB_x_mx"); archive.write(SOBx)
!    ! archive.set(.name,"SOB_y_mx"); archive.write(SOBy)
!    ! archive.set(.name,"SOB_z_mx"); archive.write(SOBz)
!   end
!
!   make_pcc_densities
!   ! Calculate pcc's densities for the paper
!      mo :: MAT{REAL}@
!      mo35,mo48,mo49,mo50 :: VEC{REAL}@
!      p35,p48,p49,p50 :: VEC{REAL}@
!      g35,g48,g49,g50 :: MAT{REAL}@
!      j,s :: MAT{REAL}@
!      in :: TEXTFILE@
!      n_pt,k :: INT
!      mu,nu,rt3,fac :: REAL
!      archive :: ARCHIVE
!
!      mo35.create(160)
!      mo48.create(160)
!      mo49.create(160)
!      mo50.create(160)
!      in.create("mo35")
!      in.open_for("read")
!      in.read(mo35)
!      in.read(mo48)
!      in.read(mo49)
!      in.read(mo50)
!
!      n_pt = .plot_grid%n_pt
!      p35.create(n_pt); g35.create(n_pt,3)
!      p48.create(n_pt); g48.create(n_pt,3)
!      p49.create(n_pt); g49.create(n_pt,3)
!      p50.create(n_pt); g50.create(n_pt,3)
!      .make_orbital_grid(p35,mo35)
!      .make_orbital_grid(p48,mo48)
!      .make_orbital_grid(p49,mo49)
!      .make_orbital_grid(p50,mo50)
!      .make_nabla_orbital_grid(g35,mo35)
!      .make_nabla_orbital_grid(g48,mo48)
!      .make_nabla_orbital_grid(g49,mo49)
!      .make_nabla_orbital_grid(g50,mo50)
!
!      j.create(n_pt,3)
!      s.create(n_pt,3)
!      nu = 0.15578d0
!      mu = 0.19107d0
!      rt3 = ONE/sqrt(THREE)
!      fac = ONE/(ONE+mu*mu+nu*nu)
!      do k = 1,3

!                + HALF*mu*(p50(:)*g49(:,k) - p49(:)*g50(:,k))
!      end
!      j = fac*j
!
!      s(:,1) = -rt3*mu * (p35(:)*p49(:) - nu*p48(:)*p50(:))
!      s(:,2) = -rt3*mu * (p35(:)*p50(:) - nu*p48(:)*p49(:))

!             + (nu*nu+mu*mu/THREE) * p35(:)*p35(:)
!      s(:,3) = HALF*s(:,3)
!
!      archive.set(.name,"current_density")
!      archive.write(j)
!      archive.set(.name,"current_density,gnuplot",format="ascii")
!      archive.write_gnuplot(j, .plot_grid%n_x, .plot_grid%n_y, .plot_grid%n_z)
!      archive.set(.name,"current_norm_density,gnuplot",format="ascii")
!      archive.write_gnuplot(j, .plot_grid%n_x, .plot_grid%n_y, .plot_grid%n_z, norm=TRUE)
!      archive.set(.name,"current_density,normalized,gnuplot",format="ascii")
!      archive.write_gnuplot(j, .plot_grid%n_x, .plot_grid%n_y, .plot_grid%n_z, normalize=TRUE)
!
!      archive.set(.name,"spin_density")
!      archive.write(s)
!      archive.set(.name,"spin_density,gnuplot",format="ascii")
!      archive.write_gnuplot(s, .plot_grid%n_x, .plot_grid%n_y, .plot_grid%n_z)
!      archive.set(.name,"spin_norm_density,gnuplot",format="ascii")
!      archive.write_gnuplot(s, .plot_grid%n_x, .plot_grid%n_y, .plot_grid%n_z, norm=TRUE)
!      archive.set(.name,"spin_density,normalized,gnuplot",format="ascii")
!      archive.write_gnuplot(s, .plot_grid%n_x, .plot_grid%n_y, .plot_grid%n_z, normalize=TRUE)
!
!      s.destroy
!      j.destroy
!      g50.destroy; p50.destroy
!      g49.destroy; p49.destroy
!      g48.destroy; p48.destroy
!      g35.destroy; p35.destroy
!      in.close
!      in.destroy
!      mo50.destroy
!      mo49.destroy
!      mo48.destroy
!      mo35.destroy
!   end

!   read_pcc_density_mx :: leaky
!   ! Read Cassam-Chenai's complex fitted density matrices
!   ! They are complex matrices in the block order:
!   ! alpha-alpha, beta-alpha, alpha-beta, beta-beta.
!
!      textfile :: TEXTFILE@
!      dens :: MAT3{CPX}@
!      val :: CPX
!      name :: STR
!      i,j,k :: INT
!
!      ! Read the name of density matrix file
!      stdin.read(name)
!
!      ! Create the density matrix
!      .density_mx.destroy
!      .density_mx.create(.n_bf,"general_complex")
!
!      ! Open the file for reading
!      textfile.create(name)
!      textfile.open_for("read")
!
!      ! Read them in
!      dens.create(.n_bf,.n_bf,4)
!
!      if (IO_IS_ALLOWED) then
!
!      do i = 1,.n_bf
!      do j = 1,.n_bf
!      do k = 1,4
!         read(textfile.unit,*) val
!         dens(i,j,k) = val
!      end
!      end
!      end
!
!      ! Check
!    ! stdout.show("pcc density, k =",1)
!    ! stdout.put(dens(:,:,1))
!    ! stdout.show("pcc density, k =",2)
!    ! stdout.put(dens(:,:,2))
!    ! stdout.show("pcc density, k =",3)
!    ! stdout.put(dens(:,:,3))
!    ! stdout.show("pcc density, k =",4)
!    ! stdout.put(dens(:,:,4))
!
!
!      ! Introduce normalisation factors
!      .normalize_shell_components(dens(:,:,1))
!      .normalize_shell_components(dens(:,:,2))
!      .normalize_shell_components(dens(:,:,3))
!      .normalize_shell_components(dens(:,:,4))
!   
!      end if
!
!      PARALLEL_BROADCAST(dens,tonto.master_processor)
!
!      ! Assign them
!      textfile.close
!      textfile.destroy
!
!      ! Set the density matrix
!      .density_mx.gc.set_aa_block_to(dens(:,:,1))
!      .density_mx.gc.set_ab_block_to(dens(:,:,2))
!      .density_mx.gc.set_ba_block_to(dens(:,:,3))
!      .density_mx.gc.set_bb_block_to(dens(:,:,4))
!  
!      ! Clean
!      dens.destroy
!
!   end

!   normalize_shell_components(X,left) :: template
!   ! Normalize the shell components in a density matrix.
!   ! If "left" is FALSE, only the right indices are normalized,
!   ! which is appropriate for molecular orbitals.
!   ! NOTE: only works for cartesian basis function matrices.
!      X :: MAT{TYPE}
!      left :: BIN, optional
!
!      s,f,l,ls,fg,lg,i,j :: INT
!      do_left :: BIN
!      fac :: VEC{REAL}@
!
!      do_left = TRUE
!      if (present(left)) do_left = left
!
!      ! Modify the RHS
!      do s = 1,.n_shell
!
!         f  = .first_basis_fn_for_shell(s)
!         l  = .last_basis_fn_for_shell(s)
!         ls = .angular_moment_for_shell(s)
!
!         ! Nothing to do for s & p
!         if (ls<=1) cycle
!
!         ! Get normalisation factors
!         lg = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!         fg = lg + 1
!         lg = lg + GAUSSIAN_DATA:n_comp(ls)
!         fac => GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!
!         ! Apply the factors
!         do i = 1,X.dim1
!            X(i,f:l) = X(i,f:l)*fac
!         end
!
!      end
!
!      if (NOT do_left) return
!
!      do s = 1,.n_shell
!
!         f  = .first_basis_fn_for_shell(s)
!         l  = .last_basis_fn_for_shell(s)
!         ls = .angular_moment_for_shell(s)
!
!         ! Nothing to do for s & p
!         if (ls<=1) cycle
!
!         ! Get normalisation factors
!         lg = GAUSSIAN_DATA:n_comp_up_to(ls-1)
!         fg = lg + 1
!         lg = lg + GAUSSIAN_DATA:n_comp(ls)
!         fac => GAUSSIAN_DATA::unnormalizing_factors(fg:lg)
!
!         ! Apply the factors
!         do j = 1,X.dim2
!            X(f:l,j) = X(f:l,j)*fac
!         end
!
!      end
!
!   end

!   normalize_shell_components(X,left) :: get_from( , TYPE?=>REAL)
!   ! Normalize the shell components in a density matrix.
!   ! If "left" is FALSE, only the right indices are normalized,
!   ! which is appropriate for molecular orbitals.
!   ! NOTE: only works for cartesian basis function matrices.
!   end

!   normalize_shell_components(X,left) :: get_from( , TYPE?=>CPX)
!   ! Normalize the shell components in a density matrix.
!   ! If "left" is FALSE, only the right indices are normalized,
!   ! which is appropriate for molecular orbitals.
!   ! NOTE: only works for cartesian basis function matrices.
!   end

!  ======================
!  Spherical averaged SFs
!  ======================

!   z_circularly_averaged_SF
!   ! Make the circularly averaged structure factors from zero up until a
!   ! specified value of sin(theta)/lambda. The average is taken for all angles
!   ! around the z axis.
! ! ENSURE(.crystal.allocated, "no crystal")
! ! ENSURE(.crystal.reflection_data_exists, "no reflection data")
!   ENSURE(.density_mx.allocated, "no density matrix")
!   ENSURE(.density_mx.has_any_genre, "no density matrix")
!      Fc :: MAT{CPX}@
!      k_pts :: MAT{REAL}@
!      k_values :: VEC{REAL}@
!      orb_kind :: STR
!      complex :: BIN
!      i,n_pts,n_theta :: INT
!      n_theta = 18
!      n_pts = 200
!      Fc.create(n_pts+1,n_theta+1)
!      k_values.create(n_pts+1)
!      do i = 0,n_pts
!         k_values(i+1) = i*0.01d0
!      end
!      .make_ao_density_mx
!    ! .pointgroup.destroy
!    ! .pointgroup.create("oh")
!    ! .symmetrize(.density_mx)
!      orb_kind = .density_mx.spinorbital_genre
!      complex = orb_kind.includes("complex")
!      if (complex) then
!        DIE("complex density not yet implemented")
!      else
!        .make_z_circularly_averaged_ft(Fc,.density_mx.r,k_values,n_theta)
!      end
!      k_values.destroy
!      stdout.text("Circularly averaged structure factors:")
!      stdout.put(Fc)
!    ! .crystal.set_F_calc(Fc)
!    ! .crystal.make_F_predicted
!      Fc.destroy
!   end

!   make_z_circularly_averaged_ft(res,dens,k_values,n_theta)
!   ! Make the z circularly averaged fourier transform of a density described by
!   ! AO density matrix "dens" evaluated at a series of reciprocal space
!   ! distances "k_values" and "n_theta" + 1 equally spaced points, starting from
!   ! 0 and going up to PI.
!      res :: MAT{CPX}, OUT
!      dens :: MAT{REAL}, target
!      n_theta :: INT, IN
!      k_values :: VEC{REAL}, IN
!   ENSURE(.basis_info_made, "no basis info")
!   ENSURE(.atom.allocated,  "no atom list")
!   ENSURE(res.dim1==k_values.dim,"incompatible res and k_values")
!   ENSURE(res.dim2==n_theta+1,"incompatible res and n_theta")
!      k,fa,fb,la,lb,q,t :: INT
!      sh :: SHELL2
!      ft_ab :: MAT4{CPX}@
!      dens_ba :: MAT{REAL}@
!      cutoff,fac,val :: REAL
!      res = ZERO
!      cutoff = TOL(10) / .n_shell_pairs
!      parallel do q = 1,.n_shell_pairs
!         .get_shell_pair(sh,q,fa,la,fb,lb)
!         if (sh.skip_ft(cutoff)) then
!           sh.destroy_ptr_part
!           cycle
!         end
!         ft_ab.create(sh.a.n_comp,sh.b.n_comp,k_values.dim,n_theta+1)
!         sh.make_z_circularly_averaged_ft(ft_ab,k_values,n_theta)
!         dens_ba => dens(fa:la,fb:lb)
!         fac = ONE
!         if (fa/=fb) fac = TWO ! count off-diagonals twice.
!         do t = 1,n_theta+1
!         do k = 1,k_values.dim
!            res(k,t) = res(k,t) + fac*sum(ft_ab(:,:,k,t)*dens_ba(:,:))
!         end
!         end
!         ft_ab.destroy
!         sh.destroy_ptr_part
!      end
!      PARALLEL_SYMMETRIC_SUM(res)
!   end

!   spherically_averaged_SF
!   ! Make the spherically averaged structure factors from zero up until a
!   ! specified value of sin(theta)/lambda.
!
!   ENSURE(.density_mx.allocated, "no density matrix")
!   ENSURE(.density_mx.has_any_genre, "no density matrix")
!
!      Fc :: VEC{CPX}@
!      k_values :: VEC{REAL}@
!      orb_kind :: STR
!      complex :: BIN
!      i,k,n_pts :: INT
!
!      n_pts = 750
!
!      Fc.create(n_pts)
!      k_values.create(n_pts)
!
!      k = 0
!      do i = 60,n_pts,7
!         k = k + 1
!         k_values(k) = i*0.01d0
!      end
!      Fc.shrink(k)
!      k_values.shrink(k)
!
!      .make_ao_density_mx
!
!    ! .pointgroup.destroy
!    ! .pointgroup.create("oh")
!    ! .symmetrize(.density_mx)
!
!      orb_kind = .density_mx.spinorbital_genre
!      complex  = orb_kind.includes("complex")
!      if (complex) then
!         DIE("complex density not yet implemented")
!      else
!         .make_spherically_averaged_ft(Fc,.density_mx.r,k_values)
!      end
!      k_values.destroy
!
!      stdout.text("Spherically averaged structure factors:")
!      stdout.put(Fc)
!    ! .crystal.set_F_calc(Fc)
!    ! .crystal.make_F_predicted
!
!      Fc.destroy
!
!   end

!   make_spherically_averaged_ft(res,dens,k_values)
!   ! Make the spherically averaged fourier transform of a density described by
!   ! AO density matrix "dens" evaluated at a series of reciprocal space
!   ! distances "k_values".
!      res :: VEC{CPX}, OUT
!      dens :: MAT{REAL}, target
!      k_values :: VEC{REAL}, IN
!
!   ENSURE(.basis_info_made, "no basis info")
!   ENSURE(.atom.allocated,  "no atom list")
!   ENSURE(res.dim==k_values.dim,"incompatible res and k_values")
!
!      k,fa,la,na,fb,lb,nb,q :: INT
!      sh :: SHELL2
!      ftab :: MAT3{CPX}@
!      Dba :: MAT{REAL}@
!      cutoff,fac :: REAL
!
!      res = ZERO
!      cutoff = TOL(10) / .n_shell_pairs
!
!      do q = 1,.n_shell_pairs
!
!         .set_shell2_from(q,sh,fa,la,na,fb,lb,nb)
!
!         ! Skip?
!         if (sh.skip_ft(cutoff)) then
!            sh.destroy_ptr_part
!            cycle
!         end
!
!         ! Make spherical average
!         ftab.create(sh.a.n_comp,sh.b.n_comp,k_values.dim)
!         sh.make_spherically_averaged_ft(ftab,k_values)
!
!         ! Density block
!         Dba => dens(fa:la,fb:lb)
!
!         ! Off-diagonal weighting
!         fac = TWO
!         if (fa==fb) fac = ONE
!
!         ! Sum
!         do k = 1,k_values.dim
!            res(k) = res(k) + fac * sum(ftab(:,:,k)*Dba(:,:))
!         end
!
!         ftab.destroy
!         sh.destroy_ptr_part
!
!      end
!
!   end

!  =======================
!  Core structure factors
!  =======================

!   make_core_SFs :: leaky
!   ! Make em!
!
!   ENSURE(.atom.allocated,"no atoms")
!   ENSURE(.atom_info_made,"no atom info")
!   ENSURE(.basis.allocated,"no basis set")
!   ENSURE(.basis_info_made,"no basis_set info")
!   ENSURE(.n_atom_kind>0,"no atom kinds")
!   ENSURE(.crystal.allocated,"no crystal information!")
!   ENSURE(.crystal.xray_data.allocated,"no crystal xray data!")
!   ENSURE(.crystal.xray_data.reflections.allocated,"no xray_data reflection!")
!
!      AO :: OPMATRIX@
!      k_pt,D,AO_core :: MAT{REAL}@
!      k1,k2,k3  :: VEC{REAL}@
!      ra,diff :: VEC{REAL}(3)
!      F_calc,core,phase :: VEC{CPX}@
!      mol :: MOLECULE@
!      II  :: CPX
!      n_k,n_core,n_bf,k,a,b,n :: INT
!
!      ! Complex i
!      II = IMAGIFY(ONE)
!
!      ! Make ANO data if not already there (leaky)
!      .make_ANOs_and_interpolators
!
!      ! Create space for the core reflections (leaky)
!      .crystal.core_reflection.destroy
!      .crystal.core_reflection.create(.crystal.xray_data.reflections.dim)
!
!      ! Space for k points
!      n_k = .crystal.xray_data.reflections.dim
!      k_pt.create(n_k,3)
!
!      ! Make k points and components k1, k2, k3 ...
!      .crystal.xray_data.reflections.make_k_pts(k_pt,.crystal.unit_cell)
!      k1 => k_pt(:,1)
!      k2 => k_pt(:,2)
!      k3 => k_pt(:,3)
!
!      ! Make space for F_calc
!      F_calc.create(n_k)
!      F_calc = ZERO
!
!      ! Make space for translation phases
!      phase.create(n_k)
!
!      ! Get core structure factors
!      ! *only for unique atoms*
!      do a = 1,.n_atom
!
!        ! Unique atom?
!        if (NOT .atom(a).is_unique_kind) cycle
!        k = .atom(a).kind
!
!        ! Skip H atoms they have no core!
!        if (.atom(a).atomic_number<=1) cycle
!
!        ! No. of core orbitals
!        n_core = .atom(a).n_core_electrons/2
!
!        ! No. of basis functions for atom u
!        n_bf = .atom(a).basis.n_bf
!
!        ! Space for the core density matrix
!        D.create(n_bf,n_bf)
!
!        ! Atomic orbitals
!        AO => .atom(a).atomic_orbitals
!
!        ! Make core density matrix
!        if (AO.r.allocated) then
!
!           ! Core atomic orbitals
!           AO_core => .atom(a).atomic_orbitals.r(:,1:n_core)
!
!           ! Make the core density matrix
!           D.to_product_of(AO_core,AO_core,transpose_b=TRUE)
!
!        else if (AO.a.allocated) then
!
!           ! Alpha core part
!           AO_core => .atom(a).atomic_orbitals.a(:,1:n_core)
!           D.to_product_of(AO_core,AO_core,transpose_b=TRUE)
!
!           ! Beta core part
!           AO_core => .atom(a).atomic_orbitals.b(:,1:n_core)
!           D.plus_product_of(AO_core,AO_core,transpose_b=TRUE)
!
!        end
!
!        ! Make a molecule from atom a
!        mol.create
!        .set_molecule_from_atom(a,mol)
!
!        ! Make the core spherical
!        mol.pointgroup.create("oh",.use_spherical_basis)
!        mol.symmetrize(D)
!
!        ! Duplicate crystal information into this mol
!        mol.crystal.create_copy(.crystal)
!
!        ! Make the core structure factors
!        mol.crystal.assign_xray
!        mol.crystal.xray_data.refine_scale      = FALSE
!        mol.crystal.xray_data.refine_extinction = FALSE
!        mol.crystal.xray_data.add_dispersion_to_F_calc  = FALSE
!        mol.make_x_structure_factors_h(D)
!        D.destroy
!
!        ! Make a copy of atom u's core structure factors
!        core.create_copy(mol.crystal.xray_data.reflections(:).F_calc)
!
!        ! Nullify parts we just copied or can't destroy
!        mol.atom.nullify_ptr_part       ! do not destroy ptr part
!        nullify(mol.basis)              ! do not destroy this
!        nullify(mol.slaterbasis)        ! do not destroy this
!        nullify(mol.coppensbasis)       ! do not destroy this
!
!        ! Safe to destroy
!        mol.destroy
!
!        ! Add in the core reflections for atom u
!        F_calc = F_calc + core
!
!        ! Store atom u position
!        ra = .atom(a).position
!
!        ! Loop over other equivalent atoms; add core contributions
!        do b = a+1,.n_atom
!
!           if (.atom(b).kind/=k) cycle
!
!           ! Difference between atom u and atom j position
!           diff = .atom(b).position - ra
!
!           ! Make the phase shifts
!           do n = 1,n_k
!              phase(n) = exp(II*(k1(n)*diff(1)+k2(n)*diff(2)+k3(n)*diff(3)))
!           end
!
!           ! Add the structure factors in
!           F_calc = F_calc + phase*core
!
!        end
!
!      end
!
!      ! Set the crystal core structure factors
!      .crystal.core_reflection(:).F_calc = F_calc
!
!      ! Clean up
!      phase.destroy
!      F_calc.destroy
!      k_pt.destroy
!
!   end

!  =================
!  Put interpolators
!  =================

   subroutine put_interpolator_list(self)
   ! Put a list of atom interpolators out.
   ! Requires a list of integers (the atoms indices) to print out.
      type(MOLECULE_TYPE), IN :: self
      STR(len=STR_SIZE) :: head
      VEC(INT,:), allocatable :: ind
      INT :: a,i
   ENSURE(allocated(self%atom),"MOLECULE.MISC:put_interpolator_list ... no atoms")
   ENSURE(self%atom_info_made,"MOLECULE.MISC:put_interpolator_list ... no atom info")
   ENSURE(has_all_interpolators_(self%atom),"MOLECULE.MISC:put_interpolator_list ... no atom interpolators")
   ENSURE(self%n_atom_kind>0,"MOLECULE.MISC:put_interpolator_list ... no atom kinds")

      ! Read the atom list
      if (n_line_items_(stdin)>1) then
         call read_all_(stdin,ind)
      else
         call create_(ind,self%n_atom)
         ind = [(i,i = 1,self%n_atom)]
      end if
      call flush_(stdout)
      call text_(stdout,"===================================")
      call text_(stdout,"Selected list of atom interpolators")
      call text_(stdout,"===================================")

      ! Copy atom interpolators. Leaky
      do a = 1,size(ind)

         ! Atom details (short version)
         call put_short_(self%atom(a))

         ! Header
         head = "Atom "//trim(to_str_(a))//" interpolator"
         call flush_(stdout)
         call text_(stdout,repeat("=",len_trim(head)))
         call text_(stdout,trim(head))
         call text_(stdout,repeat("=",len_trim(head)))
         call flush_(stdout)

         ! Put it out
         i = ind(a)
         call put_(self%atom(i)%interpolator)
      end do

      ! Clean
      call destroy_(ind)
   end subroutine


!  ========================
!  Zero the overlap-density
!  ========================

   subroutine zero_overlap_density(self)
   ! Calculate the noninteracting group overlap matrix "S"
      type(MOLECULE_TYPE), INOUT :: self
      INT :: q,fa,la,na,fb,lb,nb,atom_a,atom_b
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.MISC:zero_overlap_density ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.MISC:zero_overlap_density ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.MISC:zero_overlap_density ... no density matrix")

      ! Zero the off diagonal blocks
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         if (atom_a/=atom_b) self%density_mx%r(fa:la,fb:lb) = ZERO
         call destroy_ptr_part_(sh)
      end do

      ! Make symmetric
      call symmetric_reflect_(self%density_mx%r)
   end subroutine

   subroutine zero_off_atom_density(self)
   ! Zero the off-atom overlap blocks of the density matrix
      type(MOLECULE_TYPE), IN :: self
   ENSURE(allocated(self%density_mx),"MOLECULE.MISC:zero_off_atom_density ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.MISC:zero_off_atom_density ... no restricted density")
      call zero_off_atom_blocks_(self,self%density_mx%r)
   end subroutine

   subroutine zero_off_atom_blocks(self,P)
   ! Zero the off-atom overlap blocks in "P"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:) :: P
      INT :: a,b,na,fa,la,nb,fb,lb
   ENSURE(is_square_(P),"MOLECULE.MISC:zero_off_atom_blocks ... wrong shape, P")
   ENSURE(size(P,1)==self%n_bf,"MOLECULE.MISC:zero_off_atom_blocks ... wrong size, P")
   ENSURE(allocated(self%atom),"MOLECULE.MISC:zero_off_atom_blocks ... no atoms")
      la = 0
      do a = 1,self%n_atom
         na = self%atom(a)%basis%n_bf
         fa = la+1
         la = la+na
         lb = 0
         do b = 1,self%n_atom
            nb = self%atom(b)%basis%n_bf
            fb = lb+1
            lb = lb+nb
            if (a==b) cycle
            P(fa:la,fb:lb) = ZERO
         end do
      end do
   end subroutine


!  =============
!  Miscellaneous
!  =============

!   make_weak_force_energy_shift
!   ! Make the expectation value of the parity-violating energy shift.
!   ! Requires some archived molecular orbitals, general_complex kind.
!   ! Reference: R. Zanasi and P. Lazzeretti, CPL 286, 240 (1998)
!   ENSURE(.basis_info_made, "no basis info")
!
!       pt :: MAT{REAL}@
!       Pa,Pb :: VEC{CPX}@
!       Na,Nb :: MAT{CPX}@
!       PV,NN :: VEC{REAL}@
!       n, x,y,z :: INT
!       Gamma :: REAL
!       I :: CPX
!       arch :: ARCHIVE
!
!       .MOs.destroy_ptr_part
!       arch.set(.name,"molecular_orbitals")
!       arch.read(.MOs, genre="general_complex")
!
!       Gamma = 5.73416d-17
!       PV.create(.n_atom)
!       pt.create(.n_atom,3)
!       .atom.put_coordinates_to(pt,positions_as_rows=TRUE)
!
!       Pa.create(.n_atom);   Pb.create(.n_atom)
!       Na.create(.n_atom,3); Nb.create(.n_atom,3)
!       I = (ZERO,ONE)
!       stdout.set_real_fortran_fmt("d")
!
!       x = 1
!       y = 2
!       z = 3
!       PV = ZERO
!       do n = 1,.n_e
!
!          .make_nabla_orbital_grid_c(Na,Pa,.MOs%general_complex(      1:  .n_bf,n), pt)
!          .make_nabla_orbital_grid_c(Nb,Pb,.MOs%general_complex(.n_bf+1:2*.n_bf,n), pt)
!

!             + RE(I*conjg(Pb(:))*Nb(:,z))
!       end
!       PV = -HALF*Gamma*PV
!
!       Nb.destroy; Na.destroy
!       Pb.destroy; Pa.destroy
!       pt.destroy
!
!       NN.create(.n_atom)
!       .atom.get_mean_neutron_numbers(NN)
!       PV = PV*NN
!
!       stdout.set_real_fortran_fmt("d")
!       stdout.flush
!       stdout.text("Contributions (by nucleus) to the parity-violating weak force energy shift term:")
!       stdout.flush
!       stdout.put(PV)
!       stdout.flush
!       stdout.show("Total contribution =", sum(PV) )
!       stdout.flush
!       stdout.text("Neutron numbers:")
!       stdout.flush
!       stdout.put(NN)
!       stdout.set_real_fortran_fmt("f")
!
!       NN.destroy
!       PV.destroy
!
!       .MOs.destroy_ptr_part
!
!   end

   subroutine put_g_tensor_information(self)
   ! Put the g-tensor shift information to the output.
   ! Reference: Jayatilaka, JCP 108, 7587 (1998)
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: Lx,Ly,Lz,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz,T
      MAT(CPX,:,:), allocatable :: HH
      REAL :: Sx,Sy,Sz,SS_net,Mx,My,Mz,MM_net
      REAL :: Qx,Qy,Qz,QQ_net,Tx,Ty,Tz,TT_net
      REAL :: Sx_ppm,Sy_ppm,Sz_ppm,Mx_ppm,My_ppm,Mz_ppm
      REAL :: Qx_ppm,Qy_ppm,Qz_ppm,Tx_ppm,Ty_ppm,Tz_ppm
      REAL :: fac,fx,fy,fz,total
      VEC(REAL,3) :: quantization_axis
      CPX :: I
      INT :: m
      BIN :: int_width
      type(ARCHIVE_TYPE) :: arch
      call destroy_ptr_part_(self%density_mx)
      call set_(arch,self%name,"density_mx")
      call read_(arch,self%density_mx,genre="general_complex")
      call create_(HH,2*self%n_bf,2*self%n_bf)
      I = (ZERO,ONE)
      quantization_axis = self%SCF_data%quantization_axis

      ! Spin contribution
      call make_overlap_mx_(self)
      fac = G_FACTOR/FOUR
      HH = ZERO
      call ba_block_plus_(HH,self%overlap_mx,fac)
      call ab_block_plus_(HH,self%overlap_mx,fac)
      Sx = RE(trace_product_with_(HH,self%density_mx%gc))
      HH = ZERO
      call ba_block_plus_(HH,self%overlap_mx,fac*I)
      call ab_block_plus_(HH,self%overlap_mx,-fac*I)
      Sy = RE(trace_product_with_(HH,self%density_mx%gc))
      HH = ZERO
      call aa_block_plus_(HH,self%overlap_mx,fac)
      call bb_block_plus_(HH,self%overlap_mx,-fac)
      Sz = RE(trace_product_with_(HH,self%density_mx%gc))
      m = self%n_a-self%n_b
      fx = m*fac*quantization_axis(1)
      fy = m*fac*quantization_axis(2)
      fz = m*fac*quantization_axis(3)
      Sx_ppm = (Sx-fx)*4000000/m
      Sy_ppm = (Sy-fy)*4000000/m
      Sz_ppm = (Sz-fz)*4000000/m
      SS_net = Sx_ppm*quantization_axis(1)+Sy_ppm*quantization_axis(2)+Sz_ppm*quantization_axis(3)

      ! L contribution
      call create_(Lx,self%n_bf,self%n_bf)
      call create_(Ly,self%n_bf,self%n_bf)
      call create_(Lz,self%n_bf,self%n_bf)
      call make_L_matrices_(self,Lx,Ly,Lz)
      fac = HALF
      HH = ZERO
      call aa_block_plus_(HH,Lx,-fac*I)  ! -I factor for the nabla part
      call bb_block_plus_(HH,Lx,-fac*I)
      Mx = RE(trace_product_with_(HH,self%density_mx%gc))
      HH = ZERO
      call aa_block_plus_(HH,Ly,-fac*I)
      call bb_block_plus_(HH,Ly,-fac*I)
      My = RE(trace_product_with_(HH,self%density_mx%gc))
      HH = ZERO
      call aa_block_plus_(HH,Lz,-fac*I)
      call bb_block_plus_(HH,Lz,-fac*I)
      Mz = RE(trace_product_with_(HH,self%density_mx%gc))
      call destroy_(Lz)
      call destroy_(Ly)
      call destroy_(Lx)
      Mx_ppm = Mx*4000000/m
      My_ppm = My*4000000/m
      Mz_ppm = Mz*4000000/m
      MM_net = Mx_ppm*quantization_axis(1)+My_ppm*quantization_axis(2)+Mz_ppm*quantization_axis(3)

      ! 1 electron LS gauge contribution
      call create_(Qxx,self%n_bf,self%n_bf); call create_(Qxy,self%n_bf,self%n_bf); call create_(Qxz,self%n_bf,self%n_bf)
      call create_(Qyx,self%n_bf,self%n_bf); call create_(Qyy,self%n_bf,self%n_bf); call create_(Qyz,self%n_bf,self%n_bf)
      call create_(Qzx,self%n_bf,self%n_bf); call create_(Qzy,self%n_bf,self%n_bf); call create_(Qzz,self%n_bf,self%n_bf)
      call make_spin_orbit_Q_matrices_(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz)
      fac = G_FACTOR/(TWO*EIGHT*SPEED_OF_LIGHT_AU*SPEED_OF_LIGHT_AU)
      HH = ZERO
      call ba_block_plus_(HH,Qyy,fac)
      call ba_block_plus_(HH,Qzz,fac)
      call ba_block_plus_(HH,Qyx,-I*fac)
      call aa_block_plus_(HH,Qzx,-fac)
      call bb_block_plus_(HH,Qzx,+fac)
      call make_hermitian_(HH)
      Qx = RE(trace_product_with_(HH,self%density_mx%gc))
      HH = ZERO
      call ba_block_plus_(HH,Qxx,I*fac)
      call ba_block_plus_(HH,Qzz,I*fac)
      call ba_block_plus_(HH,Qxy,-fac)
      call aa_block_plus_(HH,Qzy,-fac)
      call bb_block_plus_(HH,Qzy,+fac)
      call make_hermitian_(HH)
      Qy = RE(trace_product_with_(HH,self%density_mx%gc))
      HH = ZERO
      call aa_block_plus_(HH,Qxx,fac)
      call aa_block_plus_(HH,Qyy,fac)
      call bb_block_plus_(HH,Qxx,-fac)
      call bb_block_plus_(HH,Qyy,-fac)
      call ba_block_plus_(HH,Qxz,-fac)
      call ba_block_plus_(HH,Qyz,-I*fac)
      call make_hermitian_(HH)
      Qz = RE(trace_product_with_(HH,self%density_mx%gc))
      call destroy_(Qzz)
      call destroy_(Qzy)
      call destroy_(Qzx)
      call destroy_(Qyz)
      call destroy_(Qyy)
      call destroy_(Qyx)
      call destroy_(Qxz)
      call destroy_(Qxy)
      call destroy_(Qxx)
      Qx_ppm = Qx*4000000/m
      Qy_ppm = Qy*4000000/m
      Qz_ppm = Qz*4000000/m
      QQ_net = Qx_ppm*quantization_axis(1)+Qy_ppm*quantization_axis(2)+Qz_ppm*quantization_axis(3)

      ! Relativistic kinetic energy contribution
      call create_(T,self%n_bf,self%n_bf)
      call make_kinetic_energy_mx_(self,T)
      fac = -G_FACTOR/(FOUR*SPEED_OF_LIGHT_AU*SPEED_OF_LIGHT_AU)
      HH = ZERO
      call ba_block_plus_(HH,T,fac)
      call ab_block_plus_(HH,T,fac)
      Tx = RE(trace_product_with_(HH,self%density_mx%gc))
      HH = ZERO
      call ba_block_plus_(HH,T,I*fac)
      call ab_block_plus_(HH,T,-I*fac)
      Ty = RE(trace_product_with_(HH,self%density_mx%gc))
      HH = ZERO
      call aa_block_plus_(HH,T,fac)
      call bb_block_plus_(HH,T,-fac)
      Tz = RE(trace_product_with_(HH,self%density_mx%gc))
      call destroy_(T)
      call destroy_(HH)
      call destroy_ptr_part_(self%density_mx)
      Tx_ppm = Tx*4000000/m
      Ty_ppm = Ty*4000000/m
      Tz_ppm = Tz*4000000/m
      TT_net = Tx_ppm*quantization_axis(1)+Ty_ppm*quantization_axis(2)+Tz_ppm*quantization_axis(3)
      call set_real_fortran_fmt_(stdout,"d")
      int_width = TRUE
      call flush_(stdout)
      call text_(stdout,"Contribution to g-tensor shift:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      call put_(stdout,"Term",int_width); call put_(stdout,"<value>");
      call put_(stdout,"shift/ppm"); call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      call text_(stdout,"Spin term:")
      call put_(stdout,"S_x",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Sx)
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Sx_ppm); call flush_(stdout)
      call put_(stdout,"S_y",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Sy)
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Sy_ppm); call flush_(stdout)
      call put_(stdout,"S_z",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Sz);
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Sz_ppm); call flush_(stdout)
      call put_(stdout,"Net",int_width); call tab_(stdout,real_fields=1);
      call put_(stdout,SS_net); call flush_(stdout)
      call text_(stdout,"Angular momentum term:")
      call put_(stdout,"L_x",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Mx);
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Mx_ppm); call flush_(stdout)
      call put_(stdout,"L_y",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,My);
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,My_ppm); call flush_(stdout)
      call put_(stdout,"L_z",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Mz);
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Mz_ppm); call flush_(stdout)
      call put_(stdout,"Net",int_width); call tab_(stdout,real_fields=1);
      call put_(stdout,MM_net); call flush_(stdout)
      call text_(stdout,"1-electron L:S gauge term:")
      call put_(stdout,"Q_x",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Qx);
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Qx_ppm); call flush_(stdout)
      call put_(stdout,"Q_y",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Qy);
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Qy_ppm); call flush_(stdout)
      call put_(stdout,"Q_z",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Qz);
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Qz_ppm); call flush_(stdout)
      call put_(stdout,"Net",int_width); call tab_(stdout,real_fields=1);
      call put_(stdout,QQ_net); call flush_(stdout)
      call text_(stdout,"Relativistic B:S kinetic term:")
      call put_(stdout,"T_x",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Tx)
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Tx_ppm); call flush_(stdout)
      call put_(stdout,"T_y",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Ty);
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Ty_ppm); call flush_(stdout)
      call put_(stdout,"T_z",int_width)
      call set_real_fortran_fmt_(stdout,"d"); call put_(stdout,Tz);
      call set_real_fortran_fmt_(stdout,"f"); call put_(stdout,Tz_ppm); call flush_(stdout)
      call put_(stdout,"Net",int_width); call tab_(stdout,real_fields=1);
      call put_(stdout,TT_net); call flush_(stdout)
      total = SS_net+MM_net+QQ_net+TT_net
      call flush_(stdout)
      call put_(stdout,"Total:",int_width); call tab_(stdout,real_fields=1); call put_(stdout,total)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      call set_real_fortran_fmt_(stdout,"f")
   end subroutine

   subroutine make_Fermi_contact_terms(self)
   ! subroutine to calculate fermi contact terms
      type(MOLECULE_TYPE), INOUT :: self
      INT :: a,i,na,nb
      VEC(REAL,:), allocatable :: fermi_terms,GN
      MAT(REAL,:,:), allocatable :: grid
      REAL :: value
      BIN :: int_width
      call create_(grid,self%n_atom,3)
      ENSURE(allocated(self%MOs%a),"MOLECULE.MISC:make_Fermi_contact_terms ... no alpha orbitals")
      ENSURE(allocated(self%MOs%b),"MOLECULE.MISC:make_Fermi_contact_terms ... no beta orbitals")

   ! grid containing positions of all nuclei in da molecule
      do i = 1,self%n_atom
         grid(i,1:3) = self%atom(i)%position
      end do

   ! the data is obtained from CRC handbook of chemistry and physics 86th
   ! edition section 9 molecular structure and spectroscopy 9-92
      WARN("MOLECULE.MISC:make_Fermi_contact_terms ... Experimantal implementation, only few g-factors are accounted for,")
      WARN("MOLECULE.MISC:make_Fermi_contact_terms ... hence you will see contact spin density, but other parameters might")
      WARN("MOLECULE.MISC:make_Fermi_contact_terms ... be missed in the table!")
      call create_(GN,100)
      GN = ZERO
      GN(1) = TWO*2.792847337
      GN(6) = TWO*0.7024118
      GN(7) = 0.4037610
      GN(8) = TWO/FIVE*(-1.8937)
      GN(29) = TWO/THREE*2.2721
      call flush_(stdout)
      call text_(stdout,"number of atoms:    ")
      call flush_(stdout)
      call put_(stdout,self%n_atom)
      call flush_(stdout)
      call put_(stdout,size(grid,1))
      call flush_(stdout)
      call text_(stdout,"atomic grid:     ")
      call flush_(stdout)
      call put_(stdout,grid)
      call flush_(stdout)
      na = self%n_a
      nb = self%n_b

!  lets evaluate the spin density at each nucleus
      call create_(fermi_terms,self%n_atom)
      call make_SD_grid_(self,fermi_terms,grid)

! from a.u. to MHz we are following Morton & Preston J. Mag. Resonance 30, 577-582, (1978)
!    fermi_terms=fermi_terms*104.98*NUCLEAR_MAGNETON_SI/PLANCK_SI/1000000.0
!   do i=1,.n_atom
!     fermi_terms(i)=fermi_terms(i)*GN(.atom(i).atomic_number)
!   end do
      int_width = TRUE
      call dash_(stdout,int_fields=2,real_fields=5)
      call flush_(stdout)
      call text_(stdout,"fermi contact terms for atoms:  ")
      call flush_(stdout)
      call put_(stdout,"i",int_width)
      call put_(stdout,"Atom",int_width)
      call put_(stdout,"spin density")
      call put_(stdout,"MHz")
      call put_(stdout,"Gauss")
      call put_(stdout,"mT")
      call put_(stdout,"10(-4) cm-1")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=5)
      do a = 1,self%n_atom
         call put_(stdout,a)
         call put_(stdout,self%atom(a)%label,int_width=TRUE)
         value = fermi_terms(a)
         call put_(stdout,value)
! from a.u. to MHz we are following Morton & Preston J. Mag. Resonance 30, 577-582, (1978)
         value = fermi_terms(a)*104.98*NUCLEAR_MAGNETON_SI/PLANCK_SI/1000000.0
         value = value*GN(self%atom(a)%atomic_number)
         call put_(stdout,value)
! from MHz to Guass following Stasko et al. ESR spektroskopia, STU, Bratislava, 1989 (textbook)
         value = value/2.80247
         call put_(stdout,value)
         value = value/10.0d0
         call put_(stdout,value)
         value = value*2.80247*10.0d0*0.333564
         call put_(stdout,value)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=5)
      call flush_(stdout)
      call destroy_(fermi_terms)
      call destroy_(grid)
   end subroutine


!   integrate_spin_numerically
!   ! Integrate the spin density numerically
!      self :: INOUT
!
!   ENSURE(.becke_grid.allocated,"no becke_grid")
!   ENSURE(.becke_grid.finalized,"no finalized becke_grid")
!
!      pt :: MAT{REAL}@
!      wt,rho_grid :: VEC{REAL}@
!      n_pt :: INT
!      n_e :: REAL
!
!      stdout.flush
!      stdout.text("Numerically integrate the spin density with a BECKE_GRID :")
!      stdout.flush
!      
!      ! Make becke grid (leaky)
!      .becke_grid.put
!      .becke_grid.put_atom_info
!      .becke_grid.make_grid(pt,wt)
!
!      n_pt = pt.dim1
!
!      rho_grid.create(n_pt)
!
!      ! Call for spin density   
!      sgn = -1
!      .make_ED_grid(rho_grid,pt,sgn)
!
!      n_e = sum(rho_grid*wt)
!
!      stdout.show("n_pt =",n_pt)
!      stdout.show("n_spin =",n_e)
!
!      rho_grid.destroy
!      wt.destroy
!      pt.destroy
!
!   end

!   integrate_nc_spin_numerically
!   ! Integrate the noncollinear spin density numerically
!      self :: PTR
!
!   ENSURE(.becke_grid.allocated,"no becke_grid")
!   ENSURE(.becke_grid.finalized,"no finalized becke_grid")
!
!      pt :: MAT{REAL}@
!      wt,rho_grid :: VEC{REAL}@
!      n_pt :: INT
!      n_e :: REAL
!
!      stdout.flush
!      stdout.text("Numerically integrate the noncollinear spin density with a BECKE_GRID :")
!      stdout.flush
!      
!
!      ! Make becke grid (leaky)
!      .becke_grid.put
!      .becke_grid.put_atom_info
!      .becke_grid.make_grid(pt,wt)
!
!      n_pt = pt.dim1
!
!      rho_grid.create(n_pt)
!
!      ! Spin density ?? dj
!      .make_nc_density_grid_gc(rho_grid,pt)
!
!      n_e = sum(rho_grid*wt)
!
!      stdout.show("n_pt =",n_pt)
!      stdout.show("n_spin =",n_e)
!
!      rho_grid.destroy
!      wt.destroy
!      pt.destroy
!
!   end

!   integrate_ku_spin_numerically
!   ! Integrate the (unpaired electron) density numerically
!      self :: PTR
!
!   ENSURE(.becke_grid.allocated,"no becke_grid")
!   ENSURE(.becke_grid.finalized,"no finalized becke_grid")
!
!      pt :: MAT{REAL}@
!      wt,rho_grid :: VEC{REAL}@
!      n_pt :: INT
!      n_e :: REAL
!
!      stdout.flush
!      stdout.text("Numerically integrate the noncollinear spin density with a BECKE_GRID :")
!      stdout.flush
!      
!
!      ! Make becke grid (leaky)
!      .becke_grid.put
!      .becke_grid.put_atom_info
!      .becke_grid.make_grid(pt,wt)
!
!      n_pt = pt.dim1
!
!      rho_grid.create(n_pt)
!
!      ! Total density   
!      .make_ED_grid(rho_grid,pt)
!
!      !  Substract paired electron density   
!      .make_ku_density_grid(rho_grid,pt)
!
!      n_e = sum(rho_grid*wt)
!
!      stdout.show("n_pt =",n_pt)
!      stdout.show("n_spin =",n_e)
!
!      rho_grid.destroy
!      wt.destroy
!      pt.destroy
!
!   end

!   apply_anti_hrr(P,PP) :: leaky
!   ! Apply the anti-horizontal recursion relation (i.e. the ant-transfer
!   ! relation) to matrix "P" to produce the shell pair vector "PP" which has to
!   ! be multipled by the source integrals to give the contracted term (usually
!   ! the J-matrix)
!      self :: INOUT
!      P :: MAT{REAL}, INOUT
!      PP :: VEC{EVEC{REAL}@, target
!
!   ENSURE(.basis_info_made,"no basis info")
!   ENSURE(.atom.allocated,"no atom info")
!
!      R :: VEC{REAL}(3)
!      R1,R2,R3,T1,T2,T3,fx,fy,fz :: REAL
!      ab :: VEC{REAL}@
!      ax,ay,az,bx,by,bz,Rx,Ry,Rz :: VEC{REAL}@
!      binbx,binby,binbz :: VEC{INT}@
!      q,fa,la,fb,lb,atom_a,atom_b,aa,bb :: INT
!      i,a,b,x,y,z,xa,ya,za,xb,yb,zb,xx,yy,zz :: INT
!      sh :: SHELL2
!
!      PP.create(.n_shell_pairs)                                     ! leaky
!      Rx.create(.basis_l_max)
!      Ry.create(.basis_l_max)
!      Rz.create(.basis_l_max)
!
!      aa = 0
!      bb = 0
!
!      do q = 1,.n_shell_pairs
!
!         .set_shell2_from(q,sh,fa,la,fb,lb,atom_a,atom_b)
!
!         if (sh.l_sum==0) then
!
!            PP(q).element.create(1)                                 ! leaky
!            PP(q)[1] = P(fa,fb)
!
!         else
!
!            fa = ncomp_up_to(.a.l-1)
!            fb = ncomp_up_to(.b.l-1)
!            la = fa + sh.a.n_comp
!            lb = fb + sh.b.n_comp
!
!            ax = nx(fa+1:la); ay = ny(fa+1:la); az = nz(fa+1:la)
!            bx = nx(fb+1:lb); by = ny(fb+1:lb); bz = nz(fb+1:lb)
!
!            PP(q).element.create(sh.n_gaussians)                    ! leaky
!            ab => PP(q).element
!
!            ab = ZERO
!
!            if (sh.a.l > sh.b.l) then
!
!               if (atom_a/=aa OR atom_b/=bb) then
!                  aa = atom_a; bb = atom_b
!                  R = sh.a.position - sh.b.position
!                  R1 = R(1);   R2 = R(2);   R3 = R(3)
!                  T1 = R1;     T2 = R2;     T3 = R3
!                  Rx(1) = ONE; Ry(1) = ONE; Rz(1) = ONE
!                  do i = 2,sh.l_max+1
!                     Rx(i) = T1; T1 = R1*T1
!                     Ry(i) = T2; T2 = R2*T2
!                     Rz(i) = T3; T3 = R2*T3
!                  end
!               end
!
!               do b = 1,sh.b.n_comp
!                  xb = bx(b); yb = by(b); zb = bz(b)
!                  binbx => binomial(bx).element
!                  binby => binomial(by).element
!                  binbz => binomial(bz).element
!                  do a = 1,sh.a.n_comp
!                     xa = ax(a); ya = ay(a); za = az(a)
!                     do x = 1,bx
!                        xx = xa + x - 1
!                        fx = binbx(x)*Rx(bx-x+1)
!                        do y = 1,by
!                           yy = ya + y - 1
!                           fy = binby(y)*Ry(by-y+1)
!                           do z = 1,bz
!                              zz = za + z - 1
!                              fz = binbz(z)*Rz(bz-z+1)
!                              i = index_of(xx,yy,zz) - fa
!                              ab(i) = ab(i) + fx*fy*fz
!                           end
!                        end
!                     end
!                  end
!               end
!
!            else
!
!               if (atom_a/=aa OR atom_b/=bb) then
!                  aa = atom_a; bb = atom_b
!                  R = sh.b.position - sh.a.position
!                  R1 = R(1);   R2 = R(2);   R3 = R(3)
!                  T1 = R1;     T2 = R2;     T3 = R3
!                  Rx(1) = ONE; Ry(1) = ONE; Rz(1) = ONE
!                  do i = 2,sh.l_max+1
!                     Rx(i) = T1; T1 = R1*T1
!                     Ry(i) = T2; T2 = R2*T2
!                     Rz(i) = T3; T3 = R2*T3
!                  end
!               end
!
!               do a = 1,sh.a.n_comp
!                  xa = ax(a); ya = ay(a); za = az(a)
!                  binax => binomial(ax).element
!                  binay => binomial(ay).element
!                  binaz => binomial(az).element
!                  do b = 1,sh.b.n_comp
!                     xb = bx(b); yb = by(b); zb = bz(b)
!                     do x = 1,ax
!                        xx = xb + x - 1
!                        fx = binax(x)*Rx(ax-x+1)
!                        do y = 1,ay
!                           yy = yb + y - 1
!                           fy = binay(y)*Ry(ay-y+1)
!                           do z = 1,az
!                              zz = zb + z - 1
!                              fz = binaz(z)*Rz(az-z+1)
!                              i = index_of(xx,yy,zz) - fb
!                              ab(i) = ab(i) + fx*fy*fz
!                           end
!                        end
!                     end
!                  end
!               end
!            end
!
!         end
!
!         sh.destroy_ptr_part
!
!      end
!
!      Rz.destroy
!      Ry.destroy
!      Rx.destroy
!
!   end

!   test_rchf_S
!   ! Test rchf S
!      S,D,T,U :: MAT{REAL}@
!      n_bf :: INT
!      .cluster.create_atom_list
!      .cluster.atom.make_index_info
!      n_bf = .cluster.atom.no_of_basis_functions
!      S.create(n_bf,n_bf)
!      ! .make_rxhf_overlap_mx(S)
!      stdout.text("rchf S:")
!      stdout.put(S)
!      S.destroy
!   end

!   test_inv_sqrt_S
!   ! Test inv sqrt S routine
!      S,D,T,U :: MAT{REAL}@
!      S.create(.n_bf,.n_bf)
!      D.create(.n_bf,.n_bf)
!      T.create(.n_bf,.n_bf)
!      U.create(.n_bf,.n_bf)
!      .make_overlap_mx(S)
!    ! U.to_unit_mx
!    ! D = S/NINE - U
!    ! stdout.text("delta")
!    ! stdout.put(D)
!    ! T = matmul(D,D)
!    ! D = T
!    ! stdout.text("delta^2")
!    ! stdout.put(D)
!    ! T = matmul(D,D)
!    ! D = T
!    ! stdout.text("delta^4")
!    ! stdout.put(D)
!    ! T = matmul(D,D)
!    ! D = T
!    ! stdout.text("delta^8")
!    ! stdout.put(D)
!      D.to_power_product_inv_sqrt_of(S,tol=TOL(10),prefactor=ONE/FOUR)
!      U = matmul(D,matmul(S,D))
!      stdout.text("check")
!      stdout.put(U)
!      stdout.show("is unit matrix =",U.is_unit_mx)
!      D.to_power_product_inverse_of(S,tol=TOL(10),prefactor=ONE/FOUR)
!      U = matmul(D,S)
!      stdout.text("check")
!      stdout.put(U)
!      stdout.show("is unit matrix =",U.is_unit_mx)
!      U.destroy
!      T.destroy
!      D.destroy
!      S.destroy
!   end

!   test_E_field_matrices
!   ! Test the E field matrices
!      Exx,Eyy,Ezz,Exy,Exz,Eyz :: MAT{REAL}@
!      Exp,Eyp,Ezp,Exm,Eym,Ezm,NN :: MAT{REAL}@
!      c :: VEC{REAL}(3)
!      Exp.create(.n_bf,.n_bf); Eyp.create(.n_bf,.n_bf); Ezp.create(.n_bf,.n_bf)
!      Exm.create(.n_bf,.n_bf); Eym.create(.n_bf,.n_bf); Ezm.create(.n_bf,.n_bf)
!      Exx.create(.n_bf,.n_bf); Eyy.create(.n_bf,.n_bf); Ezz.create(.n_bf,.n_bf)
!      Exy.create(.n_bf,.n_bf); Exz.create(.n_bf,.n_bf); Eyz.create(.n_bf,.n_bf)
!      NN.create(.n_bf,.n_bf)
!      c = [ZERO,ZERO,ZERO]
!      ! c = [ZERO,ZERO+TOL(5),ZERO]
!      .make_electric_field_matrices(Exp,Eyp,Ezp,c)
!      ! c = [ZERO,ZERO-TOL(5),ZERO]
!      ! .make_electric_field_matrices(Exm,Eym,Ezm,c)
!      ! c = [ZERO,ZERO,ZERO]
!      ! .make_EFG_matrices(Exx,Eyy,Ezz,Exy,Exz,Eyz,c)
!      ! Eyy = (Exp-Exm)/(TWO*TOL(5))
!      .make_NE_mx(NN)
!      stdout.text("Ex"); stdout.put(Exp)
!      ! stdout.text("numerical Exy 2"); stdout.put(Eyy)
!      stdout.text("numerical Ex"); stdout.put(NN)
!      Exp.destroy; Eyp.destroy; Ezp.destroy
!      Exm.destroy; Eym.destroy; Ezm.destroy
!      Exx.destroy; Eyy.destroy; Ezz.destroy
!      Exy.destroy; Exz.destroy; Eyz.destroy
!      NN.destroy
!   end

!   make_NE_mx(Z)
!   ! Calculate the one-electron electron nuclear attractio matrix numerically.
!      Z :: MAT{REAL}
!   ENSURE(.basis_info_made, "no basis info")
!   ENSURE(.atom.allocated,  "no atom list")
!   ENSURE(.dftgrid.allocated, "need to specify dftgrid for ZORA")
!     ZZ :: MAT{REAL}@
!     sh :: SHELL2
!     arch :: ARCHIVE
!     q,fa,la,fb,lb :: INT
!     Z = ZERO
!     do q = 1,.n_shell_pairs
!        .get_shell_pair(sh,q,fa,la,fb,lb)
!        ZZ.create(sh.a.n_comp,sh.b.n_comp)
!        .dftgrid.make_mx_elements_of(.E_potential,self,sh,ZZ)
!        Z(fa:la,fb:lb) = Z(fa:la,fb:lb) + ZZ
!        ZZ.destroy
!        sh.destroy_ptr_part
!     end
!     Z.symmetric_reflect
!   end

!   E_potential(values,pts)
!   ! Calculate the electric field "values" at a given set of "pts".
!   ! This is useful for numerical integration.
!      values :: VEC{REAL}
!      pts :: MAT{REAL}
!      n_pts,n,i :: INT
!      r,x,y,z :: REAL
!      c :: VEC{REAL}(3)
!      n_pts = size(pts,1)
!      values = ZERO
!      c = [ZERO,ZERO,ZERO]
!      do i = 1,n_pts
!         r = c.distance_to(pts(i,:))
!         x = c(1)-pts(i,1)
!         y = c(2)-pts(i,2)
!         z = c(3)-pts(i,3)
!         if (r>tiny(r)) then
!           ! values(i) = values(i) + (-TWO*x*x+y*y+z*z)/(r*r*r*r*r)
!           ! values(i) = values(i) + (-THREE*x*y)/(r*r*r*r*r)
!           values(i) = values(i) + x/(r*r*r)
!         else
!           values(i) = values(i) + TEN**6
!         end
!      end
!   end

!   test_ZORA_SO_matrices
!   !
!      SOx,SOy,SOz :: MAT{REAL}@
!      ZOx,ZOy,ZOz, T :: MAT{REAL}@
!      T.create(.n_bf,.n_bf)
!      SOx.create(.n_bf,.n_bf); SOy.create(.n_bf,.n_bf); SOz.create(.n_bf,.n_bf)
!      ZOx.create(.n_bf,.n_bf); ZOy.create(.n_bf,.n_bf); ZOz.create(.n_bf,.n_bf)
!      .make_nuclear_attraction_mx(SOz)
!      .make_ENA_mx(ZOz)
!      stdout.text("NA"); stdout.put(SOz)
!      stdout.text("numerical NA"); stdout.put(ZOz)
!      .get_spin_orbit_matrices(SOx,SOy,SOz)
!      .get_1e_ZORA_matrices(T,ZOx,ZOy,ZOz)
!      stdout.text("SOx"); stdout.put(SOx)
!      stdout.text("ZOx"); stdout.put(ZOx)
!      stdout.text("SOy"); stdout.put(SOy)
!      stdout.text("ZOy"); stdout.put(ZOy)
!      stdout.text("SOz"); stdout.put(SOz)
!      stdout.text("ZOz"); stdout.put(ZOz)
!      SOx.destroy; SOy.destroy; SOz.destroy
!      ZOx.destroy; ZOy.destroy; ZOz.destroy
!      T.destroy
!   end
!
!   test_dftgrid
!   !
!      Z,NZ,DZ :: MAT{REAL}@
!      Z.create(.n_bf,.n_bf)
!      NZ.create(.n_bf,.n_bf)
!      DZ.create(.n_bf,.n_bf)
!      .make_nuclear_attraction_mx(DZ,output=TRUE)
!      .make_nuclear_ints(Z)
!      .get_ENA_mx(NZ)
!      stdout.text("DZ"); stdout.put(DZ)
!      stdout.text("Z"); stdout.put(Z)
!      stdout.text("NZ"); stdout.put(NZ)
!      .test_num
!      Z.destroy; NZ.destroy; DZ.destroy
!   end
!
!   make_nuclear_ints(Z,output)
!   ! Calculate the nuclear attraction matrix "Z"
!   ! if output is present and FALSE then do not make output archive
!      Z :: MAT{REAL}
!     output :: BIN, OPTIONAL
!     do_output :: BIN
!     atom :: ATOM@
!     Z_c :: MAT{REAL}@
!     q,c,fa,la,fb,lb :: INT
!     shell :: SHELL2
!     archive :: ARCHIVE
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.allocated,  "no atom list")
!     do_output=TRUE
!     if (present(output)) do_output=output
!     Z = ZERO
!       do q=1,.n_shell_pairs
!         .get_shell_pair(shell,q,fa,la,fb,lb)
!         Z_c.create(shell.a.n_comp,shell.b.n_comp)
!         do c=1,.n_atom
!           atom=>.atom(c)
!         ! shell.get_nuc(Z_c, atom.mass,atom.position)
!           shell.make_nuclear_attraction_ints(Z_c, atom.position)
!           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom.atomic_number * Z_c
!         end
!         Z_c.destroy
!         shell.destroy_ptr_part
!       end
!     Z.make_symmetric
!     if (do_output) then
!       archive.set(.name,"nuc_mx")
!       archive.write(Z)
!     end
!   end
!
!   test_num
!   !
!     ZZ :: MAT{REAL}@
!     q,fa,la,fb,lb :: INT
!     shell :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.allocated,  "no atom list")
!   ENSURE( .dftgrid.allocated, "need to specify dftgrid for ZORA")
!        q = 529
!        .get_shell_pair(shell,q,fa,la,fb,lb)
!        write(*,*) "fa=",fa,"la=",la,"fb=",fb,"lb=",lb
!        ZZ.create(shell.a.n_comp,shell.b.n_comp)
!        .dftgrid.make_mx_elements_of(nuc_pot,self,shell,ZZ)
!        stdout.text("ZZ")
!        stdout.put(ZZ)
!        ZZ.destroy
!        shell.destroy_ptr_part
!   end
!
!   nuc_pot(values,pts)
!   ! Calculate the nuclear potential "values" of a given set of "pts".
!   ! This is usefule for numerical integration of nuclear attraction integrals.
!      values :: VEC{REAL}
!      pts :: MAT{REAL}
!      n_pts,n,i :: INT
!      Z_n,r_ni :: REAL
!      pos :: VEC{REAL}(3)
!      n_pts = size(pts,1)
!      values = ZERO
!      do n = 1,.n_atom
!         Z_n = .atom(n).atomic_number
!         pos = .atom(n).position
!         do i = 1,n_pts
!            r_ni = pos.distance_to(pts(i,:))
!            values(i) = values(i) + Z_n/r_ni
!         end
!      end
!   end
!
!   get_ENA_mx(Z)
!   ! Set "Z" to the numerically calculated electron nuclear attraction matrix.
!   ! If archives exist, read them; otherwise make them.
!       Z :: MAT{REAL}
!      archive :: ARCHIVE
!      archive.set(.name,"ENA_mx")
!      if (archive.exists) then
!         archive.read(Z)
!      else
!         .make_ENA_mx(Z)
!      end
!   end
!
!   integrate_property(make_property_grid) result (res)
!   ! Integrate a scalar molecular property, which is represented by a subroutine
!   ! "make_property_grid" which returns "values" of the property in a vector,
!   ! given "mol" as the molecule, and "pts" as a set of points. The result of
!   ! the integration is "res".
!      interface
!         make_property_grid(mol,values,pts)
!            mol :: MOLECULE
!            values :: VEC{REAL}
!            pts :: MAT{REAL}
!         end
!      end
!      res :: REAL
!   ENSURE( .dftgrid.allocated,"no DFT grid information!")
!!      res = .dftgrid.integrate_molecular_property(make_property_grid,self)
!      res = ZERO
!   end
!
!   integrate_rho_numerically
!   ! Integrate the density numerically
!      res :: REAL
!      res = .integrate_property(make_density_grid_1)
!      stdout.show("numerically integrated charge =",res)
!   end
!
!   test_eigen
!   !
!      n,i :: INT
!      dot :: REAL
!       m :: MAT{REAL}@
!      eval :: VEC{REAL}@
!      L,R,w :: MAT{REAL}@
!      stdin.read(n)
!      m.create(n,n)
!      stdin.read(m)
!      eval.create(n)
!      L.create(n,n)
!      R.create(n,n)
!      m.solve_general_eigenproblem(eval,L,R)
!      stdout.show("n =",n)
!      stdout.text("eval:")
!      stdout.put(eval)
!      stdout.text("Left eigenvectors:")
!      stdout.put(L)
!      stdout.text("right eigenvectors:")
!      stdout.put(R)
!      w.create(n,n)
!      w.to_product_of(L,R,transpose_a=TRUE)
!      stdout.text("(left)^T x (right) eigenvectors:")
!      stdout.put(w)
!      w.destroy
!      R.destroy
!      L.destroy
!      eval.destroy
!      m.destroy
!   end

!  =============================================================
!  Determinant Variation Perturbation Theory (DVPT) SCF routines
!  =============================================================

!   DVPT_SCF :: recursive, leaky
!   ! Do a restricted determinant variation perturbation theory SCF calculation. The following
!   ! are produced as results: .MOs, .MO_energies, .density_mx
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.allocated,  "no atom list")
!     .make_DVPT_initial_MOs
!     .make_DVPT_density_matrices
!     .make_DVPT_fock_matrices
!     .SCF_data.set(nuclear_repulsion_energy=.nuclear_repulsion_energy)
!     .SCF_data.reset(energy=.DVPT_SCF_energy, kinetic_energy=ZERO)
!
!   ! stdout.text("D:")
!   ! stdout.put(.density_mx.r)
!   ! stdout.text("F:")
!   ! stdout.put(.fock_mx.r)
!   ! .SCF_data.SCF_kind="rhf"
!   ! .SCF_data.reset(energy=.SCF_energy, kinetic_energy=ZERO)
!   ! .SCF_data.SCF_kind="rdvpt"
!
!     .SCF_data.put_banner
!     ! Begin iterations
!     do
!     ! .extrapolate_SCF
!       .update_DVPT_orbitals
!       .schmidt_orthonormalize(.MOs)
!       .make_DVPT_density_matrices
!       .make_DVPT_fock_matrices
!       .save_DVPT_results
!       .SCF_data.update(energy=.DVPT_SCF_energy, kinetic_energy=ZERO)
!       .SCF_data.put_results
!       if (.SCF_data.SCF_done) exit
!     end
!     .SCF_data.cleanup_diis
!     .fock_mx.destroy("restricted")
!   end
!
!   update_DVPT_orbitals
!   ! Solve for the new DVPT orbitals and energies. An initial set of orbitals
!   ! and a Fock matrix must already exist in memory. The new molecular orbitals
!   ! "MO*U" are found from the old MOs solving : (MO^T F MO)U = MO^T S MO U E
!   ! where U is a general matrix (since F is not symmetric).
!     MO_energies :: VEC{REAL}@
!     MO,F :: MAT{REAL}@
!     G,U,left :: MAT{REAL}@
!      i :: INT
!     MO_energies => .MO_energies.r
!     MO => .MOs.r
!     F  => .fock_mx.r
!     G.create(.n_bf,.n_bf)
!     U.create(.n_bf,.n_bf)
!     left.create(.n_bf,.n_bf)
!     G = F
!     G.change_basis_using(MO)
!     ! Level shifting
!     if ( .SCF_data.apply_level_shifting) then
!       do i = .n_a + 1, .n_bf
!         G(i,i) = .SCF_data.level_shift + G(i,i)
!       end
!     end
!     G.solve_general_eigenproblem(MO_energies,left,U)
!     G.to_product_of(MO,U)
!     MO = G
!     left.destroy
!     U.destroy
!     G.destroy
!   end
!
!   save_DVPT_results
!   ! Save the DVPT results in various archives
!     archive :: ARCHIVE
!      m :: INT
!     .save_SCF_results
!     m = .DVPT_order
!     archive.set(.name,"DVPT_mo_"// trim(m.to_str))
!     archive.write(.MOs.r)
!   end
!
!   make_DVPT_initial_MOs :: leaky
!   ! Make the DVPT initial orbitals. Currently this is just the same
!   ! as the usual initial guess, but the MO's are written to disk.
!       m :: INT
!      MO :: MAT{REAL}@
!      m_archive :: ARCHIVE
!      m = .DVPT_order
!      m_archive.set(.name,"DVPT_mo_"// trim(m.to_str))
!      .SCF_data.SCF_kind = "rhf"
!      .get_initial_guess
!      .SCF_data.SCF_kind = "rdvpt"
!      MO => .MOs.r
!      m_archive.write(MO)
!   end
!
!   DVPT_order result (X)
!   ! Determine the DVPT order by looking for the highest number "X" for
!   ! which a DVPT_mo_X archive file exists.
!       X :: INT
!      archive :: ARCHIVE
!      order :: INT = 999
!      if (order/=999) then
!         X = order
!      else
!         X = 0
!         do
!            archive.set(.name,"DVPT_mo_"// trim(X.to_str))
!            if (NOT archive.exists) exit
!            X = X + 1
!         end
!         order = X
!      end
!   end
!
!   make_DVPT_density_matrices
!   ! Make all the DVPT density matrices up to the required order
!   ! NOTE: the indice order is opposite to the paper.
!      m,j :: INT
!      m = .DVPT_order
!      do j = 0,m
!         .make_DVPT_density_mx(m,j)
!      end
!      .make_DVPT_eff_density_mx
!   !  .SCF_data.SCF_kind = "rhf"
!   !  .make_SCF_density_mx
!   !  .SCF_data.SCF_kind = "rdvpt"
!   end
!
!   make_DVPT_eff_density_mx
!   ! Make the effective DVPT fock matrix
!      m,i,j :: INT
!      fac,sum,det :: REAL
!      DD,D :: MAT{REAL}@
!      d_archive :: ARCHIVE
!      .density_mx.r.destroy
!      DD.create(.n_bf,.n_bf); DD = ZERO
!      D.create(.n_bf,.n_bf)
!      m   = .DVPT_order
!      do i = 0,m
!      do j = 0,i
!         fac = TWO
!         if (i==j) fac = ONE
!         .get_DVPT_density_mx(D,det,i,j)
!         sum = sum + fac*det
!         D = fac*D
!         DD.plus(D)
!      end
!      end
!      DD = DD/sum
!      d_archive.set(.name,"DVPT_density_mx")
!      d_archive.write(DD)
!      .density_mx.r => DD
!      D.destroy
!   end
!
!   get_DVPT_density_mx(D,determinant,left,right)
!   ! Get the determinant variation perturbation theory transition density matrix
!   ! for a left determinant orbitals "left" and  a right determinant orbitals "right".
!   ! Make the density if the archive does not exist.
!       D :: MAT{REAL}
!      determinant :: REAL
!      left,right :: INT
!      d_archive :: ARCHIVE
!      label :: STR
!      label = trim(left.to_str) // trim(right.to_str)
!      d_archive.set(.name,"DVPT_density_"// label)
!      if (NOT d_archive.exists) .make_DVPT_density_mx(left,right)
!      d_archive.read(D)
!      d_archive.set(.name,"DVPT_determinant_"// trim(left.to_str) // trim(right.to_str))
!      d_archive.read(determinant)
!   end
!
!   make_DVPT_density_mx(left,right)
!   ! Make the determinant variation perturbation theory transition density matrix
!   ! for a left determinant orbitals "left" and  a right determinant orbitals "right".
!   ! The density and determinant are stored as archives on disk.
!      left,right :: INT
!      D,L,R :: MAT{REAL}@
!      l_archive,r_archive,d_archive :: ARCHIVE
!      determinant :: REAL
!      D.create(.n_bf,.n_bf)
!      l_archive.set(.name,"DVPT_mo_"// trim(left.to_str))
!      r_archive.set(.name,"DVPT_mo_"// trim(right.to_str))
!   ENSURE(l_archive.exists, "no DVPT mo archive, no. ="// trim(left.to_str))
!   ENSURE(r_archive.exists, "no DVPT mo archive, no. ="// trim(right.to_str))
!      L.create(.n_bf,.n_bf)
!      R.create(.n_bf,.n_bf)
!      l_archive.read(L)
!      r_archive.read(R)
!      ! Make density matrix using occupied orbitals only
!      .make_DVPT_density_mx(D,determinant,L(:,:.n_a),R(:,:.n_a))
!
!      !       stdout.show("det =",determinant)
!      !       stdout.text("MO:")
!      !       stdout.put(L(:,.n_a))
!      !       stdout.text("D:")
!      !       stdout.put(TWO*D)
!
!      R.destroy
!      L.destroy
!      d_archive.set(.name,"DVPT_density_"// trim(left.to_str) // trim(right.to_str))
!      d_archive.write(D)
!      d_archive.set(.name,"DVPT_determinant_"// trim(left.to_str) // trim(right.to_str))
!      d_archive.write(determinant)
!      D.destroy
!   end
!
!   make_DVPT_density_mx(D,determinant,left,right)
!   ! Make the determinant variation perturbation theory transition density matrix "D" for a
!   ! left determinant of occupied orbitals "left" and a right determinant of occupied
!   ! orbitals "right". Also return the "determinant" of the orbital ovarlap matrix.
!       D :: MAT{REAL}
!      left,right :: MAT{REAL}
!      determinant :: REAL
!      S,O,C :: MAT{REAL}@
!      dim :: INT
!   ENSURE(size(D,1)==size(D,2),"D is not square")
!   ENSURE(size(D,1)==.n_bf,"D has wrong size")
!   ENSURE(size(left,1) ==.n_bf,"left has wrong size")
!   ENSURE(size(right,1)==.n_bf,"right has wrong size")
!   ENSURE(size(left,2)==size(right,2),"left and and right are incompatible")
!      dim = size(left,2)
!      S.create(dim,dim)
!      O.create(.n_bf,.n_bf)
!      .make_overlap_mx(O)
!      O.change_basis_using(S,left,right)
!      O.destroy
!      C.create(dim,dim)
!      .make_cofactor(C,determinant,S)
!      C.back_transform_to(D,right,left)
!      C.destroy
!      S.destroy
!   end
!
!   make_cofactor(C,determinant,S)
!   ! Make the cofactor matrix "C" and "determinant" for the matrix "S".
!   ! Note: this constructs the alpha-alpha part of the cofactor, but the
!   ! determinant is that for the full overlap matrix
!      C,S :: MAT{REAL}
!      determinant :: REAL
!      eval,n :: VEC{REAL}@
!      left,right,W :: MAT{REAL}@
!      dim,i :: INT
!      dot :: REAL
!   ENSURE(size(C,1)==size(C,2),"C is not square")
!   ENSURE(size(S,1)==size(S,2),"incorrect size for array S")
!   ENSURE(size(S,1)==size(C,1),"array C and array S incompatible")
!      dim = size(C,1)
!      left.create(dim,dim)
!      right.create(dim,dim)
!      eval.create(dim)
!      ! Get the eigenvalues and determinant of S
!      S.solve_general_eigenproblem(eval,left,right)
!      determinant = product(eval)
!      determinant = determinant*determinant ! include beta part
!      ! Make the cofactor
!      n.create(dim)
!      n = determinant/eval
!      W.create(dim,dim)
!      W.to_product_with_diagonal(n,left,transpose_a=TRUE)
!      C = matmul(right,W)
!
!    ! stdout.text("=========make_cofactor==============")
!    ! stdout.text("S:")
!    ! stdout.put(S)
!    ! stdout.text("n:")
!    ! stdout.put(n)
!    ! stdout.text("L:")
!    ! stdout.put(left)
!    ! stdout.text("R:")
!    ! stdout.put(right)
!    ! W = matmul(right,transpose(left))
!    ! stdout.text("R L^T:")
!    ! stdout.put(W)
!    ! W = matmul(transpose(left),right)
!    ! stdout.text("L^T R:")
!    ! stdout.put(W)
!    ! stdout.text("C:")
!    ! stdout.put(C)
!    ! stdout.text("====================================")
!
!      W.destroy
!      n.destroy
!      eval.destroy
!      right.destroy
!      left.destroy
!   end
!
!   make_DVPT_fock_matrices :: leaky
!   ! Make the DVPT fock matrices up to the required order,
!   ! including the effective fock matrix
!      m,j :: INT
!      m = .DVPT_order
!      do j = 0,m
!         .make_DVPT_fock_mx(m,j)
!      end
!      .make_DVPT_eff_fock_mx
!   !  .SCF_data.SCF_kind = "rhf"
!   !  .make_fock_mx
!   !  .SCF_data.SCF_kind = "rdvpt"
!   end
!
!   make_DVPT_eff_fock_mx :: leaky
!   ! Make the effective DVPT fock matrix
!      m,j :: INT
!      E_m,E_mj,sum,det :: REAL
!      FF,F,W,D :: MAT{REAL}@
!      f_archive :: ARCHIVE
!      .fock_mx.r.destroy
!      FF.create(.n_bf,.n_bf); FF = ZERO
!      F.create(.n_bf,.n_bf)
!      W.create(.n_bf,.n_bf)
!      D.create(.n_bf,.n_bf)
!      m   = .DVPT_order
!      E_m = .DVPT_electronic_energy
!      sum = ZERO
!      do j = 0,m
!         .get_DVPT_fock_mx(F,m,j)
!         .get_DVPT_density_mx(D,det,m,j)
!         sum = sum + det
!         det = 1/(TWO*det)
!         FF.plus(F)
!         .add_core_hamiltonian(FF)
!         E_mj = E_m + det*F.trace_of_product(D)
!         .make_overlap_mx(F)
!         W.to_product_of(D,F)
!         D.to_product_of(F,W)
!         FF = FF - E_mj*D
!      end
!      FF = FF/sum
!      f_archive.set(.name,"DVPT_effective_fock_"// trim(m.to_str))
!      f_archive.write(FF)
!      .fock_mx.r => FF
!      D.destroy
!      W.destroy
!      F.destroy
!   end
!
!   get_DVPT_fock_mx(F,left,right)
!   ! Get the determinant variation perturbation theory transition Fock matrix
!   ! for a left determinant orbitals "left" and  a right determinant orbitals "right".
!       F :: MAT{REAL}
!      left,right :: INT
!      f_archive :: ARCHIVE
!      label :: STR
!      label = trim(left.to_str) // trim(right.to_str)
!      f_archive.set(.name,"DVPT_fock_"// label)
!      if (NOT f_archive.exists) .make_DVPT_fock_mx(left,right)
!      f_archive.read(F)
!   end
!
!   make_DVPT_fock_mx(left,right)
!   ! Make the determinant variation perturbation theory transition fock matrix
!   ! for a left determinant orbitals "left" and  a right determinant orbitals "right".
!   ! The Fock matrix is stored as an archive on disk.
!      left,right :: INT
!       F :: MAT{REAL}@
!      f_archive :: ARCHIVE
!      label :: STR
!      F.create(.n_bf,.n_bf)
!      .make_DVPT_fock_mx(F,left,right)
!      label = trim(left.to_str) // trim(right.to_str)
!      f_archive.set(.name,"DVPT_fock_"// label)
!      f_archive.write(F)
!      F.destroy
!   end
!
!   make_DVPT_fock_mx(F,left,right)
!   ! Make the determinant variation perturbation theory transition fock matrix for
!   ! a left determinant of orbitals "left" and a right determinant of orbitals
!   ! "right". If present, "D" is used as the density matrix.
!       F :: MAT{REAL}
!      left,right :: INT
!       P :: MAT{REAL}@
!      determinant,fac :: REAL
!      P.create(.n_bf,.n_bf)
!      .get_DVPT_density_mx(P,determinant,left,right)
!   !  .make_r_asymmetric_fock(P,F,direct=FALSE,core=FALSE,r12=TRUE)
!      .make_r_fock(P,F,direct=FALSE,core=FALSE,r12=TRUE)
!      fac = TWO/determinant
!      F = fac*F
!      P.destroy
!   end
!
!   make_r_fock(P,F,direct,core,r12) :: leaky
!   ! Make a new restricted Fock matrix "F" from the density matrix "P".
!   ! If present and TRUE , "direct" means calculate integrals on the fly
!   ! If present and FALSE, "core" removes the core matrix contribution
!   ! If present and FALSE, "r12"  removes the two electron contribution
!     direct,core,r12 :: BIN, optional
!     P,F :: MAT{REAL}
!     J,K :: MAT{REAL}@
!     do_direct,add_core,add_r12 :: BIN
!
!     do_direct= FALSE
!     add_core = TRUE
!     add_r12  = TRUE
!     if (present(direct)) do_direct = direct
!     if (present(core))   add_core = core
!     if (present(r12))    add_r12  = r12
!     if (add_r12) then
!        J.create(.n_bf,.n_bf)
!        K.create(.n_bf,.n_bf)
!        if (do_direct) then; .make_r_JK_direct(J,K,P)
!        else;                .make_r_JK_disk(J,K,P)
!        end
!        F = J - HALF*K
!        K.destroy
!        J.destroy
!     else
!        F = ZERO
!     end
!     if (add_core) .add_core_hamiltonian(F)
!   end
!
!   make_r_asymmetric_fock(P,F,direct,core,r12) :: leaky
!   ! Make a new restricted Fock matrix "F" from an asymmetric density matrix "P".
!   ! If present and TRUE , "direct" means calculate integrals on the fly
!   ! If present and FALSE, "core" removes the core matrix contribution
!   ! If present and FALSE, "r12"  removes the two electron contribution
!     direct,core,r12 :: BIN, optional
!     P,F :: MAT{REAL}
!     J,K :: MAT{REAL}@
!     do_direct,add_core,add_r12 :: BIN
!   ENSURE(size(F,1)==.n_bf, "Fock matrix dimensions wrong")
!   ENSURE(size(P,1)==.n_bf, "Density matrix dimensions wrong")
!     do_direct= FALSE
!     add_core = TRUE
!     add_r12  = TRUE
!     if (present(direct)) do_direct = direct
!     if (present(core))   add_core = core
!     if (present(r12))    add_r12  = r12
!     if (add_r12) then
!        J.create(.n_bf,.n_bf)
!        K.create(.n_bf,.n_bf)
!        if (do_direct) then; .make_r_asymmetric_JK_direct(J,K,P)
!        else;                .make_r_asymmetric_JK_disk(J,K,P)
!        end
!        F = J - HALF*K
!        K.destroy
!        J.destroy
!     else
!        F = ZERO
!     end
!     if (add_core) then
!       .add_core_hamiltonian(F)
!     end
!   end
!
!   make_r_asymmetric_JK_direct(J,K,P)
!   ! Make the real coulomb matrices "J" and exchange matrix "K"  from an
!   ! asymmetric density matrix "P" directly from the integrals.
!     J,K,P :: MAT{REAL}
!      I :: MAT4{REAL}@
!     shell4 :: SHELL4@
!     I_max :: VEC{REAL}@
!     ac,ab,cd,aa,cc :: INT
!     a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld :: INT
!     cutoff,I_abcd,factor :: REAL
!     skip :: BIN
!     J = ZERO
!     K = ZERO
!     cutoff = .SCF_data.eri_schwarz_cutoff
!     shell4.create
!     I_max.create(.n_shell_pairs)
!     .make_max_abab_integrals(I_max)
!     do ab = 1,.n_shell_pairs
!       .get_shell_pair(ab,a,b)
!       fa = .first_basis_fn_for_shell(a)
!       fb = .first_basis_fn_for_shell(b)
!       la = .last_basis_fn_for_shell(a)
!       lb = .last_basis_fn_for_shell(b)
!       do cd = 1,ab
!         .get_shell_pair(cd,c,d)
!         fc = .first_basis_fn_for_shell(c)
!         fd = .first_basis_fn_for_shell(d)
!         lc = .last_basis_fn_for_shell(c)
!         ld = .last_basis_fn_for_shell(d)
!         .get_shell_quartet(shell4,a,b,c,d)
!         skip = .schwarz_inequality_test(cutoff,ab,cd,fa,la,fb,lb,fc,lc,fd,ld,P,I_max)
!         if (skip) then
!            shell4.destroy_ptr_part
!            cycle
!         end
!         I.create(fa,la,fb,lb,fc,lc,fd,ld)
!         shell4.get_ERI(I)
!         factor = ONE                                ! Evaluate the integrals
!         if (a==b)          factor = HALF            ! Coincidence factors
!         if (c==d)          factor = HALF * factor
!         if (a==c AND b==d) factor = HALF * factor
!         do d = fd,ld
!         do c = fc,lc
!         do b = fb,lb
!         do a = fa,la
!            I_abcd = factor * I(a,b,c,d)
!            J(a,b) = J(a,b) + I_abcd*(P(d,c)+P(c,d)) ! These are symmetric
!            J(c,d) = J(c,d) + I_abcd*(P(b,a)+P(a,b))
!            K(a,d) = K(a,d) + I_abcd*P(b,c)          ! These are asymmetric
!            K(a,c) = K(a,c) + I_abcd*P(b,d)
!            K(b,c) = K(b,c) + I_abcd*P(a,d)
!            K(b,d) = K(b,d) + I_abcd*P(a,c)
!            K(d,a) = K(d,a) + I_abcd*P(c,b)
!            K(c,a) = K(c,a) + I_abcd*P(d,b)
!            K(c,b) = K(c,b) + I_abcd*P(d,a)
!            K(d,b) = K(d,b) + I_abcd*P(c,a)
!         end
!         end
!         end
!         end
!         I.destroy
!         shell4.destroy_ptr_part
!       end
!     end
!     I_max.destroy
!     shell4.destroy
!     .weight_diagonal_blocks(J,TWO)
!     J.make_symmetric
!   end
!
!   make_r_asymmetric_JK_disk(J,K,P)
!   ! Make the real coulomb matrices "J" and exchange matrix "K"  from an
!   ! asymmetric density matrix "P" using disk integrals.
!     J,K,P :: MAT{REAL}
!     eri_archive,ind_archive :: ARCHIVE
!      I :: MAT4{REAL}@
!     q,n_quartets,a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld :: INT
!     I_abcd :: REAL
!     eri_archive.create(.name,"eri_integrals")
!     ind_archive.create(.name,"eri_index")
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.allocated,  "no atom list")
!   ENSURE(eri_archive.exists, "no integral file")
!   ENSURE(ind_archive.exists, "no integral index file")
!     eri_archive.open_for("read-only",buffered=TRUE,type="real")
!     ind_archive.open_for("read-only",buffered=TRUE,type="int")
!     J = ZERO
!     K = ZERO
!     n_quartets = .n_shell_quartets
!     do
!        ind_archive.file.read(q)
!        if (q > n_quartets) exit
!        .get_shell_quartet(q,fa,la,fb,lb,fc,lc,fd,ld)
!        I.create(fa,la,fb,lb,fc,lc,fd,ld)
!        eri_archive.file.read(I)
!        do d = fd,ld
!        do c = fc,lc
!        do b = fb,lb
!        do a = fa,la
!           I_abcd = I(a,b,c,d)
!           J(a,b) = J(a,b) + I_abcd*(P(d,c)+P(c,d)) ! These are symmetric
!           J(c,d) = J(c,d) + I_abcd*(P(b,a)+P(a,b))
!           K(a,d) = K(a,d) + I_abcd*P(b,c)          ! These are asymmetric
!           K(a,c) = K(a,c) + I_abcd*P(b,d)
!           K(b,c) = K(b,c) + I_abcd*P(a,d)
!           K(b,d) = K(b,d) + I_abcd*P(a,c)
!           K(d,a) = K(d,a) + I_abcd*P(c,b)
!           K(c,a) = K(c,a) + I_abcd*P(d,b)
!           K(c,b) = K(c,b) + I_abcd*P(d,a)
!           K(d,b) = K(d,b) + I_abcd*P(c,a)
!        end
!        end
!        end
!        end
!        I.destroy
!     end
!     ind_archive.destroy
!     eri_archive.destroy
!     .weight_diagonal_blocks(J,TWO)
!     J.make_symmetric
!   end
!
!   get_shell_pair_indices_from(index,a,b)
!   ! Return the actual shell indicies "a" and "b" which map to "index"
!     index :: INT, IN
!     a,b :: INT, OUT
!     a  = (1+sqrt(8.0d0*index-7.0d0))/2
!     b  = index - a*(a-1)/2
!   end
!
!   DVPT_SCF_energy result (res)
!   ! Evaluates the determinant variation perturbation theory SCF energy
!   ! (including the nuclear repulsion contribution)
!     res :: REAL
!     res = .DVPT_electronic_energy + .nuclear_repulsion_energy
!   end
!
!   DVPT_electronic_energy result (res)
!   ! Evaluates the SCF electronic energy
!     res :: REAL
!     SCF_kind :: STR
!     F,D :: MAT{REAL}@
!     m,i,j :: INT
!     fac,det,sum :: REAL
!     m = .DVPT_order
!     SCF_kind = .SCF_data.SCF_kind
!     select case (SCF_kind)
!        case ("rdvpt","restricted_dvpt")
!           F.create(.n_bf,.n_bf)
!           D.create(.n_bf,.n_bf)
!           res = ZERO
!           sum = ZERO
!           do i = 0,m
!           do j = 0,i
!              fac = TWO
!              if (i==j) fac = ONE
!              .get_DVPT_density_mx(D,det,i,j)
!              .get_DVPT_fock_mx(F,i,j)
!              .add_core_hamiltonian(F)
!
!           !  stdout.show("i   =",i)
!           !  stdout.show("j   =",j)
!           !  stdout.show("det =",det)
!           !  stdout.text("D:")
!           !  stdout.put(TWO*D)
!           !  stdout.text("F:")
!           !  stdout.put(F)
!
!              .add_core_hamiltonian(F)
!              res = res + fac*F.trace_of_product(D)
!              sum = sum + fac*det
!           end
!           end
!           res = res/sum
!           D.destroy
!           F.destroy
!        case default; DIE("not yet implemented, " // .SCF_data.SCF_kind)
!     end
!   end

!   make_spin_b_field_grid(bb,pt,Dx,Dy,Dz)
!   ! Make the magnetic B field "bb" due to the spin density at a set of points
!   ! given in "pt" using three real symmetric AO density matrices "Dx", "Dy" and
!   ! "Dz" corresponding to the Sx, Sy and Sz densities. Note: the contribution
!   ! is added to whatever is already in bb. Note: factor of half in the
!   ! densities is assumed.
!      bb,pt :: MAT{REAL}
!      Dx,Dy,Dz :: MAT{REAL}
!   ENSURE(.basis_info_made,"no basis set")
!   ENSURE(.atom.allocated,"no atom list")
!   ENSURE(pt.dim2==3,"no basis set")
!   ENSURE(Dx.dim1==.n_bf AND Dx.is_square,"wrong shape for Dx")
!   ENSURE(Dy.dim1==.n_bf AND Dy.is_square,"wrong shape for Dy")
!   ENSURE(Dz.dim1==.n_bf AND Dz.is_square,"wrong shape for Dz")
!      DD :: MAT3{REAL}@
!      M :: MAT4{REAL}@
!      Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz :: MAT{REAL}@
!      q,fa,fb,la,lb,n, n_pt, i,j,k :: INT
!      eps :: MAT3{REAL}(3,3,3)
!      fac :: REAL
!      shell :: SHELL2
!      n_pt = size(pt,1)
!      bb = ZERO
!      do q = 1,.n_shell_pairs
!        .get_shell_pair(shell,q,fa,la,fb,lb)
!        DD.create(shell.b.n_comp,shell.a.n_comp,3)
!        DD(:,:,1) = Dx(fb:lb,fa:la)
!        DD(:,:,2) = Dy(fb:lb,fa:la)
!        DD(:,:,3) = Dz(fb:lb,fa:la)
!        fac = ONE; if (fa==fb) fac=HALF
!        M.create(shell.a.n_comp,shell.b.n_comp,3,3)
!        Mxx => M(:,:,1,1); Mxy => M(:,:,1,2); Mxz => M(:,:,1,3)
!        Myx => M(:,:,2,1); Myy => M(:,:,2,2); Myz => M(:,:,2,3)
!        Mzx => M(:,:,3,1); Mzy => M(:,:,3,2); Mzz => M(:,:,3,3)
!        do n = 1,n_pt
!          shell.make_magnetic_S_ints(Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz,pt(n,:))
!          do i = 1,3
!          do j = 1,3
!          do k = 1,3
!             eps(i,j,k) = M(:,:,i,j).trace_product_with(DD(:,:,k))
!          end
!          end
!          end
!          bb(n,1) = bb(n,1) + eps(:,:,1).trace - eps(1,:,:).trace
!          bb(n,2) = bb(n,2) + eps(:,:,2).trace - eps(2,:,:).trace
!          bb(n,3) = bb(n,3) + eps(:,:,3).trace - eps(3,:,:).trace
!        end
!        M.destroy
!        DD.destroy
!        shell.destroy_ptr_part
!      end
!   !  bb.zero_small_values(TOL(10))
!      bb = -bb ! this is -g_e mu_b; the final answer is in units of mu_0/4pi
!   end

!   make_spin_b_field
!   ! Make the magnetic B field "bb" due to the spin density at a set of points
!   ! given in "pt"
!      self :: target
!      B,pt,Dx,Dy,Dz :: MAT{REAL}@
!      dmc :: MAT3{CPX}@
!      aa,bb :: REAL
!      xv :: VEC{REAL}(3)
!      n_pt,i,j,k,n :: INT
!      n_pt = 60*75
!      n_pt = 56
!      dmc.create(160,160,4)
!      ! Read the density matrix
!      open(unit=13,file="opdens",status="old")
!      do i=1,160
!      do j=1,160
!      do k=1,4
!         read(13,*) dmc(i,j,k)
!      end
!      end
!      end
!      close(13)
!      ! Make the points array
!      pt.create(n_pt,3)
!      aa=7.4/sqrt(2.0)
!!      bb=0.2/sqrt(2.0)
!      bb=2./sqrt(2.0)
!      xv(1)=aa
!      xv(2)=-xv(1)
!      n = 0
!!      do i=1,75
!      do i=1,8
!        xv(3)=5.4
!   !     do j=1,59
!        do j=1,6
!          n = n + 1
!          pt(n,:) = xv
!    !      xv(3)=xv(3)-0.2
!          xv(3)=xv(3)-2.
!        end
!        n = n + 1
!        pt(n,:) = xv ! make one more point
!        xv(1)=xv(1)-bb
!        xv(2)=-xv(1)
!      end
!      Dx.create(160,160)
!      Dy.create(160,160)
!      Dz.create(160,160)
!      Dx =  dmc(:,:,2) + dmc(:,:,3)  * 0.5     ! ab + ba
!      Dy = (dmc(:,:,2) - dmc(:,:,3)) * (0,0.5) ! -i ab + i ba
!      Dz =  dmc(:,:,1) - dmc(:,:,4)  * 0.5     ! aa - bb
!      B.create(n_pt,3)
!      .make_spin_b_field_grid(B,pt,Dx,Dy,Dz)
!      ! Write out the answer
!      stdout.flush
!      stdout.text("The magnetic field from the spin")
!      stdout.put(B)
!      B.destroy
!      Dz.destroy
!      Dy.destroy
!      Dx.destroy
!      pt.destroy
!   end

!   make_divergence_j_para
!   ! Make the divergence of the paramagnetic current at a set of points
!   ! given in "pt"
!      B,pt,Dx,Dy,Dz :: MAT{REAL}@
!      dmc :: MAT3{CPX}@
!      aa,bb :: REAL
!      xv :: VEC{REAL}(3)
!      n_pt,i,j,k,n :: INT
!      n_pt = 60*75
!      n_pt = 56
!      dmc.create(160,160,4)
!      ! Read the density matrix
!      open(unit=13,file="opdens",status="old")
!      do i=1,160
!      do j=1,160
!      do k=1,4
!         read(13,*) dmc(i,j,k)
!      end
!      end
!      end
!      close(13)
!      ! Make the points array
!      pt.create(n_pt,3)
!      aa=7.4/sqrt(2.0)
!!      bb=0.2/sqrt(2.0)
!      bb=2./sqrt(2.0)
!      xv(1)=aa
!      xv(2)=-xv(1)
!      n = 0
!!      do i=1,75
!      do i=1,8
!        xv(3)=5.4
!   !     do j=1,59
!        do j=1,6
!          n = n + 1
!          pt(n,:) = xv
!    !      xv(3)=xv(3)-0.2
!          xv(3)=xv(3)-2.
!        end
!        n = n + 1
!        pt(n,:) = xv ! make one more point
!        xv(1)=xv(1)-bb
!        xv(2)=-xv(1)
!      end
!      Dx.create(160,160)
!      Dy.create(160,160)
!      Dz.create(160,160)
!      Dx =  dmc(:,:,2) + dmc(:,:,3)  * 0.5     ! ab + ba
!      Dy = (dmc(:,:,2) - dmc(:,:,3)) * (0,0.5) ! -i ab + i ba
!      Dz =  dmc(:,:,1) - dmc(:,:,4)  * 0.5     ! aa - bb
!      B.create(n_pt,3)
!      .make_spin_b_field_grid(B,pt,Dx,Dy,Dz)
!      ! Write out the answer
!      stdout.flush
!      stdout.text("The magnetic field from the spin")
!      stdout.put(B)
!      B.destroy
!      Dz.destroy
!      Dy.destroy
!      Dx.destroy
!      pt.destroy
!   end

!  ============
!  Put routines
!  ============

   subroutine put_oscillator_overlaps(self)
   ! Put out the overlap matrix between the normal and oscillator basis
   ! functions.
      type(MOLECULE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), allocatable :: SAO,SMO
      MAT(REAL,:,:), allocatable :: S,SS,TM
      type(SHELL2_TYPE) :: sh
      VEC(REAL,3) :: ci,cj
      INT :: maxl,n,i,j,k,l,p,q,fa,la,na,fb,lb,nb,nc,nt
   ENSURE(allocated(self%MOs),"MOLECULE.MISC:put_oscillator_overlaps ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.MISC:put_oscillator_overlaps ... no MO's")
   ENSURE(self%basis_info_made,"MOLECULE.MISC:put_oscillator_overlaps ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.MISC:put_oscillator_overlaps ... no atom info")

      ! Get overlap in localized basis
      call make_overlap_mx_(self)
      call create_(SS,self%n_a,self%n_a)
      call change_basis_to_(self%overlap_mx,SS,self%localiser%MO)
      call text_(stdout,"transformed S:")
      call put_(stdout,SS)
      call destroy_(SS)

      ! Make the AO oscillator interals
      ! For l=2 oscillators
      maxl = 2

      ! No. of oscillators
      n = n_comp_up_to_(maxl)
      call create_(SMO,self%n_a,n,self%n_a,n)
      call create_(SAO,self%n_bf,self%n_bf,n,n)
      do i = 1,self%n_a
         do j = 1,self%n_a
            ci = self%localiser%dipole(i,i,:)
            cj = self%localiser%dipole(j,j,:)
            do q = 1,self%n_shell_pairs
               call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
               call make_oscillator_overlap_ints_(sh,SAO(fa:la,fb:lb,:,:),ci,cj,0,maxl,0,maxl)
               call destroy_ptr_part_(sh)
            end do
         ! Transform to the MO basis
            do k = 1,n
               do l = 1,n
                  call symmetric_reflect_(SAO(:,:,k,l))
               end do
            end do
         end do
      end do
      call destroy_(SAO)

      ! Print out
      call flush_(stdout)
      call text_(stdout,"Oscillator overlap matrices, up to l = "//trim(to_str_(maxl)))
      call text_(stdout,"Row = OMO, Col = oscillator fn: s, px, py, pz, ...")
      call flush_(stdout)
      do i = 1,self%n_a
         call show_(stdout,"MO  =",i)
         call put_(stdout,SMO(i,1,:,:))
      end do

      ! Print out
      call create_(SS,n*self%n_a,n*self%n_a)
      SS = reshape(SMO,[n*self%n_a,n*self%n_a])
      call flush_(stdout)
      call text_(stdout,"Ordered oscillator overlap matrix:")
      call flush_(stdout)
      call put_(stdout,SS)

      ! Define the coefficient matrix TM for the projected oscillators
      call create_(TM,n*self%n_a,n*self%n_a)
      call to_unit_mx_(TM)
      call create_(S,n*self%n_a,n*self%n_a)
      nt = self%n_a
      do l = 1,maxl
         nc = n_comp_(l)
         p = nt+1
         q = nt+nc*self%n_a
         call Schmidt_orthonormalize_(TM(:,1:q),SS,from=p,to=nt)
         call symmetrically_orthonormalize_(TM(:,p:q),SS)
         nt = q
      end do

      ! Print out
      call flush_(stdout)
      call text_(stdout,"Orthonormalized oscillator orbitals:")
      call flush_(stdout)
      call put_(stdout,TM)
      call change_basis_to_(SS,S,TM)
      call text_(stdout,"Transformed Oscillator overlap matrix:")
      call put_(stdout,S)

      ! Save the TM array for plots
      self%localiser%TM = TM
      call destroy_(S)
      call destroy_(TM)
      call destroy_(SS)
      call destroy_(SMO)
   end subroutine


!  =====================
!  Hirshfeld atom kappas
!  =====================

   subroutine fit_Hirshfeld_atom_kappas(self)
   ! Fit the kappa scale factors for the Hirshfeld atoms so as to get
   ! the same G and K kinetic energies for each atom.
      type(MOLECULE_TYPE), INOUT :: self
      REAL :: max_shift,max_allowed_shift,shift_convergence,delta
      VEC(REAL,:), allocatable :: kappa,kappa_old,kappa_del,G
      MAT(REAL,:,:), allocatable :: H
      INT :: iter,max_iter
      BIN :: finished
   ENSURE(allocated(self%atom),"MOLECULE.MISC:fit_Hirshfeld_atom_kappas ... no atoms to fit")
   ENSURE(allocated(self%crystal),"MOLECULE.MISC:fit_Hirshfeld_atom_kappas ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.MISC:fit_Hirshfeld_atom_kappas ... no crystal xray data")
   ENSURE(allocated(self%crystal%asymmetric_unit_atom),"MOLECULE.MISC:fit_Hirshfeld_atom_kappas ... no crystal asym atoms")

      ! Fit parameters
      max_iter = 50
      max_allowed_shift = 0.10d0
      shift_convergence = 0.001d0

      ! Set up grids, atomic densities
      call make_atom_partition_info_(self)

      ! Parameter vectors, shifts
      call create_(kappa,self%n_atom)
      call create_(kappa_old,self%n_atom)
      call create_(kappa_del,self%n_atom)
      kappa = ONE

      ! Gradient and Hessian
      call create_(G,self%n_atom)
      call create_(H,self%n_atom,self%n_atom)

      ! Begin kappa refinement loop
      iter = 0
      call flush_(stdout)
      do
         iter = iter+1

         ! Get the gradient G and Hessian H
         call make_scaled_stockholder_derivs_(self,G,H,kappa,delta,skip_ANOs=TRUE)

         ! Solve for the shifts
         G = -G
         call solve_linear_equation_(H,G,kappa_del)

         ! Get the maximum shift
      ! kappa_del = G
         max_shift = maxval(abs(kappa_del))

         ! Renormalize shift if too big
      ! if (max_shift>max_allowed_shift) then
      !    fac = max_allowed_shift/max_shift
      !    kappa_del = fac * kappa_del
      ! end

         ! Put the results
         call put_(stdout,iter)
         call put_(stdout,max_shift)
         call put_(stdout,delta)
         call flush_(stdout)
      ! stdout.text("G:")
      ! stdout.put(G)

         ! Finished?
         finished = max_shift<shift_convergence OR iter>max_iter
         if (finished) exit

         ! Store parameters, add shift
         kappa_old = kappa
         kappa = kappa+kappa_del
      end do

      ! Put the results
      call flush_(stdout)
      call text_(stdout,"Final kinetic energies:")
      call put_Hirshfeld_G_and_K_energy_(self,kappa)
      call flush_(stdout)
      call text_(stdout,"kappa values:")
      call put_(stdout,kappa)

      ! Clean up
      call destroy_(H); call destroy_(G)
      call destroy_(kappa_del); call destroy_(kappa_old); call destroy_(kappa)
   end subroutine

   subroutine put_Hirshfeld_G_and_K_energy_0(self,kappa)
   ! Put the Hirshfeld G and K energies.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: kappa
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: E_G,E_K,wt,Wa,G_grid,K_grid
      REAL :: E_GT,E_KT
      INT :: a,n_pt
   ENSURE(self%basis_info_made,"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_0 ... no atom list")
   ENSURE(allocated(self%MOs),"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_0 ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_0 ... no restricted MO's")
   ENSURE(allocated(self%becke_grid),"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_0 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_0 ... no finalized becke_grid")
   ENSURE(size(kappa)==self%n_atom,"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_0 ... wrong size, kappa")

      ! Set up grids, atomic densities
      call make_atom_partition_info_(self)

      ! Atomic kinetic energy arrays
      call create_(E_G,self%n_atom)
      call create_(E_K,self%n_atom)

      ! Make the atomic energies
      do a = 1,self%n_atom
         call make_grid_(self%becke_grid,pt,wt,[a])
         n_pt = size(pt,1)
         call create_(Wa,n_pt)
         call make_stockholder_atom_weight_kappa_(self,Wa,a,kappa,pt)
         call create_(G_grid,n_pt)
         call create_(K_grid,n_pt)
         call make_G_and_K_kinetic_grids_(self,G_grid,K_grid,pt)
         E_G(a) = sum(G_grid*Wa*wt)
         E_K(a) = sum(K_grid*Wa*wt)
         call destroy_(K_grid); call destroy_(G_grid)
         call destroy_(Wa); call destroy_(wt); call destroy_(pt)
      end do

      ! Check total kinetic energies
      call make_grid_(self%becke_grid,pt,wt)
      n_pt = size(pt,1)
      call create_(G_grid,n_pt)
      call create_(K_grid,n_pt)
      call make_G_and_K_kinetic_grids_(self,G_grid,K_grid,pt)
      E_GT = sum(G_grid*wt)
      E_KT = sum(K_grid*wt)
      call destroy_(K_grid); call destroy_(G_grid)
      call destroy_(wt); call destroy_(pt)

      ! Put the table
      call flush_(stdout)
      call show_(stdout,"Numerical E_G       =",E_GT)
      call show_(stdout,"Sum of atomic E_G's =",sum(E_G))
      call show_(stdout,"Numerical E_K       =",E_KT)
      call show_(stdout,"Sum of atomic E_K's =",sum(E_K))
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Label",int_width=TRUE)
      call put_(stdout,"E_G")
      call put_(stdout,"E_K")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      do a = 1,self%n_atom
         call put_(stdout,a)
         call put_(stdout,self%atom(a)%label,int_width=TRUE)
         call put_(stdout,E_G(a))
         call put_(stdout,E_K(a))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=2)

      ! Clean up
      call destroy_(E_K); call destroy_(E_G)
   end subroutine

   subroutine put_Hirshfeld_G_and_K_energy_1(self)
   ! Put the Hirshfeld G and K energies.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: E_G,E_K,wt,Wa,G_grid,K_grid
      REAL :: E_GT,E_KT
      INT :: a,n_pt
   ENSURE(self%basis_info_made,"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_1 ... no atom list")
   ENSURE(allocated(self%MOs),"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_1 ... no MO's")
   ENSURE(allocated(self%MOs%r),"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_1 ... no restricted MO's")
   ENSURE(allocated(self%becke_grid),"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_1 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.MISC:put_Hirshfeld_G_and_K_energy_1 ... no finalized becke_grid")

      ! Set up grids, atomic densities, and NO's
      call make_atom_partition_info_(self)

      ! Atomic kinetic energy arrays
      call create_(E_G,self%n_atom)
      call create_(E_K,self%n_atom)

      ! Make the atomic energies
      do a = 1,self%n_atom
         call make_grid_(self%becke_grid,pt,wt,[a])
         n_pt = size(pt,1)
         call create_(Wa,n_pt)
         call make_stockholder_atom_weight_(self,Wa,a,pt)
         call create_(G_grid,n_pt)
         call create_(K_grid,n_pt)
         call make_G_and_K_kinetic_grids_(self,G_grid,K_grid,pt)
         E_G(a) = sum(G_grid*Wa*wt)
         E_K(a) = sum(K_grid*Wa*wt)
         call destroy_(K_grid)
         call destroy_(G_grid)
         call destroy_(Wa)
         call destroy_(wt)
         call destroy_(pt)
      end do

      ! Check total kinetic energies
      call make_grid_(self%becke_grid,pt,wt)
      n_pt = size(pt,1)
      call create_(G_grid,n_pt)
      call create_(K_grid,n_pt)
      call make_G_and_K_kinetic_grids_(self,G_grid,K_grid,pt)
      E_GT = sum(G_grid*wt)
      E_KT = sum(K_grid*wt)
      call destroy_(K_grid)
      call destroy_(G_grid)
      call destroy_(wt)
      call destroy_(pt)

      ! Put the table
      call flush_(stdout)
      call show_(stdout,"Numerical E_G       =",E_GT)
      call show_(stdout,"Sum of atomic E_G's =",sum(E_G))
      call show_(stdout,"Numerical E_K       =",E_KT)
      call show_(stdout,"Sum of atomic E_K's =",sum(E_K))
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Label",int_width=TRUE)
      call put_(stdout,"E_G")
      call put_(stdout,"E_K")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      do a = 1,self%n_atom
         call put_(stdout,a)
         call put_(stdout,self%atom(a)%label,int_width=TRUE)
         call put_(stdout,E_G(a))
         call put_(stdout,E_K(a))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=2)

      ! Clean up
      call destroy_(E_K)
      call destroy_(E_G)
   end subroutine


!  ===========================
!  Make the delta density grid
!  ===========================

   subroutine make_file_delta_density_grid(self)
   ! This routine will read in names for two files containing
   ! molecular orbitals, then read in type of molecular orbitals and
   ! will make density difference between first and second one.
      type(MOLECULE_TYPE), INOUT :: self
      type(ARCHIVE_TYPE) :: arch
      VEC(REAL,:), allocatable :: grid1,grid2
      MAT(REAL,:,:), allocatable :: pt
      STR(len=STR_SIZE) :: name_mo_1,name_mo_2,genre
   ENSURE(allocated(self%plot_grid),"MOLECULE.MISC:make_file_delta_density_grid ... no grid")
   DIE_IF(stdin%buffer%n_items/=4,"MOLECULE.MISC:make_file_delta_density_grid ... must supply 3 arguments")
      call destroy_(self%MOs)

      ! Read MO1 & MO2 file names and kind
      call read_(stdin,name_mo_1)
      call read_(stdin,name_mo_2)
      call read_(stdin,genre)

      ! Read 1st MO's
      call set_(arch,self%name,name_mo_1,genre)
      call read_(arch,self%MOs)
      call assign_NOs_to_MOs_(self)

      ! Make density grid for MO1
      call create_(grid1,self%plot_grid%n_pt)
      call create_(pt,self%plot_grid%n_pt,3); call make_points_(self%plot_grid,pt)
      call make_ED_grid_(self,grid1,pt)
      call destroy_(pt)

      ! Read 2nd MO's
      call set_(arch,self%name,name_mo_2,genre)
      call read_(arch,self%MOs)
      call assign_NOs_to_MOs_(self)

      ! Make density grid for MO2
      call create_(grid2,self%plot_grid%n_pt)
      call create_(pt,self%plot_grid%n_pt,3); call make_points_(self%plot_grid,pt)
      call make_ED_grid_(self,grid2,pt)
      call destroy_(pt)

      ! Subtract
      grid1 = grid1-grid2

      ! Print
      call dump_plot_grid_(self,grid1,"file_delta_density_grid")

      ! Clean up
      call destroy_(grid2)
      call destroy_(grid1)
      call destroy_(self%MOs)
   end subroutine

   subroutine make_file_delta_spin_grid(self)
   ! This routine will read in names for two files containing
   ! molecular orbitals, then read in type of molecular orbitals and
   ! will make SPIN denstity difference between first file and the
   ! second one.
      type(MOLECULE_TYPE), INOUT :: self
      type(ARCHIVE_TYPE) :: arch
      VEC(REAL,:), allocatable :: grid1,grid2
      MAT(REAL,:,:), allocatable :: pt
      STR(len=STR_SIZE) :: name_mo_1,name_mo_2,genre
   ENSURE(allocated(self%plot_grid),"MOLECULE.MISC:make_file_delta_spin_grid ... no grid")
   DIE_IF(stdin%buffer%n_items/=4,"MOLECULE.MISC:make_file_delta_spin_grid ... must supply 3 arguments")
      call destroy_(self%MOs)

      ! Read MO1 & MO2 file names and kind
      call read_(stdin,name_mo_1)
      call read_(stdin,name_mo_2)
      call read_(stdin,genre)

      ! Read 1st MO's
      call set_(arch,self%name,name_mo_1,genre)
      call read_(arch,self%MOs)
      call assign_NOs_to_MOs_(self)

      ! Make density grid for MO1
      call create_(grid1,self%plot_grid%n_pt)
      call create_(pt,self%plot_grid%n_pt,3); call make_points_(self%plot_grid,pt)
      call make_SD_grid_(self,grid1,pt)
      call destroy_(pt)

      ! Read 2nd MO's
      call set_(arch,self%name,name_mo_2,genre)
      call read_(arch,self%MOs)
      call assign_NOs_to_MOs_(self)

      ! Make density grid for MO2
      call create_(grid2,self%plot_grid%n_pt)
      call create_(pt,self%plot_grid%n_pt,3); call make_points_(self%plot_grid,pt)
      call make_SD_grid_(self,grid2,pt)
      call destroy_(pt)

      ! Subtract
      grid1 = grid1-grid2

      ! Print
      call dump_plot_grid_(self,grid1,"file_delta_spin_grid")

      ! Clean up
      call destroy_(grid2)
      call destroy_(grid1)
      call destroy_(self%MOs)
   end subroutine


!  =================
!  Put interpolators
!  =================

   subroutine put_slater_interpolators(self)
   ! Print out the slaterbasis interpolator tables for a certain
   ! "basis_kind" for all interpolators up to "Z_max" and up to a
   ! certain "density_cutoff", all these as read from stdin.
      type(MOLECULE_TYPE), INOUT :: self
      type(INTERPOLATOR_TYPE), allocatable :: interpolator
      VEC(type(SLATERBASIS_TYPE),:), allocatable :: slaterbasis
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      STR(len=STR_SIZE) :: word,basis_kind,plotfile
      REAL :: density_cutoff
      BIN :: do_log10
      INT :: Z_max,Z
   ENSURE(stdin%buffer%n_items==5,"MOLECULE.MISC:put_slater_interpolators ... must be four items after the keyword")
   ENSURE(stdin%buffer%item_index==1,"MOLECULE.MISC:put_slater_interpolators ... must be at 2nd item on the input line")
      if (FALSE) self = self

      ! Read the input
      call read_(stdin,basis_kind)
      call read_(stdin,Z_max)
      call read_(stdin,density_cutoff)
      call read_(stdin,do_log10)

      ! Create a log-linear prototype interpolator
      call create_(interpolator)
      call set_table_length_(interpolator,60.0d0)
   ! interpolator.set_table_eps(density_cutoff)
      call set_interpolation_method_(interpolator,"linear")
      call set_range_mapping_(interpolator,"none")
      call set_range_mapping_(interpolator,"none")
      call set_domain_mapping_(interpolator,"none")

      ! See if interpolator appears in the input
      word = next_item_(stdin)
      call to_lower_case_(word)
      if (word=="interpolator=") then
         call skip_next_item_(stdin)  ! skip interpolator=
         call skip_next_item_(stdin)  ! skip {
         call read_keywords_(interpolator)
      end if

      ! Get the library basis set labels up to Z_max
      labels = library_basis_labels_(Z_max,basis_kind)
      call put_(stdout,labels)

      ! Read the library bases
      call read_library_data_(slaterbasis,library_file_(slaterbasis,basis_kind),labels)

      ! Put out data
      call put_(slaterbasis)

      ! Take the normalisation coeffieicnts out
      call unnormalize_(slaterbasis)

      ! Make interpolators and put out the gnuplots
      do Z = 1,size(slaterbasis)
         call show_(stdout,"Z =",Z)
         call make_interpolator_(slaterbasis(Z),interpolator)
         plotfile = trim(to_str_(Z))//"_"//trim(generic_chemical_symbol_(Z))
         plotfile = trim(plotfile)//"_"//trim(basis_kind)//".gnuplot"
         call redirect_(stdout,plotfile)
         call put_gnuplot_(slaterbasis(Z)%interpolator,do_log10)
         call revert_(stdout)
      end do
      call text_(stdout,"Finished all interpolator plots")

      ! Cleanup
      call destroy_(slaterbasis)
      call destroy_(labels)
      call destroy_(interpolator)
   end subroutine

   subroutine put_coppens_interpolators(self)
   ! Print out the slaterbasis interpolator tables for a certain
   ! "basis_kind" for all interpolators up to "Z_max" and up to a
   ! certain "density_cutoff", all these as read from stdin.
      type(MOLECULE_TYPE), INOUT :: self
      type(INTERPOLATOR_TYPE), allocatable :: interpolator
      VEC(type(COPPENSBASIS_TYPE),:), allocatable :: slaterbasis
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      STR(len=STR_SIZE) :: word,basis_kind,plotfile
      REAL :: density_cutoff
      BIN :: do_log10
      INT :: Z_max,Z
   ENSURE(stdin%buffer%n_items==5,"MOLECULE.MISC:put_coppens_interpolators ... must be four items after the keyword")
   ENSURE(stdin%buffer%item_index==1,"MOLECULE.MISC:put_coppens_interpolators ... must be at 2nd item on the input line")

      ! The following code is inherited from MOLECULE.MISC
      if (FALSE) self = self

      ! Read the input
      call read_(stdin,basis_kind)
      call read_(stdin,Z_max)
      call read_(stdin,density_cutoff)
      call read_(stdin,do_log10)

      ! Create a log-linear prototype interpolator
      call create_(interpolator)
      call set_table_length_(interpolator,60.0d0)
   ! interpolator.set_table_eps(density_cutoff)
      call set_interpolation_method_(interpolator,"linear")
      call set_range_mapping_(interpolator,"none")
      call set_range_mapping_(interpolator,"none")
      call set_domain_mapping_(interpolator,"none")

      ! See if interpolator appears in the input
      word = next_item_(stdin)
      call to_lower_case_(word)
      if (word=="interpolator=") then
         call skip_next_item_(stdin)  ! skip interpolator=
         call skip_next_item_(stdin)  ! skip {
         call read_keywords_(interpolator)
      end if

      ! Get the library basis set labels up to Z_max
      labels = library_basis_labels_(Z_max,basis_kind)
      call put_(stdout,labels)

      ! Read the library bases
      call read_library_data_(slaterbasis,library_file_(slaterbasis,basis_kind),labels)

      ! Put out data
      call put_(slaterbasis)

      ! Take the normalisation coeffieicnts out
      call unnormalize_(slaterbasis)

      ! Make interpolators and put out the gnuplots
      do Z = 1,size(slaterbasis)
         call show_(stdout,"Z =",Z)
         call make_interpolator_(slaterbasis(Z),interpolator)
         plotfile = trim(to_str_(Z))//"_"//trim(generic_chemical_symbol_(Z))
         plotfile = trim(plotfile)//"_"//trim(basis_kind)//".gnuplot"
         call redirect_(stdout,plotfile)
         call put_gnuplot_(slaterbasis(Z)%interpolator,do_log10)
         call revert_(stdout)
      end do
      call text_(stdout,"Finished all interpolator plots")

      ! Cleanup
      call destroy_(slaterbasis)
      call destroy_(labels)
      call destroy_(interpolator)
   end subroutine

   subroutine put_diff_slater_interpolators(self)
   ! Print out the difference between two slaterbasis interpolator tables for
   ! bases "basis1" and "basis2", for all interpolators up to "Z_max"
   ! and up to a certain "density_cutoff", all these as read from
   ! stdin.
      type(MOLECULE_TYPE), INOUT :: self
      type(INTERPOLATOR_TYPE), allocatable :: interpolator
      VEC(type(SLATERBASIS_TYPE),:), allocatable :: slater1basis,slater2basis
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      STR(len=STR_SIZE) :: basis1,basis2,plotfile
      REAL :: density_cutoff
      BIN :: do_log10
      INT :: Z_max,Z
   ENSURE(stdin%buffer%n_items==6,"MOLECULE.MISC:put_diff_slater_interpolators ... must be five items after the keyword")
   ENSURE(stdin%buffer%item_index==1,"MOLECULE.MISC:put_diff_slater_interpolators ... must be at 2nd item on the input line")
      if (FALSE) self = self

      ! Read the input
      call read_(stdin,basis1)
      call read_(stdin,basis2)
      call read_(stdin,Z_max)
      call read_(stdin,density_cutoff)
      call read_(stdin,do_log10)

      ! Create a log-linear prototype interpolator
      call create_(interpolator)
      call set_table_length_(interpolator,60.0d0)
   ! interpolator.set_table_eps(density_cutoff)
      call set_interpolation_method_(interpolator,"linear")
      call set_range_mapping_(interpolator,"none")
      call set_domain_mapping_(interpolator,"none")

      ! Get the library basis set labels up to Z_max
      labels = library_basis_labels_(Z_max,basis1)
      call read_library_data_(slater1basis,library_file_(slater1basis,basis1),labels)
      call destroy_(labels)
      labels = library_basis_labels_(Z_max,basis2)
      call read_library_data_(slater2basis,library_file_(slater2basis,basis2),labels)
      call destroy_(labels)

      ! Make interpolators and put out the gnuplots
      do Z = 1,size(slater1basis)
         call show_(stdout,"Z =",Z)
         call make_interpolator_(slater1basis(Z),interpolator)
         call make_interpolator_(slater2basis(Z),interpolator)
         plotfile = trim(to_str_(Z))//"_"//trim(generic_chemical_symbol_(Z))
         plotfile = trim(plotfile)//"_"//trim(basis1)//"-"//trim(basis2)//".gnuplot"
         call redirect_(stdout,plotfile)
         call put_gnuplot_(slater1basis(Z)%interpolator,slater2basis(Z)%interpolator,do_log10)
         call revert_(stdout)
      end do
      call text_(stdout,"Finished all interpolator plots")

      ! Cleanup
      call destroy_(slater2basis)
      call destroy_(slater1basis)
      call destroy_(interpolator)
   end subroutine

   subroutine put_diff_coppens_slater_ints(self)
   ! Print out the difference between two slaterbasis interpolator tables for
   ! bases "basis1" and "basis2", for all interpolators up to "Z_max"
   ! and up to a certain "density_cutoff", all these as read from
   ! stdin.
      type(MOLECULE_TYPE), INOUT :: self
      type(INTERPOLATOR_TYPE), allocatable :: interpolator
      VEC(type(COPPENSBASIS_TYPE),:), allocatable :: coppensbasis
      VEC(type(SLATERBASIS_TYPE),:), allocatable :: slaterbasis
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      STR(len=STR_SIZE) :: basis1,basis2,plotfile
      REAL :: density_cutoff
      BIN :: do_log10
      INT :: Z_max,Z
   ENSURE(stdin%buffer%n_items==6,"MOLECULE.MISC:put_diff_coppens_slater_ints ... must be six items after the keyword")
   ENSURE(stdin%buffer%item_index==1,"MOLECULE.MISC:put_diff_coppens_slater_ints ... must be at 2nd item on the input line")
      if (FALSE) self = self

      ! Read the input
      call read_(stdin,basis1)
      call read_(stdin,basis2)
      call read_(stdin,Z_max)
      call read_(stdin,density_cutoff)
      call read_(stdin,do_log10)

      ! Create a log-linear prototype interpolator
      call create_(interpolator)
      call set_table_length_(interpolator,60.0d0)
   ! interpolator.set_table_eps(density_cutoff)
      call set_interpolation_method_(interpolator,"linear")
      call set_range_mapping_(interpolator,"none")
      call set_domain_mapping_(interpolator,"none")

      ! Get the library basis set labels up to Z_max
      labels = library_basis_labels_(Z_max,basis1)
      call read_library_data_(coppensbasis,library_file_(coppensbasis,basis1),labels)
      call destroy_(labels)
      labels = library_basis_labels_(Z_max,basis2)
      call read_library_data_(slaterbasis,library_file_(slaterbasis,basis2),labels)
      call destroy_(labels)

      ! Make interpolators and put out the gnuplots
      do Z = 1,size(slaterbasis)
         call show_(stdout,"Z =",Z)
         call make_interpolator_(coppensbasis(Z),interpolator)
         call make_interpolator_(slaterbasis(Z),interpolator)
         plotfile = trim(to_str_(Z))//"_"//trim(generic_chemical_symbol_(Z))
         plotfile = trim(plotfile)//"_"//trim(basis1)//"-"//trim(basis2)//".gnuplot"
         call redirect_(stdout,plotfile)
         call put_gnuplot_(coppensbasis(Z)%interpolator,slaterbasis(Z)%interpolator,do_log10)
         call revert_(stdout)
      end do
      call text_(stdout,"Finished all interpolator plots")

      ! Cleanup
      call destroy_(slaterbasis)
      call destroy_(coppensbasis)
      call destroy_(interpolator)
   end subroutine


!  ================
!  Random ED, atoms
!  ================

   subroutine make_Vanfleteren_atoms(self)
   ! Do it.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: S,Sc,rho,rmh
      MAT(REAL,:,:), allocatable :: rho_this,rho_next
      MAT3(REAL,:,:,:), allocatable :: rho_this_A,rho_next_A
      MAT3(REAL,:,:,:), allocatable :: wgt_this_A
      INT :: iter,n,a,na,f,l
      REAL :: diff
   ENSURE(allocated(self%MOs),"MOLECULE.MISC:make_Vanfleteren_atoms ... No MOs")
   ENSURE(allocated(self%MOs%r),"MOLECULE.MISC:make_Vanfleteren_atoms ... No restricted MOs")

      ! No. of basis fn's
      n = self%n_bf

      ! VA iterative array matrices
      call create_(rho_next_A,n,n,self%n_atom)
      call create_(rho_this_A,n,n,self%n_atom)
      call create_(wgt_this_A,n,n,self%n_atom)

      ! Total densities
      call create_(rho_this,n,n)
      call create_(rho_next,n,n)

      ! And zero em ...
      rho_this_A = ZERO

      ! Overlap matrices & powers
      call create_(S,n,n)
   ! Sph.create(n,n)
   ! Smh.create(n,n)
      call create_(rmh,n,n)

      ! And define em ...
      S = self%overlap_mx
   ! Sph.to_sqrt_of(S)         ! S^+1/2
   ! Smh.to_inverse_sqrt_of(S) ! S^-1/2

      ! S . MOs
      Sc = matmul(S,self%MOs%r)

      ! Check
   ! W = S
   ! W.change_basis_using(Smh)
   ! stdout.text("Check Smh . S . Smh :")
   ! stdout.put(W)

      ! Check
   ! W = matmul(Sph,Smh)
   ! stdout.text("Check Sph . Smh :")
   ! stdout.put(W)

      ! Get rho in symmetrically orthonormal basis 
      rho = self%density_mx%r
      call change_basis_using_(rho,Sc)
      call text_(stdout,"rho in MO basis:")
      call put_(stdout,rho)

      ! Set initial rho^(i)_AA
      rho_this_A = ZERO
      l = 0
      do a = 1,size(self%atom)
         f = l+1
         l = l+self%atom(a)%basis%n_bf
         rho_this_A(f:l,f:l,a) = self%atom(a)%density_mx%r
      end do
      call text_(stdout,"Transferred rho^(i)_AA in AO basis:")
      call put_(stdout,rho_this_A)

      ! Get rho^(i)_AA in the MO basis
      rho_this = ZERO
      l = 0
      do a = 1,size(self%atom)

         ! Basis fn limits
         na = self%atom(a)%basis%n_bf
         f = l+1
         l = l+self%atom(a)%basis%n_bf
         ! Initial rho^(i)_AA
         call change_basis_using_(rho_this_A(:,:,a),Sc)
         ! rho^(i) = Sum_A rho^(i)_AA ... eqn 6(a)
         rho_this = rho_this+rho_this_A(:,:,a)
      end do
      call text_(stdout,"rho^(i)_AA in the MO basis:")
      call put_(stdout,rho_this_A)
      call text_(stdout,"rho^(i) in the MO basis:")
      call put_(stdout,rho_this)

      ! Start iterations
      iter = 0
      do
         iter = iter+1

         ! Overwrite new [rho^(i)]^-1/2
         call to_sqrt_of_(rmh,rho_this)

         ! Make the w_A_i ... 6b
         wgt_this_A = ZERO
         do a = 1,self%n_atom
            call change_basis_to_(rho_this_A(:,:,a),wgt_this_A(:,:,a),rmh)
         end do

         ! Update the atomic rho_A's
         rho_next_A = ZERO
         do a = 1,self%n_atom
            call change_basis_to_(rho,rho_next_A(:,:,a),wgt_this_A(:,:,a))
         end do

         ! Difference
         diff = maxval(abs(rho_next_A-rho_this_A))
         call show_(stdout,"Diff. between rho i+1 and rho i =",diff)

         ! Converged?
         if (diff<TOL(2) OR iter>10) exit
         rho_this_A = rho_next_A
      end do
   end subroutine

   subroutine put_ED_between_atoms(self)
   ! WARNING: this routine reads from stdin.
   ! Also: the number of points must be specified in an interpolator.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: A,B
      STR(len=STR_SIZE) :: ABstr
      STR(len=STR_SIZE) :: datafile
      STR(len=STR_SIZE) :: plotfile
      REAL :: r_AB
   ENSURE(allocated(self%interpolator),"MOLECULE.MISC:put_ED_between_atoms ... no interpolator")

      ! Reads atoms
      call read_(stdin,A)
      call read_(stdin,B)

      ! Make ED in .interpolator
      call make_ED_interpolator_between_atoms_(self,A,B)

      ! Dump it
      r_AB = distance_between_(self%atom,A,B)
      call head_(stdout,"ED between two atoms from A->B")
      call show_(stdout,"Atom A = ",a)
      call show_(stdout,"Atom B = ",b)
      call flush_(stdout)
      call show_(stdout,"R(A-B)    = ",r_AB)
      call show_(stdout,"R(A-B) /A = ",r_AB*ANGSTROM_PER_BOHR)
      call flush_(stdout)
      call show_(stdout,"n_pts  = ",self%interpolator%n_data)
      call flush_(stdout)
      call text_(stdout,"ED values along A->B")
      call flush_(stdout)
      call text_(stdout,". See datafile ED_A_B.gnu")
      call text_(stdout,". See plotfile plot_ED_A_B.gnu")
      call text_(stdout,". Execute 'gnuplot -persist <plotfile>' to view")
      ABstr = trim(trim(to_str_(A))//"_"//trim(to_str_(B)))
      datafile = trim("ED_"//trim(ABstr)//".gnu")
      call redirect_(stdout,datafile)
      call put_gnuplot_(self%interpolator)
      call revert_(stdout)
      plotfile = trim("plot_"//trim(datafile))
      call redirect_(stdout,plotfile)
      call text_(stdout,"set xlabel 'R_{AB}/bohr'")
      call text_(stdout,"set ylabel 'ED in e/bohr^3'")
      call text_(stdout,"set yrange [0:1]")
      call text_(stdout,"set mxtics")
      call text_(stdout,"set mytics")
      call text_(stdout,"set grid  xtics  ytics")
      call text_(stdout,"set grid mxtics mytics")
      call text_(stdout,"set terminal qt")
      call text_(stdout,"plot '"//trim(datafile)//"' using 1:2 with lines ")
      call revert_(stdout)
      datafile = trim("ED_"//trim(ABstr)//"_A_log10.gnu")
      call redirect_(stdout,datafile)
      call put_gnuplot_(self%interpolator,do_log10=TRUE,to_angstrom=TRUE)
      call revert_(stdout)
      plotfile = trim("plot_"//trim(datafile))
      call redirect_(stdout,plotfile)
      call text_(stdout,"set xlabel 'R_{AB}/Ang'")
      call text_(stdout,"set ylabel 'log10(ED), ED in e/bohr^3'")
      call text_(stdout,"set mxtics")
      call text_(stdout,"set mytics")
      call text_(stdout,"set grid  xtics  ytics")
      call text_(stdout,"set grid mxtics mytics")
      call text_(stdout,"set terminal qt")
      call text_(stdout,"plot '"//trim(datafile)//"' using 1:2 with lines ")
      call revert_(stdout)
   end subroutine

   subroutine put_SA_ED_between_atoms(self)
   ! WARNING: this routine reads from stdin.
   ! Also: the number of points must be specified in an interpolator.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: A,B
      STR(len=STR_SIZE) :: ABstr
      STR(len=STR_SIZE) :: datafile
      STR(len=STR_SIZE) :: plotfile
      REAL :: r_AB
   ENSURE(allocated(self%interpolator),"MOLECULE.MISC:put_SA_ED_between_atoms ... no interpolator")

      ! Reads atoms
      call read_(stdin,A)
      call read_(stdin,B)

      ! Make ED in .interpolator. "B" returned.
      call make_SA_ED_interp_between_atoms_(self,A,B)

      ! Dump it
      r_AB = distance_between_(self%atom,A,B)
      call head_(stdout,"Salvador atom electron density")
      call text_(stdout,"ED for atom A, along A->B")
      call flush_(stdout)
      call show_(stdout,"Atom A    = ",A)
      call show_(stdout,"Atom B    = ",B)
      call flush_(stdout)
      call show_(stdout,"R(A-B)    = ",r_AB)
      call show_(stdout,"R(A-B) /A = ",r_AB*ANGSTROM_PER_BOHR)
      call flush_(stdout)
      call show_(stdout,"n_pts     = ",self%interpolator%n_data)
      call flush_(stdout)
      call text_(stdout,". See datafile SA_ED_AB.gnu")
      call text_(stdout,". See plotfile plot_SA_ED_AB.gnu")
      call text_(stdout,". Execute 'gnuplot -persist <plotfile>' to view")
      ABstr = trim(trim(to_str_(A))//"_"//trim(to_str_(B)))
      datafile = trim("SA_ED_"//trim(ABstr)//".gnu")
      call redirect_(stdout,datafile)
      call put_gnuplot_(self%interpolator)
      call revert_(stdout)
      plotfile = trim("plot_"//trim(datafile))
      call redirect_(stdout,plotfile)
      call text_(stdout,"set xlabel 'R_{AB}/bohr'")
      call text_(stdout,"set ylabel 'SA cell fn.'")
      call text_(stdout,"set yrange [0:1]")
      call text_(stdout,"set mxtics")
      call text_(stdout,"set mytics")
      call text_(stdout,"set grid  xtics  ytics")
      call text_(stdout,"set grid mxtics mytics")
      call text_(stdout,"set terminal qt")
      call text_(stdout,"plot '"//trim(datafile)//"' using 1:2 with lines ")
      call revert_(stdout)
   end subroutine

   subroutine put_SA_cell_fn_between_atoms(self)
   ! WARNING: this routine reads from stdin.
   ! Also: the number of points must be specified in an interpolator.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: A,B
      STR(len=STR_SIZE) :: ABstr
      STR(len=STR_SIZE) :: datafile
      STR(len=STR_SIZE) :: plotfile
      REAL :: r_AB
   ENSURE(allocated(self%interpolator),"MOLECULE.MISC:put_SA_cell_fn_between_atoms ... no interpolator")

      ! Reads atoms
      call read_(stdin,A)
      call read_(stdin,B)

      ! Make ED in .interpolator. "B" returned.
      call make_SA_cell_fn_interp_between_atoms_(self,A,B)

      ! Dump it
      r_AB = distance_between_(self%atom,A,B)
      call head_(stdout,"Salvador & Ramos-Cordoba cell function")
      call text_(stdout,"Unitless values along A->B")
      call flush_(stdout)
      call show_(stdout,"Atom A    = ",A)
      call show_(stdout,"Atom B    = ",B)
      call flush_(stdout)
      call show_(stdout,"R(A-B)    = ",r_AB)
      call show_(stdout,"R(A-B) /A = ",r_AB*ANGSTROM_PER_BOHR)
      call flush_(stdout)
      call show_(stdout,"n_pts     = ",self%interpolator%n_data)
      call flush_(stdout)
      call text_(stdout,". See datafile SA_cell_fn_AB.gnu")
      call text_(stdout,". See plotfile plot_SA_cell_fn_AB.gnu")
      call text_(stdout,". Execute 'gnuplot -persist <plotfile>' to view")
      ABstr = trim(trim(to_str_(A))//"_"//trim(to_str_(B)))
      datafile = trim("SA_cell_fn_"//trim(ABstr)//".gnu")
      call redirect_(stdout,datafile)
      call put_gnuplot_(self%interpolator)
      call revert_(stdout)
      plotfile = trim("plot_"//trim(datafile))
      call redirect_(stdout,plotfile)
      call text_(stdout,"set xlabel 'R_{AB}/bohr'")
      call text_(stdout,"set ylabel 'SA cell fn.'")
      call text_(stdout,"set yrange [0:1]")
      call text_(stdout,"set mxtics")
      call text_(stdout,"set mytics")
      call text_(stdout,"set grid  xtics  ytics")
      call text_(stdout,"set grid mxtics mytics")
      call text_(stdout,"set terminal qt")
      call text_(stdout,"plot '"//trim(datafile)//"' using 1:2 with lines ")
      call revert_(stdout)
   end subroutine

   subroutine put_spherical_SA_ED_from_atom(self)
   ! WARNING: this routine reads from stdin.
   ! Also: the number of points must be specified in an interpolator.
      type(MOLECULE_TYPE), INOUT :: self
      INT :: A,B
      STR(len=STR_SIZE) :: ABstr
      STR(len=STR_SIZE) :: datafile
      STR(len=STR_SIZE) :: plotfile
      REAL :: r_AB
      REAL :: r_max
   ENSURE(allocated(self%interpolator),"MOLECULE.MISC:put_spherical_SA_ED_from_atom ... no interpolator")

      ! Reads atoms
      call read_(stdin,A)

      ! Make ED in .interpolator. "B" returned.
      if (stdin%buffer%n_items>2) then
         call read_(stdin,r_max)
         call make_sph_SA_interp_from_atom_(self,A,B,r_max)
         r_AB = distance_between_(self%atom,A,B)
      else
         call make_sph_SA_interp_from_atom_(self,A,B)
         r_AB = distance_between_(self%atom,A,B)
         r_max = r_AB
      end if

      ! Dump it
      call head_(stdout,"Spherically averaged Salvador atom ED")
      call text_(stdout,"ED values along A->B, B the furthest neighbour atom.")
      call flush_(stdout)
      call text_(stdout,"The plot goes up to R_max along the bond axis.")
      call flush_(stdout)
      call show_(stdout,"Atom A    = ",A)
      call show_(stdout,"Atom B    = ",B)
      call flush_(stdout)
      call show_(stdout,"R(A-B)    = ",r_AB)
      call show_(stdout,"R(A-B) /A = ",r_AB*ANGSTROM_PER_BOHR)
      call flush_(stdout)
      call show_(stdout,"R_max     = ",r_max)
      call show_(stdout,"R_max /A  = ",r_max*ANGSTROM_PER_BOHR)
      call flush_(stdout)
      call show_(stdout,"n_pts     = ",self%interpolator%n_data)
      call flush_(stdout)
      call text_(stdout,". See datafile sph_SA_ED_AB.gnu")
      call text_(stdout,". See plotfile plot_sph_SA_ED_AB.gnu")
      call text_(stdout,". Execute 'gnuplot -persist <plotfile>' to view")
      ABstr = trim(trim(to_str_(A))//"_"//trim(to_str_(B)))
      datafile = trim("sph_SA_ED_"//trim(ABstr)//".gnu")
      call redirect_(stdout,datafile)
      call put_gnuplot_(self%interpolator)
      call revert_(stdout)
      plotfile = trim("plot_"//trim(datafile))
      call redirect_(stdout,plotfile)
      call text_(stdout,"set xlabel 'R_{AB}/bohr'")
      call text_(stdout,"set ylabel 'sph. SA ED, in e/bohr^3'")
      call text_(stdout,"set yrange [0:1]")
      call text_(stdout,"set mxtics")
      call text_(stdout,"set mytics")
      call text_(stdout,"set grid  xtics  ytics")
      call text_(stdout,"set grid mxtics mytics")
      call text_(stdout,"set terminal qt")
      call text_(stdout,"plot '"//trim(datafile)//"' using 1:2 with lines ")
      call revert_(stdout)
      datafile = trim("sph_SA_ED_"//trim(ABstr)//"_A_log10.gnu")
      call redirect_(stdout,datafile)
      call put_gnuplot_(self%interpolator,do_log10=TRUE,to_angstrom=TRUE)
      call revert_(stdout)
      plotfile = trim("plot_"//trim(datafile))
      call redirect_(stdout,plotfile)
      call text_(stdout,"set xlabel 'R_{AB}/Ang'")
      call text_(stdout,"set ylabel 'log10(sph. SA ED), ED in e/bohr^3'")
      call text_(stdout,"set mxtics")
      call text_(stdout,"set mytics")
      call text_(stdout,"set grid  xtics  ytics")
      call text_(stdout,"set grid mxtics mytics")
      call text_(stdout,"set terminal qt")
      call text_(stdout,"plot '"//trim(datafile)//"' using 1:2 with lines ")
      call revert_(stdout)
   end subroutine


!  ===
!  MP2
!  ===

   subroutine MP2(self)
   ! MP2 energy calculation; select the kind of spinorbitals.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%basis_info_made,"MOLECULE.MISC:MP2 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.MISC:MP2 ... no atom list")
   ENSURE(allocated(self%MOs),"MOLECULE.MISC:MP2 ... no molecular orbitals")
      select case (spinorbital_genre_(self%SCF_data))

      case ("r "); call make_r_mp2_(self)

      case ("u "); call make_u_mp2_(self)

      case ("gc")
         call make_gc_mp2_(self)

      case default
         DIE("MOLECULE.MISC:MP2 ... unimplemented")
      end select
   end subroutine

   subroutine make_r_mp2(self)
   ! Calculate the electron repulsion integrals (ERIs) in the MO
   ! basis from the AO integrals and make restricted MP2 in core
      type(MOLECULE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), allocatable :: v
      VEC(REAL,:), allocatable :: eval
      REAL :: E2,Escf
      REAL :: virt,ram_lim
      INT :: a,b,r,s,fa,lr
   ENSURE(allocated(self%MOs%r),"MOLECULE.MISC:make_r_mp2 ... no molecular orbitals")
   ENSURE(allocated(self%MO_energies%r),"MOLECULE.MISC:make_r_mp2 ... no orbital energies")
      call create_(eval,self%n_bf)
      eval = self%MO_energies%r
      call flush_(stdout)
      call text_(stdout," Orbital energies:")
      call flush_(stdout)
      call put_(stdout,eval)
      virt = real(self%n_bf)**4*8/1024**3
      call flush_(stdout)
      ram_lim = 4.0  ! in Gb
      call show_(stdout,"Needed space for ERI in RAM (in Gb) =",virt)
      if (virt>ram_lim) then
         call show_(stdout," Current RAM limit is (in Gb) =",ram_lim)
         DIE("MOLECULE.MISC:make_r_mp2 ...  Current RAM limit has been exceeded! ")
      end if
      call set_MO_limits_(self,eval,fa,lr)
      call create_(v,self%n_bf,self%n_bf,self%n_bf,self%n_bf)
      v = ZERO
      call make_MO_ERI_integrals_(self,v)
      call flush_(stdout)
      call text_(stdout,"==========================")
      call text_(stdout,"Restricted MP2 calculation")
      call text_(stdout,"==========================")
      call flush_(stdout)
      call text_(stdout,". This is a toy implementation with integrals stored in memory")
      call text_(stdout,". The active space is restricted to minic the gaussian program")
      call flush_(stdout)
      call show_(stdout,"First active occupied orbital =",fa)
      call show_(stdout,"Last  active virtual  orbital =",lr)
      E2 = ZERO

      ! Based on Szabo, Ostlund Book, p. 352, eq. 6.74
      ! a,b are occupied spinorbitals
      ! r,s are virtual orbitals

      ! In the case of chosen MO limits
      do a = fa,self%n_e/2
         do b = fa,self%n_e/2
            do r = self%n_e/2+1,lr
               do s = self%n_e/2+1,lr
                  E2 = E2+TWO*v(a,b,r,s)*v(r,s,a,b)/(eval(a)+eval(b)-eval(r)-eval(s))
                  E2 = E2-v(a,b,r,s)*v(r,s,b,a)/(eval(a)+eval(b)-eval(r)-eval(s))
               end do
            end do
         end do
      end do
      Escf = SCF_energy_(self)
      call flush_(stdout)
      call text_(stdout,"Results:")
      call flush_(stdout)
      call show_(stdout,"MP2 correlation energy        = ",E2)
      call show_(stdout,"SCF energy                    = ",Escf)
      call show_(stdout,"Total MP2 energy              = ",E2+Escf)
      call flush_(stdout)
      call destroy_(v)
      call destroy_(eval)
   end subroutine

   subroutine make_chem_mp2(self)
   ! Calculate the electron repulsion integrals (ERIs) in the MO
   ! basis from the AO integrals and make restricted MP2 in core
      type(MOLECULE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), allocatable :: v
      VEC(REAL,:), allocatable :: eval
      INT :: n,f,l,i,j,a,b
      REAL :: e2,top,bot,Vc,Vx
      call create_(eval,self%n_bf)
      eval = self%MO_energies%r

      ! Get active space
      n = self%n_e/2
      call set_MO_limits_(self,eval,f,l)

      ! Get integrals
      call create_(v,self%n_bf,self%n_bf,self%n_bf,self%n_bf)
      v = ZERO
      call make_MO_ERI_chem_integrals_(self,v)

      ! In the case of chosen MO limits
      e2 = ZERO
      do i = f,n
         do j = f,n
            do a = n+1,l
               do b = n+1,l
                  Vc = v(i,a,j,b)
                  Vx = Vc-v(i,b,j,a)
                  top = TWO*Vc*Vc+Vx*Vx
                  bot = eval(i)+eval(j)-eval(a)-eval(b)
                  e2 = e2+top/bot
               end do
            end do
         end do
      end do
      e2 = HALF*e2
      call flush_(stdout)
      call text_(stdout,"==========================")
      call text_(stdout,"Restricted MP2 calculation")
      call text_(stdout,"==========================")
      call flush_(stdout)
      call text_(stdout,". This is a toy implementation with integrals stored in memory")
      call text_(stdout,". The active space is restricted to minic the gaussian program")
      call flush_(stdout)
      call show_(stdout,"First active occupied orbital =",f)
      call show_(stdout,"Last  active virtual  orbital =",l)
      call flush_(stdout)
      call text_(stdout,"Results:")
      call flush_(stdout)
      call show_(stdout,"MP2 correlation energy        = ",e2)
      call destroy_(v)
      call destroy_(eval)
   end subroutine

   subroutine make_u_mp2(self)
   ! Calculate the electron repulsion integrals (ERI's) in the uMO
   ! basis from the AO integrals and make unrestricted MP2
      type(MOLECULE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), allocatable :: vaa,vab,vbb
      REAL :: E2,E
      REAL :: virt,ram_lim
      VEC(REAL,:), allocatable :: evala,evalb
      INT :: a,b,r,s,fa,fb,lr,ls
   ENSURE(allocated(self%MOs%a),"MOLECULE.MISC:make_u_mp2 ... no alpha orbitals")
   ENSURE(allocated(self%MOs%b),"MOLECULE.MISC:make_u_mp2 ... no beta orbitals")
   ENSURE(allocated(self%MO_energies%a),"MOLECULE.MISC:make_u_mp2 ... no alpha energies")
   ENSURE(allocated(self%MO_energies%b),"MOLECULE.MISC:make_u_mp2 ... no beta energies")
      call create_(evala,self%n_bf)
      call create_(evalb,self%n_bf)
      evala = self%MO_energies%a
      evalb = self%MO_energies%b
      call flush_(stdout)
      call text_(stdout," Alpha orbital unrestricted energies:")
      call flush_(stdout)
      call put_(stdout,evala)
      call text_(stdout," Beta orbital unrestricted energies:")
      call flush_(stdout)
      call put_(stdout,evalb)
      virt = (real(self%n_bf)**4*8/1024**3)*3
      call flush_(stdout)
      ram_lim = 4.0  ! in Gb
      call show_(stdout,"Needed space for ERI in RAM (in Gb) =",virt)
      if (virt>ram_lim) then
         call show_(stdout," Current RAM limit is (in Gb) =",ram_lim)
         DIE("MOLECULE.MISC:make_u_mp2 ...  Current RAM limit has been exceeded! ")
      end if
      call text_(stdout," Alpha orbital limits:  ")
      call set_MO_limits_(self,evala,fa,lr)
      call text_(stdout," Beta orbital limits:  ")
      call set_MO_limits_(self,evalb,fb,ls)
   !   fa=1; fb=1; lr=.n_bf; ls=.n_bf
      call create_(vaa,self%n_bf,self%n_bf,self%n_bf,self%n_bf)
      call create_(vab,self%n_bf,self%n_bf,self%n_bf,self%n_bf)
      call create_(vbb,self%n_bf,self%n_bf,self%n_bf,self%n_bf)
      vaa = ZERO; vab = ZERO; vbb = ZERO
      call make_u_MO_ERI_integrals_(self,vaa,vbb,vab)
      call flush_(stdout)
      call text_(stdout," The MP2 correlation loops beginning! ")
      E2 = ZERO
      E = ZERO

   ! Full system
      !do a = 1,.n_e/2
      !  do b = 1,.n_e/2
      !    do r = .n_e/2+1,.n_bf
      !      do s = .n_e/2+1,.n_bf

   ! In the case of chosen MO limits
      ! Vaa
      do a = fa,self%n_a
         do b = fa,self%n_a
            do r = self%n_a+1,lr
               do s = self%n_a+1,lr
                  E = E+vaa(a,b,r,s)*vaa(r,s,a,b)/(evala(a)+evala(b)-evala(r)-evala(s))
                  E = E-vaa(a,b,r,s)*vaa(r,s,b,a)/(evala(a)+evala(b)-evala(r)-evala(s))
               end do
            end do
         end do
      end do
      E2 = E
      call show_(stdout," Eaa= ",E2/TWO)
      call text_(stdout," aaaa finished ")
      E = ZERO
      ! Vbb
      do a = fb,self%n_b
         do b = fb,self%n_b
            do r = self%n_b+1,ls
               do s = self%n_b+1,ls
                  E = E+vbb(a,b,r,s)*vbb(r,s,a,b)/(evalb(a)+evalb(b)-evalb(r)-evalb(s))
                  E = E-vbb(a,b,r,s)*vbb(r,s,b,a)/(evalb(a)+evalb(b)-evalb(r)-evalb(s))
               end do
            end do
         end do
      end do
      call show_(stdout," Ebb= ",E/TWO)
      E2 = E2+E
      call text_(stdout," bbbb finished ")
      E = ZERO
      ! Vab = Vba
      do a = fa,self%n_a
         do b = fb,self%n_b
            do r = self%n_a+1,lr
               do s = self%n_b+1,ls
                  E = E+vab(a,b,r,s)*vab(r,s,a,b)/(evala(a)+evalb(b)-evala(r)-evalb(s))
               end do
            end do
         end do
      end do
      call show_(stdout," Eab= ",E)
      call text_(stdout," aabb finished ")
      E2 = E2+E*TWO
      E2 = E2/TWO
      E = ZERO
      E = SCF_energy_(self)
      call flush_(stdout)
      call text_(stdout," MP2 results (in Hartree)")
      call show_(stdout,"SCF energy= ",E)
      call show_(stdout,"MP2 correlation energy= ",E2)
      call show_(stdout,"Total MP2 energy= ",E2+E)
      call flush_(stdout)
      call destroy_(vaa); call destroy_(vab); call destroy_(vbb);
      call destroy_(evala); call destroy_(evalb)
   end subroutine

   subroutine make_gc_mp2(self)
   ! Calculate the electron repulsion integrals (ERI's) in the gc_MO basis from the
   ! AO integrals and make general complex MP2
      type(MOLECULE_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), allocatable :: v
      CPX :: E2
      REAL :: virt,ram_lim,E
      VEC(REAL,:), allocatable :: eval
      INT :: a,b,r,s,fa,lr
   ENSURE(allocated(self%MOs%gc),"MOLECULE.MISC:make_gc_mp2 ... no molecular orbitals")
   ENSURE(allocated(self%MO_energies%g),"MOLECULE.MISC:make_gc_mp2 ... no orbital energies")
      call create_(eval,2*self%n_bf)
      eval = self%MO_energies%g
      call flush_(stdout)
      call text_(stdout," Orbital energies:")
      call flush_(stdout)
      call put_(stdout,eval)
      virt = real(2*self%n_bf)**4*8*2/1024**3
      call flush_(stdout)
      ram_lim = 4.0
      call show_(stdout,"Needed space for ERI in RAM (in Gb) =",virt)
      if (virt>ram_lim) then
         call show_(stdout," Current RAM limit is (in Gb) =",ram_lim)
         DIE("MOLECULE.MISC:make_gc_mp2 ...  Current RAM limit has been exceeded! ")
      end if
      call set_MO_limits_gc_(self,eval,fa,lr)
   !   fa=1; lr=2*.n_bf;
      call create_(v,2*self%n_bf,2*self%n_bf,2*self%n_bf,2*self%n_bf)
      v = (ZERO,ZERO)
      call make_gc_MO_ERI_integrals_(self,v)
      call flush_(stdout)
      call text_(stdout," The MP2 correlation loops beginning! ")
      E2 = (ZERO,ZERO)

      ! Based on Szabo, Ostlund Book, p. 352, eq. 6.74
      ! a,b are occupied spinorbitals
      ! r,s are virtual orbitals

   ! In the case of chosen MO limits
      do a = fa,self%n_e
         do b = fa,self%n_e

      !   do b = a+1,.n_e
            do r = self%n_e+1,lr
               do s = self%n_e+1,lr

      !       do s =  r+1,lr
                  E2 = E2+(v(a,b,r,s)-v(a,b,s,r))*conjg(v(a,b,r,s)-v(a,b,s,r))/(eval(a)+eval(b)-eval(r)-eval(s))
               end do
            end do
         end do
      end do
      E2 = E2/FOUR
      E = ZERO
      E = SCF_energy_(self)
      call flush_(stdout)
      call text_(stdout," MP2 results (in Hartree)")
      call show_(stdout,"SCF energy= ",E)
      call show_(stdout,"MP2 correlation energy= ",E2)
      call show_(stdout,"Total MP2 energy= ",real(E2)+E)
      call flush_(stdout)
      WARN("MOLECULE.MISC:make_gc_mp2 ...  gchf MP2 has not been completely benchmarked yet,")
      WARN("MOLECULE.MISC:make_gc_mp2 ...  but seems more robust than before!")
      call flush_(stdout)
      call destroy_(v)
      call destroy_(eval)
   end subroutine

   subroutine set_MO_limits(self,eval,fa,lr)
   ! Finding the lowest and highest MO
   ! which will define the active space
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:) :: eval
      INT :: fa,lr
      REAL :: Emax,Emin,Edel
      INT :: a,r,i

      ! Set_MO_limits
      ! these should be read from stdin, currently set on hard here in
      Emin = -5.0
      Emax = 100.0
      Edel = 0.1
      fa = 1
      lr = self%n_bf
      if (eval(self%n_e/2)<Emin) then
         DIE("MOLECULE.MISC:set_MO_limits ... Emin is too high, no occupied orbitals")
      end if
      if (eval(self%n_e/2+1)>Emax) then
         DIE("MOLECULE.MISC:set_MO_limits ... Emax is too low, no virtual orbitals")
      end if

   ! fa means first a (occupied)
      do a = 1,self%n_e/2
         if (eval(a)>Emin) then
            call show_(stdout,"found Emin =",a)
            do i = a,2,-1
               if (eval(i)-eval(i-1)>Edel) then
                  fa = i
                  call show_(stdout,"found fa =",fa)
                  exit
               else
                  fa = i-1
               end if
            end do
            exit
         end if
      end do

   ! lr means last r (virtual)
      do r = self%n_e/2+1,self%n_bf
         if (eval(r)>Emax) then
            do i = r,self%n_bf
               if (eval(i)-eval(i-1)>Edel) then
                  lr = i-1
                  exit
               else
                  lr = i
               end if
            end do
            exit
         end if
      end do
      call flush_(stdout)
      call text_(stdout,"First active occupied orbital=")
      call put_(stdout,fa)
      call put_(stdout,eval(fa))
      call flush_(stdout)
      call text_(stdout,"Last active virtual orbital=")
      call put_(stdout,lr)
      call put_(stdout,eval(lr))
      call flush_(stdout)
   end subroutine

   subroutine set_MO_limits_gc(self,eval,fa,lr)
   ! Finding the lowest and highest MO
   ! which will define the active space
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:) :: eval
      INT :: fa,lr
      REAL :: Emax,Emin,Edel
      INT :: a,r,i

      ! Set_MO_limits
      ! these should be read from stdin, currently set on hard here in
      Emin = -5.0
      Emax = 100.0
      Edel = 0.1
      fa = 1
      lr = 2*self%n_bf
      if (eval(self%n_e)<Emin) then
         DIE("MOLECULE.MISC:set_MO_limits_gc ... Emin is too high, no occupied orbitals")
      end if
      if (eval(self%n_e+1)>Emax) then
         DIE("MOLECULE.MISC:set_MO_limits_gc ... Emax is too low, no virtual orbitals")
      end if

   ! fa means first a (occupied)
      do a = 1,self%n_e
         if (eval(a)>Emin) then
            call show_(stdout,"found Emin =",a)
            do i = a,2,-1
               if (eval(i)-eval(i-1)>Edel) then
                  fa = i
                  call show_(stdout,"found fa =",fa)
                  exit
               else
                  fa = i-1
               end if
            end do
            exit
         end if
      end do

   ! lr means last r (virtual)
      do r = self%n_e+1,2*self%n_bf
         if (eval(r)>Emax) then
            do i = r,2*self%n_bf
               if (eval(i)-eval(i-1)>Edel) then
                  lr = i-1
                  exit
               else
                  lr = i
               end if
            end do
            exit
         end if
      end do
      call flush_(stdout)
      call text_(stdout,"First active occupied orbital=")
      call put_(stdout,fa)
      call put_(stdout,eval(fa))
      call flush_(stdout)
      call text_(stdout,"Last active virtual orbital=")
      call put_(stdout,lr)
      call put_(stdout,eval(lr))
      call flush_(stdout)
   end subroutine


!  ===
!  MP2
!  ===

   subroutine test_vdw_bonding(self)
   ! Test for vdw bonding between atom pairs.
      type(MOLECULE_TYPE), IN :: self
      INT :: a,b
      REAL :: val,bond
      REAL :: dx,dy,dz,r2
      BIN :: vdw,cov
   ENSURE(allocated(self%atom),"MOLECULE.MISC:test_vdw_bonding ... no atom list")
      val = 0.4*BOHR_PER_ANGSTROM
      do a = 1,self%n_atom
         do b = a+1,self%n_atom
            vdw = is_vdw_bonded_to_(self%atom(a),self%atom(b),val,10.0d0)
            cov = is_bonded_to_(self%atom(a),self%atom(b),val)
            bond = vdw_radius_ccdc_(self%atom(a))+vdw_radius_ccdc_(self%atom(b))
            bond = bond*1.10d0*ANGSTROM_PER_BOHR
            dx = abs(self%atom(a)%position(1)-self%atom(b)%position(1))*ANGSTROM_PER_BOHR
            dy = abs(self%atom(a)%position(2)-self%atom(b)%position(2))*ANGSTROM_PER_BOHR
            dz = abs(self%atom(a)%position(3)-self%atom(b)%position(3))*ANGSTROM_PER_BOHR
            r2 = dx*dx+dy*dy+dz*dz
            call flush_(stdout)
            call text_(stdout,"=====")
            call show_(stdout,"VDW BONDED?  =",vdw)
            call show_(stdout,"COV BONDED?  =",cov)
            call show_(stdout,"bond dist    =",bond_length_(self%atom,a,b)*ANGSTROM_PER_BOHR)
            call show_(stdout,"vdw_bond_max =",bond)
            call show_(stdout,"dx           =",dx)
            call show_(stdout,"dy           =",dy)
            call show_(stdout,"dz           =",dz)
            call show_(stdout,"r2           =",r2)
            call show_(stdout,"r_vdw        =",sqrt(r2))
            call show_(stdout,"r2<rvdw*rvdw =",r2<bond*bond)
            call flush_(stdout)
            call show_(stdout,"Atom a       =",a)
            call show_(stdout,"Atom a label =",self%atom(a)%tag)
            call show_(stdout,"Atom a r_vdw =",vdw_radius_ccdc_(self%atom(a)))
            call show_(stdout,"Atom b       =",b)
            call show_(stdout,"Atom b label =",self%atom(b)%tag)
            call show_(stdout,"Atom b r_vdw =",vdw_radius_ccdc_(self%atom(b)))
         end do
      end do
   end subroutine

   subroutine put_3_center_overlaps_check(self)
   ! Put out the largest difference between the three-center overlaps
   ! and their quadrature on the DFT grid, and for calibration the same
   ! for the two-center overlaps on that grid.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), allocatable :: S3
      MAT(REAL,:,:), allocatable :: pt,G,A,W,Q
      VEC(REAL,:), allocatable :: wt
      type(SHELL1_TYPE) :: sh,c
      INT :: n_aux,n_pt,s,fs,ls,ns,atom,b,k,fc,lc,m,i
      REAL :: max_S,max_diff,max_diff_2c
   ENSURE(self%basis_info_made,"MOLECULE.MISC:put_3_center_overlaps_check ... no basis info")
      call resolve_auxiliary_bases_(self)
      n_aux = n_auxiliary_bf_(self)
      call create_(S3,self%n_bf,self%n_bf,n_aux)
      call make_3_center_overlaps_(self,S3)

      ! The grid, and the basis functions on it
      call make_atom_partition_info_(self)
      call make_grid_(self%becke_grid,pt,wt)
      n_pt = size(pt,1)
      call create_(G,n_pt,self%n_bf)
      do s = 1,self%n_shell
         call set_shell1_from_(self,s,sh,fs,ls,ns)
         call make_grid_(sh,G(:,fs:ls),pt)
      end do
      call create_(A,n_pt,n_aux)
      lc = 0
      do atom = 1,self%n_atom
         b = self%auxiliary_basis_for_atom(atom)
         do k = 1,self%auxiliary_basis(b)%n_shell
            call set_(c,self%auxiliary_basis(b)%shell(k),self%atom(atom)%position)
            fc = lc+1
            lc = lc+c%n_bf
            call make_grid_(c,A(:,fc:lc),pt)
         end do
      end do

      ! Compare
      call create_(W,n_pt,self%n_bf)
      call create_(Q,self%n_bf,self%n_bf)
      call make_overlap_mx_(self)
      do i = 1,self%n_bf
         W(:,i) = wt*G(:,i)
      end do
      call to_product_of_(Q,G,W,transpose_a=TRUE)
      max_diff_2c = maxval(abs(Q-self%overlap_mx))
      max_S = maxval(abs(S3))
      max_diff = ZERO
      do m = 1,n_aux
         do i = 1,self%n_bf
            W(:,i) = wt*A(:,m)*G(:,i)
         end do
         call to_product_of_(Q,G,W,transpose_a=TRUE)
         max_diff = max(max_diff,maxval(abs(Q-S3(:,:,m))))
      end do
      call flush_(stdout)
      call text_(stdout,"Three-center overlaps against grid quadrature:")
      call show_(stdout,"No. of basis functions            =",self%n_bf)
      call show_(stdout,"No. of auxiliary basis functions  =",n_aux)
      call show_(stdout,"No. of grid points                =",n_pt)
      call show_(stdout,"Largest overlap                   =",max_S)
      call show_(stdout,"Largest difference                =",max_diff)
      call show_(stdout,"Same, two-center overlaps         =",max_diff_2c)
      call destroy_(Q); call destroy_(W); call destroy_(A); call destroy_(G)
      call destroy_(wt); call destroy_(pt)
      call destroy_(S3)
   end subroutine

end module
