!---------------------------------------------------------------------------
!
! CELL_MAP: a density of the unit cell held as its Fourier coefficients,
! and the maps made from it.
!
! The coefficients are structure factors already expanded over the whole
! sphere of reciprocal space, on absolute scale and divided by the cell
! volume, so that the density at a point r is the real part of
!
!    rho(r) = sum_n coeff(n) exp(-i q_n.r),   q_n = 2 pi B h_n,
!
! with B the reciprocal cell matrix and h_n the Miller indices of
! reflection n. CRYSTAL:make_cell_map fills the object from diffraction
! data; this module only holds the coefficients and sums the series.
! It holds no crystal, atoms or plot grid: those are passed in.
!
! Copyright (C) Dylan Jayatilaka, 2026
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
!---------------------------------------------------------------------------
module CELL_MAP_MODULE

#  include "macros"

   include "cell_map.use"

   implicit none

   include "cell_map.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object and set defaults
      type(CELL_MAP_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"CELL_MAP:create ... error in allocating memory")
#else
      allocate(self)
#endif
      call set_defaults_(self)
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(CELL_MAP_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"CELL_MAP:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(CELL_MAP_TYPE), INOUT :: self
      call destroy_(self%h1)
      call destroy_(self%h2)
      call destroy_(self%h3)
      call destroy_(self%coeff)
      call destroy_(self%F_pro)
      call destroy_(self%atom_tags)
   end subroutine


!  ===
!  Set
!  ===

   PURE subroutine set_defaults(self)
   ! Set the defaults: a residual map, no angular filter
      type(CELL_MAP_TYPE), INOUT :: self
      self%kind = "residual"
      self%l_value = -1
      self%m_value = -99
      self%window_width = 0.25*BOHR_PER_ANGSTROM
      self%sharpen_U = ZERO
      self%sharpen_order = 0
      self%laplacian_order = 0
      self%wiener_weight = FALSE
      self%l_max = 2
      self%centre = ZERO
      self%n_radii = 40
      self%radius_max = 2*BOHR_PER_ANGSTROM
      self%volume = ZERO
      self%reciprocal_mx = ZERO
      self%direct_mx = ZERO
      self%density_source = "wavefunction"
      self%atom_weight = "hirshfeld"
   end subroutine

   PURE subroutine set_kind(self,kind)
   ! Set the "kind" of density the coefficients describe
      type(CELL_MAP_TYPE), INOUT :: self
      STR(len=*), IN :: kind
      self%kind = kind
      call to_lower_case_(self%kind)
      ENSURE(kind_is_known_(self),"CELL_MAP:set_kind ... unknown kind: "//trim(self%kind))
   end subroutine

   pure function kind_is_known(self) result (res)
   ! Return TRUE if ".kind" is one CRYSTAL:make_cell_map can make
      type(CELL_MAP_TYPE), IN :: self
      BIN :: res
      select case (self%kind)
      case ("residual","f_exp","f_calc","deformation_calc","deformation_exp"); res = TRUE
      case default; res = FALSE
      end select
   end function

   pure function is_deformation_kind(self) result (res)
   ! Return TRUE if ".kind" subtracts the promolecule, so that the
   ! promolecule structure factors must be set first
      type(CELL_MAP_TYPE), IN :: self
      BIN :: res
      res = self%kind=="deformation_calc" OR self%kind=="deformation_exp"
   end function

   PURE subroutine set_promolecule_F(self,F)
   ! Take "F", the promolecule structure factors of the unique reflections,
   ! in the model's conventions (the same positions, ADPs and phase
   ! convention as F_calc), for the deformation kinds
      type(CELL_MAP_TYPE), INOUT :: self
      VEC(CPX,:), IN :: F
      call destroy_(self%F_pro)
      call create_(self%F_pro,size(F))
      self%F_pro = F
   end subroutine

   PURE subroutine set_cell(self,unit_cell)
   ! Take the reciprocal cell matrix and the volume from "unit_cell"
      type(CELL_MAP_TYPE), INOUT :: self
      type(UNIT_CELL_TYPE), IN :: unit_cell
   ENSURE(unit_cell%volume>ZERO,"CELL_MAP:set_cell ... unit cell has no volume")
      self%reciprocal_mx = unit_cell%reciprocal_mx
      self%direct_mx = unit_cell%direct_mx
      self%volume = unit_cell%volume
   end subroutine

   PURE subroutine set_coefficients(self,h1,h2,h3,coeff)
   ! Take the Miller indices "h1", "h2", "h3" and the coefficients "coeff",
   ! one per reflection over the whole sphere and on absolute scale. The
   ! coefficients are divided by the cell volume here, so set_cell must
   ! have been called first.
      type(CELL_MAP_TYPE), INOUT :: self
      VEC(INT,:), IN :: h1,h2,h3
      VEC(CPX,:), IN :: coeff
      INT :: n
   ENSURE(self%volume>ZERO,"CELL_MAP:set_coefficients ... set the cell first")
   ENSURE(size(h1)==size(coeff),"CELL_MAP:set_coefficients ... h1: wrong dim")
   ENSURE(size(h2)==size(coeff),"CELL_MAP:set_coefficients ... h2: wrong dim")
   ENSURE(size(h3)==size(coeff),"CELL_MAP:set_coefficients ... h3: wrong dim")
      n = size(coeff)

      ! Only the coefficient arrays: the settings and the promolecule stay
      call destroy_(self%h1)
      call destroy_(self%h2)
      call destroy_(self%h3)
      call destroy_(self%coeff)
      call create_(self%h1,n); self%h1 = h1
      call create_(self%h2,n); self%h2 = h2
      call create_(self%h3,n); self%h3 = h3
      call create_(self%coeff,n)
      self%coeff = coeff/self%volume
   end subroutine


!  =======
!  Inquiry
!  =======

   pure function n_reflections(self) result (res)
   ! The number of reflections held, over the whole sphere
      type(CELL_MAP_TYPE), IN :: self
      INT :: res
      if (allocated(self%coeff)) then; res = size(self%coeff)
      else; res = 0
      end if
   end function

   PURE function electron_count(self) result (res)
   ! The integral of the density over the cell: the real part of the
   ! coefficient of (0,0,0) times the volume. Zero for a map with no
   ! (0,0,0) term, such as a residual map.
      type(CELL_MAP_TYPE), IN :: self
      REAL :: res
      INT :: n
   ENSURE(allocated(self%coeff),"CELL_MAP:electron_count ... no coefficients")
      res = ZERO
      do n = 1,size(self%coeff)
         if (self%h1(n)/=0 OR self%h2(n)/=0 OR self%h3(n)/=0) cycle
         res = res+REALIFY(self%coeff(n))*self%volume
      end do
   end function


!  ====
!  Maps
!  ====

   PURE subroutine make_scattering_vectors(self,q)
   ! Set row n of "q" to the scattering vector 2 pi B h_n of reflection n,
   ! in cartesian coordinates, with B the reciprocal cell matrix.
      type(CELL_MAP_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: q
      VEC(REAL,3) :: h
      INT :: n
   ENSURE(allocated(self%coeff),"CELL_MAP:make_scattering_vectors ... no coefficients")
   ENSURE(size(q,1)==size(self%coeff),"CELL_MAP:make_scattering_vectors ... q: wrong dim1")
   ENSURE(size(q,2)==3,"CELL_MAP:make_scattering_vectors ... q: wrong dim2")
      do n = 1,size(self%coeff)
         h = [self%h1(n),self%h2(n),self%h3(n)]
         q(n,:) = 2*PI*matmul(self%reciprocal_mx,h)
      end do
   end subroutine

   PURE subroutine make_values_at(self,values,pts)
   ! Set "values" to the density at the cartesian points "pts", one row
   ! per point, by summing the Fourier series at each point.
      type(CELL_MAP_TYPE), IN :: self
      VEC(REAL,:), OUT :: values
      MAT(REAL,:,:), IN :: pts
      MAT(REAL,:,:), allocatable :: q
   ENSURE(allocated(self%coeff),"CELL_MAP:make_values_at ... no coefficients")
   ENSURE(size(values)==size(pts,1),"CELL_MAP:make_values_at ... values: wrong dim")
   ENSURE(size(pts,2)==3,"CELL_MAP:make_values_at ... pts: wrong dim2")
      call create_(q,size(self%coeff),3)
      call make_scattering_vectors_(self,q)
      call fourier_series_at_(values,q,self%coeff,pts)
      call destroy_(q)
   end subroutine

   PURE subroutine make_map_at(self,values,pts)
   ! Set "values" to the map these settings describe, at the cartesian
   ! points "pts", one row per point: the density itself when l_value is
   ! negative, otherwise the local moment of angular momentum l_value.
      type(CELL_MAP_TYPE), IN :: self
      VEC(REAL,:), OUT :: values
      MAT(REAL,:,:), IN :: pts
      if (self%l_value<0) then
         call make_values_at_(self,values,pts)
      else
         call make_moment_at_(self,values,pts)
      end if
   end subroutine

   PURE subroutine make_moment_at(self,values,pts)
   ! Set "values" to the local moment of the density of angular momentum
   ! l = l_value, scaled to the units of a density, at the cartesian
   ! points "pts". The moment is the Fourier series with coefficient n
   ! multiplied by
   !
   !    (-i)^l sigma^l q^l Y_lm(q^) exp(-sigma^2 q^2/2),
   !
   ! with q the scattering vector of reflection n, sigma the window width
   ! and Y_lm the real harmonic normalised to 1 over the sphere: it is the
   ! moment of the density seen through a Gaussian window of width sigma
   ! centred at the point, divided by sigma^l. With sharpen_U set, a mean
   ! ADP U is divided out of every reflection first, which is the same
   ! formula with sigma^2 - U in the exponent; it is allowed only while
   ! that is positive, so the series still converges. With sharpen_order
   ! the exact factor exp(U q^2/2) is replaced by its Taylor polynomial of
   ! that order, a sum of Laplacian powers, bounded by the window for any
   ! U. With laplacian_order the map is multiplied by (-sigma^2 q^2)^k, its
   ! k-th Laplacian scaled by sigma^2k. With m_value set it is that
   ! one component, in the frame of the cartesian axes; otherwise it is the
   ! norm over m, which does not depend on the axes. l = 0 has one
   ! component and keeps its sign.
   ! m is the usual real-harmonic index, m > 0 cosine-like and m < 0
   ! sine-like: for l = 1, m = -1, 0, 1 are y, z, x, and the three maps are
   ! sqrt(3/4 pi) sigma times the gradient of the blurred density
   ! (FOURIER_SUMS:harmonic_column maps m onto the columns).
      type(CELL_MAP_TYPE), IN :: self
      VEC(REAL,:), OUT :: values
      MAT(REAL,:,:), IN :: pts
      MAT(REAL,:,:), allocatable :: q,Y
      VEC(CPX,:), allocatable :: c
      VEC(REAL,:), allocatable :: comp
      REAL :: q2,sig,net
      CPX :: fac
      INT :: n_refl,n,lv,m,m_lo,m_hi,col
   ENSURE(allocated(self%coeff),"CELL_MAP:make_moment_at ... no coefficients")
   ENSURE(size(values)==size(pts,1),"CELL_MAP:make_moment_at ... values: wrong dim")
   ENSURE(size(pts,2)==3,"CELL_MAP:make_moment_at ... pts: wrong dim2")
   ENSURE(self%l_value>=0,"CELL_MAP:make_moment_at ... l_value not set")
   ENSURE(self%m_value==-99 OR abs(self%m_value)<=self%l_value,"CELL_MAP:make_moment_at ... m_value out of range")
   ENSURE(self%window_width>ZERO,"CELL_MAP:make_moment_at ... window_width not set")
   ENSURE(self%sharpen_order>0 OR self%sharpen_U<self%window_width**2,"CELL_MAP:make_moment_at ... sharpen_U must be below window_width^2")
      n_refl = size(self%coeff)
      lv = self%l_value
      sig = self%window_width
      fac = (-IMAGIFY(ONE))**lv*sig**lv

      ! The net blur: the window less the mean ADP divided out, when the
      ! sharpening is exact; with a Taylor polynomial of order sharpen_order
      ! the window stays whole and the polynomial in q^2 does the sharpening,
      ! which is bounded whatever U
      net = sig*sig
      if (self%sharpen_order==0) net = net-self%sharpen_U

      ! The scattering vectors, and the solid harmonics q^l Y_lm(q^) at them
      call create_(q,n_refl,3)
      call make_scattering_vectors_(self,q)
      call create_(Y,n_refl,(lv+1)*(lv+1))
      call make_solid_harmonics_(Y,q,lv)

      ! Which components
      if (self%m_value==-99) then
         m_lo = -lv; m_hi = lv
      else
         m_lo = self%m_value; m_hi = self%m_value
      end if

      ! Each component's series, squared and added
      call create_(c,n_refl)
      call create_(comp,size(values))
      values = ZERO
      do m = m_lo,m_hi
         col = harmonic_column_(lv,m)
         do n = 1,n_refl
            q2 = q(n,1)*q(n,1)+q(n,2)*q(n,2)+q(n,3)*q(n,3)
            c(n) = self%coeff(n)*fac*Y(n,col)*exp(-net*q2/2)
            if (self%sharpen_order>0) c(n) = c(n)*sharpening_polynomial_(self,q2)
            if (self%laplacian_order>0) c(n) = c(n)*(-sig*sig*q2)**self%laplacian_order
         end do
         call fourier_series_at_(comp,q,c,pts)
         if (m_lo==m_hi) then
            values = comp
         else
            values = values+comp*comp
         end if
      end do
      if (m_lo/=m_hi) values = sqrt(values)
      call destroy_(comp)
      call destroy_(c)
      call destroy_(Y)
      call destroy_(q)
   end subroutine

   PURE subroutine make_radial_functions(self,f,radii)
   ! Set "f(i,lm)" to the radial functions rho_lm(s) of the density about
   ! ".centre" at the radii "radii(i)", for l = 0 to ".l_max" and m = -l
   ! to l, column lm = l^2 + l + m + 1:
   !
   !    rho_lm(s) = 4 pi (-i)^l sum_n coeff(n) exp(-i q_n.c) Y_lm(q^_n) j_l(q_n s),
   !
   ! the expansion of the density on each sphere about the centre c in
   ! real harmonics, so that rho(c + s s^) = sum_lm rho_lm(s) Y_lm(s^).
   ! It is exact for the Fourier series, and is the reference that an
   ! angular quadrature about an atom is tested against. The factor of
   ! each reflection is real once the phase about the centre and (-i)^l
   ! are taken together, so the sum over radii is real arithmetic.
      type(CELL_MAP_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: f
      VEC(REAL,:), IN :: radii
      MAT(REAL,:,:), allocatable :: q,Y,A
      VEC(REAL,:), allocatable :: qn,j
      CPX :: ph,fac
      INT :: n_refl,n,i,lv,m,col,lm
   ENSURE(allocated(self%coeff),"CELL_MAP:make_radial_functions ... no coefficients")
   ENSURE(self%l_max>=0,"CELL_MAP:make_radial_functions ... l_max must not be negative")
   ENSURE(size(f,1)==size(radii),"CELL_MAP:make_radial_functions ... f: wrong dim1")
   ENSURE(size(f,2)==(self%l_max+1)*(self%l_max+1),"CELL_MAP:make_radial_functions ... f: wrong dim2")
      n_refl = size(self%coeff)

      ! The scattering vectors, their lengths, and the solid harmonics
      call create_(q,n_refl,3)
      call make_scattering_vectors_(self,q)
      call create_(qn,n_refl)
      do n = 1,n_refl
         qn(n) = sqrt(q(n,1)*q(n,1)+q(n,2)*q(n,2)+q(n,3)*q(n,3))
      end do
      call create_(Y,n_refl,(self%l_max+1)*(self%l_max+1))
      call make_solid_harmonics_(Y,q,self%l_max)

      ! A(n,lm) = Re[4 pi (-i)^l coeff(n) exp(-i q.c) Y_lm(q^)], the columns in
      ! the order l^2 + l + m + 1; Y_lm(q^) is the solid harmonic over q^l,
      ! and at q = 0 only l = 0 survives
      call create_(A,n_refl,(self%l_max+1)*(self%l_max+1))
      do n = 1,n_refl
         ph = self%coeff(n)*exp(-IMAGIFY(ONE)*(q(n,1)*self%centre(1)+q(n,2)*self%centre(2)+q(n,3)*self%centre(3)))
         do lv = 0,self%l_max
            fac = 4*PI*(-IMAGIFY(ONE))**lv*ph
            do m = -lv,lv
               lm = lv*lv+lv+m+1
               col = harmonic_column_(lv,m)
               if (qn(n)>1.0d-12) then
                  A(n,lm) = REALIFY(fac)*Y(n,col)/qn(n)**lv
               else if (lv==0) then
                  A(n,lm) = REALIFY(fac)*Y(n,col)
               else
                  A(n,lm) = ZERO
               end if
            end do
         end do
      end do

      ! The sum over reflections at each radius
      call create_(j,self%l_max+1)
      f = ZERO
      do i = 1,size(radii)
         do n = 1,n_refl
            call spherical_bessel_(j,qn(n)*radii(i),self%l_max)
            do lv = 0,self%l_max
               do m = -lv,lv
                  lm = lv*lv+lv+m+1
                  f(i,lm) = f(i,lm)+A(n,lm)*j(lv+1)
               end do
            end do
         end do
      end do
      call destroy_(j)
      call destroy_(A)
      call destroy_(Y)
      call destroy_(qn)
      call destroy_(q)
   end subroutine

   pure function sharpening_polynomial(self,q2) result (res)
   ! The Taylor polynomial of order ".sharpen_order" of exp(U q^2/2), the
   ! factor that divides a mean ADP U out of a reflection with q^2 = "q2":
   ! the backward step in scale as a sum of Laplacian powers, which is
   ! bounded by the window for any U
      type(CELL_MAP_TYPE), IN :: self
      REAL, IN :: q2
      REAL :: res
      REAL :: term
      INT :: j
      res = ONE
      term = ONE
      do j = 1,self%sharpen_order
         term = term*(self%sharpen_U*q2/2)/j
         res = res+term
      end do
   end function

   PURE subroutine make_values_on_cell(self,values,nx,ny,nz)
   ! Set "values" to the density on a grid of "nx", "ny", "nz" points
   ! along the cell axes, both ends of each axis included, incrementing
   ! first on x, then y, then z, as PLOT_GRID:make_points does. The phase
   ! of each reflection separates into one factor per axis, so the three
   ! one-dimensional tables of phases are made once and the sum at a
   ! point is a product of three table entries.
      type(CELL_MAP_TYPE), IN :: self
      VEC(REAL,:), OUT :: values
      INT, IN :: nx,ny,nz
      MAT(CPX,:,:), allocatable :: phx,phy,phz
      INT :: ix,iy,iz,n,p,n_refl
      REAL :: xi,yi,zi,del
      CPX :: ci
   ENSURE(allocated(self%coeff),"CELL_MAP:make_values_on_cell ... no coefficients")
   ENSURE(size(values)==nx*ny*nz,"CELL_MAP:make_values_on_cell ... values: wrong dim")
   ENSURE(nx>1 AND ny>1 AND nz>1,"CELL_MAP:make_values_on_cell ... need at least two points on each axis")
      n_refl = size(self%coeff)
      ci = -2*PI*IMAGIFY(ONE)

      ! The x phases
      call create_(phx,n_refl,nx)
      del = ONE/(nx-1)
      xi = ZERO
      do ix = 1,nx
         do n = 1,n_refl
            phx(n,ix) = exp(ci*self%h1(n)*xi)
         end do
         xi = xi+del
      end do

      ! The y phases
      call create_(phy,n_refl,ny)
      del = ONE/(ny-1)
      yi = ZERO
      do iy = 1,ny
         do n = 1,n_refl
            phy(n,iy) = exp(ci*self%h2(n)*yi)
         end do
         yi = yi+del
      end do

      ! The z phases, carrying the coefficient
      call create_(phz,n_refl,nz)
      del = ONE/(nz-1)
      do iz = 1,nz
         zi = (iz-1)*del
         do n = 1,n_refl
            phz(n,iz) = exp(ci*self%h3(n)*zi)*self%coeff(n)
         end do
      end do

      ! The map
      p = 0
      do iz = 1,nz
         do iy = 1,ny
            do ix = 1,nx
               p = p+1
               values(p) = sum(REALIFY(phx(:,ix)*phy(:,iy)*phz(:,iz)))
            end do
         end do
      end do
      call destroy_(phz)
      call destroy_(phy)
      call destroy_(phx)
   end subroutine


!  =====
!  Input
!  =====

   subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(CELL_MAP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","CELL_MAP:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"CELL_MAP:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      type(CELL_MAP_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                  ")  ! exit case
      case ("kind=              "); call read_kind_(self)
      case ("l_value=           "); call read_l_value_(self)
      case ("m_value=           "); call read_m_value_(self)
      case ("window_width=      "); call read_window_width_(self)
      case ("sharpen_u=         "); call read_sharpen_U_(self)
      case ("sharpen_order=     "); call read_sharpen_order_(self)
      case ("laplacian_order=   "); call read_laplacian_order_(self)
      case ("wiener_weight=     "); call read_wiener_weight_(self)
      case ("l_max=             "); call read_l_max_(self)
      case ("centre=            "); call read_centre_(self)
      case ("center=            "); call read_centre_(self)
      case ("centre_fractional= "); call read_centre_fractional_(self)
      case ("center_fractional= "); call read_centre_fractional_(self)
      case ("n_radii=           "); call read_n_radii_(self)
      case ("radius_max=        "); call read_radius_max_(self)
      case ("density_source=    "); call read_density_source_(self)
      case ("atom_weight=       "); call read_atom_weight_(self)
      case ("atoms=             "); call read_atoms_(self)
      case default; allocate(tonto%known_keywords(19))
      tonto%known_keywords(1) = "}                  "
      tonto%known_keywords(2) = "kind=              "
      tonto%known_keywords(3) = "l_value=           "
      tonto%known_keywords(4) = "m_value=           "
      tonto%known_keywords(5) = "window_width=      "
      tonto%known_keywords(6) = "sharpen_u=         "
      tonto%known_keywords(7) = "sharpen_order=     "
      tonto%known_keywords(8) = "laplacian_order=   "
      tonto%known_keywords(9) = "wiener_weight=     "
      tonto%known_keywords(10) = "l_max=             "
      tonto%known_keywords(11) = "centre=            "
      tonto%known_keywords(12) = "center=            "
      tonto%known_keywords(13) = "centre_fractional= "
      tonto%known_keywords(14) = "center_fractional= "
      tonto%known_keywords(15) = "n_radii=           "
      tonto%known_keywords(16) = "radius_max=        "
      tonto%known_keywords(17) = "density_source=    "
      tonto%known_keywords(18) = "atom_weight=       "
      tonto%known_keywords(19) = "atoms=             "
      call unknown_(tonto,word,"CELL_MAP:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_kind(self)
   ! Read which density of the cell to make
      type(CELL_MAP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: kind
      call read_(stdin,kind)
      call set_kind_(self,kind)
   end subroutine

   subroutine read_l_value(self)
   ! Read the angular momentum of a local-moment map, and make sure the
   ! harmonics up to it are available. A negative value asks for the plain
   ! density. Setting l resets m_value, so the map is the norm over m
   ! until m_value= is given again.
      type(CELL_MAP_TYPE), INOUT :: self
      call read_(stdin,self%l_value)
      self%m_value = -99
      if (self%l_value>=0) call set_indices_(self%l_value)
   end subroutine

   subroutine read_m_value(self)
   ! Read one component m of a local-moment map; without it the map is
   ! the norm over m. l_value must come first.
      type(CELL_MAP_TYPE), INOUT :: self
      call read_(stdin,self%m_value)
      DIE_IF(self%l_value<0,"CELL_MAP:read_m_value ... set l_value before m_value")
      DIE_IF(abs(self%m_value)>self%l_value,"CELL_MAP:read_m_value ... m_value must lie between -l_value and l_value")
   end subroutine

   subroutine read_window_width(self)
   ! Read the width of the Gaussian window of a local-moment map, a length
      type(CELL_MAP_TYPE), INOUT :: self
      call read_quantity_(stdin,self%window_width)
      DIE_IF(self%window_width<=ZERO,"CELL_MAP:read_window_width ... window_width must be positive")
   end subroutine

   subroutine read_sharpen_U(self)
   ! Read the mean ADP divided out of a local-moment map, an area with
   ! units (angstrom^2); it must stay below the window width squared
      type(CELL_MAP_TYPE), INOUT :: self
      call read_quantity_(stdin,self%sharpen_U)
      DIE_IF(self%sharpen_U<ZERO,"CELL_MAP:read_sharpen_U ... sharpen_u must not be negative")
      DIE_IF(self%sharpen_order==0 AND self%sharpen_U>=self%window_width**2,"CELL_MAP:read_sharpen_U ... sharpen_u must be below window_width^2: blur more than you sharpen, or set sharpen_order")
   end subroutine

   subroutine read_sharpen_order(self)
   ! Read the order of the Taylor polynomial that replaces the exact
   ! sharpening factor; 0 keeps the exact factor
      type(CELL_MAP_TYPE), INOUT :: self
      call read_(stdin,self%sharpen_order)
      DIE_IF(self%sharpen_order<0,"CELL_MAP:read_sharpen_order ... sharpen_order must not be negative")
   end subroutine

   subroutine read_laplacian_order(self)
   ! Read how many Laplacians to apply to a local-moment map
      type(CELL_MAP_TYPE), INOUT :: self
      call read_(stdin,self%laplacian_order)
      DIE_IF(self%laplacian_order<0,"CELL_MAP:read_laplacian_order ... laplacian_order must not be negative")
   end subroutine

   subroutine read_wiener_weight(self)
   ! Read whether each observed coefficient is weighted by F^2/(F^2+sigma_F^2)
      type(CELL_MAP_TYPE), INOUT :: self
      call read_(stdin,self%wiener_weight)
   end subroutine

   subroutine read_l_max(self)
   ! Read the highest l of the radial functions about a centre
      type(CELL_MAP_TYPE), INOUT :: self
      call read_(stdin,self%l_max)
      DIE_IF(self%l_max<0,"CELL_MAP:read_l_max ... l_max must not be negative")
      call set_indices_(self%l_max)
   end subroutine

   subroutine read_centre(self)
   ! Read the centre of the radial functions, cartesian, with units
      type(CELL_MAP_TYPE), INOUT :: self
      call read_quantity_(stdin,self%centre)
   end subroutine

   subroutine read_centre_fractional(self)
   ! Read the centre of the radial functions in fractional coordinates.
   ! The cell must be known: give crystal= first.
      type(CELL_MAP_TYPE), INOUT :: self
      VEC(REAL,3) :: frac
      call read_(stdin,frac)
      DIE_IF(self%volume<=ZERO,"CELL_MAP:read_centre_fractional ... the cell is not set: give crystal= first")
      self%centre = matmul(self%direct_mx,frac)
   end subroutine

   subroutine read_n_radii(self)
   ! Read the number of radii of the radial functions
      type(CELL_MAP_TYPE), INOUT :: self
      call read_(stdin,self%n_radii)
      DIE_IF(self%n_radii<1,"CELL_MAP:read_n_radii ... n_radii must be positive")
   end subroutine

   subroutine read_radius_max(self)
   ! Read the largest radius of the radial functions, a length
      type(CELL_MAP_TYPE), INOUT :: self
      call read_quantity_(stdin,self%radius_max)
      DIE_IF(self%radius_max<=ZERO,"CELL_MAP:read_radius_max ... radius_max must be positive")
   end subroutine

   subroutine read_density_source(self)
   ! Read which density the Hirshfeld-atom decomposition expands: the
   ! wavefunction's, or the Fourier density of this cell map
      type(CELL_MAP_TYPE), INOUT :: self
      call read_(stdin,self%density_source)
      call to_lower_case_(self%density_source)
      DIE_IF(self%density_source/="wavefunction" AND self%density_source/="cell_map","CELL_MAP:read_density_source ... density_source must be wavefunction or cell_map")
   end subroutine

   subroutine read_atom_weight(self)
   ! Read whether the Hirshfeld weight is applied in the atom decomposition
      type(CELL_MAP_TYPE), INOUT :: self
      call read_(stdin,self%atom_weight)
      call to_lower_case_(self%atom_weight)
      DIE_IF(self%atom_weight/="hirshfeld" AND self%atom_weight/="none","CELL_MAP:read_atom_weight ... atom_weight must be hirshfeld or none")
   end subroutine

   subroutine read_atoms(self)
   ! Read the atoms of a rebuilt-atoms map, a list of indices or tags in
   ! braces, atoms= { O1 C1 3 }; without it every atom is used
      type(CELL_MAP_TYPE), INOUT :: self
      call read_all_(stdin,self%atom_tags)
   end subroutine


!  ======
!  Output
!  ======

   subroutine put_radial_functions(self)
   ! Put the radial functions rho_lm(s) of the density about ".centre",
   ! l = 0 to ".l_max", at ".n_radii" radii up to ".radius_max", in
   ! electrons per cubic Angstrom; then the power of each l, the sum of
   ! the squares over m, which does not depend on the axes.
      type(CELL_MAP_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: f
      VEC(REAL,:), allocatable :: radii
      REAL :: power
      REAL :: fac,fac3
      INT :: i,lv,m,lm,n_lm
      n_lm = (self%l_max+1)*(self%l_max+1)
      call create_(radii,self%n_radii)
      do i = 1,self%n_radii
         radii(i) = i*self%radius_max/self%n_radii
      end do
      call create_(f,self%n_radii,n_lm)
      call make_radial_functions_(self,f,radii)
      fac = ANGSTROM_PER_BOHR
      fac3 = BOHR_PER_ANGSTROM**3
      call flush_(stdout)
      call text_(stdout,"====================================")
      call text_(stdout,"Radial functions of the cell density")
      call text_(stdout,"====================================")
      call flush_(stdout)
      call show_(stdout,"Kind                    =",self%kind)
      call show_(stdout,"Centre (Angstrom)       =",self%centre*fac)
      call show_(stdout,"l_max                   =",self%l_max)
      call text_(stdout,". rho_lm(s) in e/Angstrom^3, s in Angstrom; the density on the sphere")
      call text_(stdout,"  of radius s is the sum over l, m of rho_lm(s) Y_lm, with Y_lm real")
      call text_(stdout,"  harmonics of unit integral; m = -1, 0, 1 are y, z, x")
      call flush_(stdout)

      ! The table of rho_lm(s)
      call text_(stdout,"rho_lm(s), columns (l,m):")
      call flush_(stdout)
      do i = 1,self%n_radii
         call put_(stdout,radii(i)*fac,width=10,precision=5)
         do lm = 1,n_lm
            call put_(stdout,f(i,lm)*fac3,width=12,precision=5)
         end do
         call flush_(stdout)
      end do

      ! The power of each l
      call flush_(stdout)
      call text_(stdout,"Power P_l(s) = sum over m of rho_lm(s)^2, columns l = 0 to l_max:")
      call flush_(stdout)
      do i = 1,self%n_radii
         call put_(stdout,radii(i)*fac,width=10,precision=5)
         do lv = 0,self%l_max
            power = ZERO
            do m = -lv,lv
               lm = lv*lv+lv+m+1
               power = power+(f(i,lm)*fac3)**2
            end do
            call put_(stdout,power,width=12,precision=5)
         end do
         call flush_(stdout)
      end do
      call destroy_(f)
      call destroy_(radii)
   end subroutine

   subroutine put(self)
   ! Put the settings and the size of the map
      type(CELL_MAP_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"=============")
      call text_(stdout,"Cell map data")
      call text_(stdout,"=============")
      call flush_(stdout)
      call show_(stdout,"Kind                    =",self%kind)
      call show_(stdout,"No. of reflections      =",n_reflections_(self))
      call show_(stdout,"Cell volume             =",self%volume)
      call show_(stdout,"Electron count          =",electron_count_(self))
      if (self%l_value>=0) then
         call show_(stdout,"l_value                 =",self%l_value)
         if (self%m_value/=-99) call show_(stdout,"m_value                 =",self%m_value)
         call show_(stdout,"Window width            =",self%window_width)
         call show_(stdout,"Sharpening U            =",self%sharpen_U)
         call show_(stdout,"Sharpening order        =",self%sharpen_order)
         call show_(stdout,"Laplacian order         =",self%laplacian_order)
         call show_(stdout,"Wiener weight?          =",self%wiener_weight)
      end if
      call show_(stdout,"l_max                   =",self%l_max)
      call show_(stdout,"Centre                  =",self%centre)
      call show_(stdout,"No. of radii            =",self%n_radii)
      call show_(stdout,"Largest radius          =",self%radius_max)
   end subroutine

end module
