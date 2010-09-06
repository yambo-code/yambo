!
! Copyright (C) 2000-2010 M. Gruening and the YAMBO team
!              http://www.yambo-code.org
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine OEP_driver(X,E,k,q,V_oep)
  !
  ! Driver for OEP potentials. Variables: 
  !  * the approximation for the OEP eq. (Exact, Slater, KLI) 
  !  * the solution method (iterative ?? la Kuemmel-Perdew or by Chi inversion) 
  !  * the approximation for correlation (nothing, LDA, static RPA)
  !
  ! STATUS (23/08/10)
  !
  ! sRPA available only for OEP_exact_inversion
  ! Collisions available only for OEP_exact_inversion
  ! OEP_kli_inversion, do not work, missing timing/parall
  ! OEP_exact_iterative, do not work
  ! OEP_kli_iterative, partially tested, missing parallel
  ! OEP_Hole, partially tested, missing timing 
  ! NOTES:
  ! In the future also CEDA might be included
  ! In the future we might choose just the iterative option. Now both for testing prupose. 
  ! Later should also distiguish in case of real time evolution between memory/not memory 
  !
  use pars,            ONLY:SP,DP
  use X_m,             ONLY:X_t
  use electrons,       ONLY:levels,n_met_bands
  use R_lattice,       ONLY:bz_samp,nkibz
  use FFT_m,           ONLY:fft_size
  use drivers,         ONLY:l_oep_exact,l_oep_slater,l_oep_kli
  use SC,              ONLY:it_now,l_oep_iterative 
  !
  implicit none
  real(SP),intent(inout)  :: V_oep(fft_size)
  type(X_t),intent(in)    :: X
  type(levels),intent(in) :: E
  type(bz_samp),intent(in):: k,q
  !
  ! Work Space
  !
  real(SP)    ::V_io(fft_size)
  real(SP),allocatable    ::rho(:),V_slt(:)
  complex(SP),allocatable :: W_x(:,:,:)
  !
  V_io = V_oep
  V_oep= 0._SP
  !
  if (l_oep_iterative.or.l_oep_kli.or.l_oep_slater) then
     allocate(rho(fft_size),V_slt(fft_size))
     allocate(W_x(nkibz,n_met_bands,fft_size))
     !   
     call el_density(E,k,rho,.false.)   ! Density and Slater potential, needed always but 
     call OEP_Hole(E,k,q,V_slt,W_x)     ! for the exact oep with Chi inversion
     if (l_oep_slater.or.it_now==1) V_io = V_slt/rho
     if (l_oep_kli.or.(it_now==1.and.l_oep_exact)) then
       if (l_oep_iterative) then
         call OEP_kli_iterative(E,k,V_io,V_slt,W_x,rho)
       else
         call OEP_kli_inversion(E,k,V_oep,V_slt,W_x,rho)
       endif
     end if
     if (it_now>1.and.l_oep_exact) call OEP_exact_iterative(E,k,V_io,W_x,rho)
   else
     call OEP_exact_inversion(X,E,k,q,V_oep)
   end if
   if (l_oep_iterative.or.l_oep_slater) V_oep = V_io
   !
end subroutine OEP_driver


