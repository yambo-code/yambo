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
  !  * the approximation for the OEP eq. (Exact, Slater, CEDA, KLI) 
  !  * the solution method (iterative Kuemmel-Perdew way or by Chi inversion) 
  !  * the approximation for correlation (nothing, LDA, static RPA)
  !
  ! STATUS (7/02/11)
  !
  ! sRPA available only for OEP_exact_inversion
  ! Collisions available only for OEP_exact_inversion
  ! OEP_app_inversion, partially tested/CEDA scf does not converge well
  ! OEP_exact_inversion, results are far from reasonable, something is broken(!?)
  ! OEP_iterative, partially tested, missing parallel
  ! OEP_Hole, partially tested 
  ! NOTES:
  ! In the future we might choose just the iterative option. Now both for testing purpose. 
  ! Later should also distiguish in case of real time evolution between memory/not memory 
  !
  use pars,            ONLY:SP,DP
  use X_m,             ONLY:X_t
  use electrons,       ONLY:levels,n_met_bands
  use R_lattice,       ONLY:bz_samp,nkibz
  use FFT_m,           ONLY:fft_size
  use drivers,         ONLY:l_oep_exact,l_oep_slater,l_oep_kli,l_oep_ceda
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
  allocate(rho(fft_size),V_slt(fft_size))
  allocate(W_x(nkibz,n_met_bands,fft_size))
  if (l_oep_iterative.or.(l_oep_kli.or.l_oep_ceda)) then
    call el_density(E,k,rho,.false.)     
    call OEP_Hole(E,k,q,V_slt,W_x)   !this can be changed, W_x can be used in case of exact/noniterative oep  
  endif
  if (l_oep_slater .or. it_now==1) V_io = V_slt/rho
  !
  if (l_oep_iterative) call OEP_iterative(E,k,V_io,V_slt,W_x,rho)
  if (.not.l_oep_iterative.and..not.l_oep_slater) then
     if (.not.l_oep_exact) then
       call OEP_app_inversion(E,k,V_oep,V_slt,W_x,rho)
     else
       call OEP_exact_inversion(X,E,k,q,W_x,V_oep)
     endif	
   end if
   deallocate(rho,V_slt,W_x)
   if (l_oep_iterative.or.l_oep_slater) V_oep = V_io
   !
 end subroutine OEP_driver
