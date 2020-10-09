!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine RT_Glesser_evolve(En,kpt,dG_old,dG_in,dG_out,dT,dTp)
 !
 ! dG_old = dG(T0)     --> Time at first iteration in integrator with more than one step
 ! dG_in  = dG(T )     --> Effective time at the present step, i.e. for RK2 at step 2, T=T0+dt/2
 ! dG_out = dG(T0+dt)
 !
 !  T0+dt = T+dt'
 !
 use pars,           ONLY:SP,cZERO
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_G_k_range
 use real_time,      ONLY:l_RT_EULER,l_RT_EXP,l_RT_INV, &
&                         l_RT_RWA,l_RT_DIAG,l_RT_ACC,  &
&                         G_lesser_reference,RT_bands,RT_nk,RTibz
 !
 implicit none
 !
 type(bz_samp),  intent(in) :: kpt
 type(levels),   intent(in) :: En
 !
 real(SP),       intent(in) :: dT,dTp
 complex(SP),    intent(in) :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 complex(SP),    intent(in) :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 complex(SP),   intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 !
 ! Work space
 !
 logical :: l_RT_STD
 integer :: ik
 !
 dG_out=cZERO
 !
 l_RT_STD=.not.(l_RT_ACC.or.l_RT_DIAG)
 !
 do ik=1,RT_nk
   !
   if( .not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
   !
   if (l_RT_EXP.and.l_RT_ACC ) call RT_EXP_step_accurate(dG_old(:,:,ik),dG_in(:,:,ik),dG_out(:,:,ik),ik,dT)
   if (l_RT_EXP.and.l_RT_STD ) call RT_EXP_step_std(dG_old(:,:,ik),dG_in(:,:,ik),dG_out(:,:,ik),ik,dT)
   !
   if (l_RT_EULER            ) call RT_EULER_step(dG_in(:,:,ik),dG_out(:,:,ik),ik,dT)
   !
   if (l_RT_INV.and.l_RT_ACC ) call RT_INV_step_accurate(dG_old(:,:,ik),dG_in(:,:,ik),dG_out(:,:,ik),ik,dT)
   if (l_RT_INV.and.l_RT_DIAG) call RT_INV_step_diago(dG_old(:,:,ik),dG_in(:,:,ik),dG_out(:,:,ik),ik,dT)
   if (l_RT_INV.and.l_RT_STD ) call RT_INV_step_std(dG_old(:,:,ik),dG_in(:,:,ik),dG_out(:,:,ik),ik,dT)
   !
   ! Here the term G(T0) [dG_old] is added to dG(t)
   !
   ! G(T0+dt) = G(T0) e^{-i\delta\epsilon dt} + dGT0() e^{-i\delta\epsilon (dt')}
   !
   ! Dephasing is integrated analitically in this case.
   !
   if(     l_RT_RWA)           call RT_apply_RWA(ik,En,kpt,dT,dTp,dG_out(:,:,ik),dG_old(:,:,ik))
   !
   if(.not.l_RT_RWA)           call RT_Dephasing_step(dG_out(:,:,ik),dG_old(:,:,ik),ik,dT)
   !
   ! GPL_EXCLUDE_START
   !
   ! Carriers relaxation step:
   !
   ! G(T0+dt) = G(T0) - i dt * Sigma_relax(T)
   !
   call RT_Scattering_step(dG_old(:,:,ik),dG_in(:,:,ik),dG_out(:,:,ik),ik,dT)
   !
   ! GPL_EXCLUDE_END
   !
   call RT_G_symmetrization(dG_out(:,:,ik),ik)
   !
 enddo
 !
 contains
   !
   subroutine RT_G_symmetrization(G_inout,ik)
     !
     integer,     intent(in)    :: ik
     complex(SP), intent(inout) :: G_inout(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
     !
     integer                :: ib,ibp
     real(SP)               :: E_occ_tmp,H_occ_tmp
     complex(SP)            :: tmp
     !
     do ib=RT_bands(1),RT_bands(2)
       !
       G_inout(ib,ib)=cmplx(0._SP,aimag(G_inout(ib,ib)))
       !
       E_occ_tmp= aimag(G_inout(ib,ib))+            aimag(G_lesser_reference(ib,ib,ik))
       H_occ_tmp=-aimag(G_inout(ib,ib))+ ( spin_occ-aimag(G_lesser_reference(ib,ib,ik)) )
       !
       if ( E_occ_tmp<0._SP .or. H_occ_tmp>spin_occ ) G_inout(ib,ib)=                      G_lesser_reference(ib,ib,ik)
       if ( H_occ_tmp<0._SP .or. E_occ_tmp>spin_occ ) G_inout(ib,ib)=cmplx(0._SP,spin_occ)-G_lesser_reference(ib,ib,ik)
       !
       do ibp=ib+1,RT_bands(2)
         !
         tmp=( G_inout(ib,ibp)-conjg(G_inout(ibp,ib)) )/2._SP
         !
         G_inout(ib,ibp)=       tmp
         G_inout(ibp,ib)=-conjg(tmp)
         !
       enddo
       !
     enddo
     !
   end subroutine
   !
end subroutine RT_Glesser_evolve
