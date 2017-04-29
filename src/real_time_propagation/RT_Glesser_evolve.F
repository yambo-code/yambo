!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine RT_Glesser_evolve(En,dG_old,dG_in,dG_out,dT,dTp)
 !
 ! dG_old = dG(T0)     --> Time at first iteration in integrator with more than one step
 ! dG_in  = dG(T )     --> Effective time, i.e. at the second step of RK2 T=T0+dt/2
 ! dG_out = dG(T0+dt)
 !
 !  T0+dt = T+dt'
 !
 use pars,           ONLY:SP,cZERO,cONE
 use electrons,      ONLY:levels,spin_occ
 use real_time,      ONLY:l_RT_EULER,l_RT_EXP,l_RT_INV,l_RT_RWA,integrator_step, &
&                         G_lesser_reference,RT_bands,RT_nk
 !
 implicit none
 !
 type(levels),   intent(in) :: En
 !
 real(SP),       intent(in) :: dT,dTp
 complex(SP),    intent(in) :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 complex(SP),    intent(in) :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 complex(SP),   intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Workspace
 !
 integer                :: ik,ib
 complex(SP)            :: I1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)            :: I2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)            :: I1_matrix(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 dG_out=cZERO
 !
 if(l_RT_EXP.or.l_RT_INV) then
   I1_matrix=cZERO
   do ib=RT_bands(1),RT_bands(2)
     I1_matrix(ib,ib)=cONE
   enddo
 endif
 !
 do ik=1,RT_nk
   !
   ! Here we compute dG(t)=G(t+dt)-G(t)
   !
   if (l_RT_EXP) then
     !
     ! U1 = exp ( -i * Ho_plus_Sigma * dt)
     ! U2 = exp ( +i + Ho_plus_Sigma * dt)
     !
     call RT_build_U1_and_U2(ik,I1_k,I2_k,dT)
     !
     ! dG_out = G(T0+dt) -G(T0) = U1(T;dt) G(T0) U2(T;-dt) - G(T0)
     !
     call RT_EXP_or_INV_step(dG_out(:,:,ik),dG_old(:,:,ik),G_lesser_reference(:,:,ik),I1_k,I2_k,I1_matrix)
     !
   else if(l_RT_EULER) then
     !
     ! I1 = Ho_plus_Sigma*G   
     ! I2 = G*Ho_plus_Sigma
     !
     call RT_build_I1_and_I2(dG_in(:,:,ik),G_lesser_reference(:,:,ik),ik,I1_k,I2_k)
     !
     ! dG_out = G(T0+dt) -G(T0)= - i dt *(I1(T) - I2(T))
     !
     call RT_EULER_step(dG_out(:,:,ik),I1_k,I2_k,dT)
     !
   else if(l_RT_INV) then
     !
     ! T1 = (1-idt/2*H) / (1+idt/2*H) -1 
     ! T2 = (1+idt/2*H) / (1-idt/2*H) -1
     !
     call RT_build_T1_and_T2(ik,I1_k,I2_k,i1_matrix,dT)
     !
     ! dG_out = G(T0+dt) -G(T0) = T1(T;dt) G(T0) T2(T;-dt) - G(T0)
     !
     call RT_EXP_or_INV_step(dG_out(:,:,ik),dG_old(:,:,ik),G_lesser_reference(:,:,ik),I1_k,I2_k,I1_matrix)
     !
   endif
   !
   ! Here the term G(T0) [dG_old] is added to dG(t)
   !
   if(l_RT_RWA) then
     !
     ! G(T0+dt) = G(T0) e^{-i\delta\epsilon dt} - i dt *(I1(T) - I2(T)) e^{-i\delta\epsilon (dt')}
     !
     ! Dephasing is integrated analitically in this case.
     !
     call RT_apply_RWA(ik,En,dT,dTp,dG_out(:,:,ik),dG_old(:,:,ik))
     !
   else
     !
     call RT_Dephasing_step(dG_out(:,:,ik),dG_old(:,:,ik),ik,dT)
     !
   endif
   !
   ! Carriers relaxation step:
   !
   ! G(T0+dt) = G(T0) - i dt * Sigma_relax(T)
   !
   call RT_Scattering_step(dG_out(:,:,ik),ik,dT)
   !
 enddo
 !
 call RT_G_symmetrization(dG_out)
 !
 integrator_step=integrator_step+1
 !
 contains
   !
   subroutine RT_G_symmetrization(G_inout)
     !
     complex(SP), intent(inout) :: G_inout(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
     !
     integer                :: ik,ib,ibp
     real(SP)               :: E_occ_tmp,H_occ_tmp
     complex(SP)            :: tmp
     !
     do ik=1,RT_nk
       !
       do ib=RT_bands(1),RT_bands(2)
         !
         G_inout(ib,ib,ik)=cmplx(0._SP,aimag(G_inout(ib,ib,ik)))
         !
         E_occ_tmp= aimag(G_inout(ib,ib,ik))+            aimag(G_lesser_reference(ib,ib,ik))
         H_occ_tmp=-aimag(G_inout(ib,ib,ik))+ ( spin_occ-aimag(G_lesser_reference(ib,ib,ik)) )
         !
         if ( E_occ_tmp<0._SP .or. H_occ_tmp>spin_occ ) G_inout(ib,ib,ik)=                      G_lesser_reference(ib,ib,ik)
         if ( H_occ_tmp<0._SP .or. E_occ_tmp>spin_occ ) G_inout(ib,ib,ik)=cmplx(0._SP,spin_occ)-G_lesser_reference(ib,ib,ik)
         !
         do ibp=ib+1,RT_bands(2)
           !
           tmp=( G_inout(ib,ibp,ik)-conjg(G_inout(ibp,ib,ik)) )/2._SP
           !
           G_inout(ib,ibp,ik)=       tmp
           G_inout(ibp,ib,ik)=-conjg(tmp)
           !
         enddo
         !
       enddo
       !
     enddo
     !
   end subroutine
   !
end subroutine RT_Glesser_evolve
