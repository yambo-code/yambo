!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine K_dichroism_IP(iq,Xk,BSE_scatt,w,Dip)
 !
 use pars,           ONLY:SP,cZERO,cI,pi
 use LIVE_t,         ONLY:live_timing
 use drivers,        ONLY:l_rpa_IP
 use DIPOLES,        ONLY:DIPOLE_t,DIP_rotated
 use X_m,            ONLY:X_dichroism,global_gauge
 use BS_solvers,     ONLY:BSS_add_antiresonant,BSS_n_freqs,Co_factor
 use BS,             ONLY:BS_T_grp,BS_nT_grps,BS_K_coupling,BS_perturbative_SOC
 use electrons,      ONLY:n_sp_pol
 use R_lattice,      ONLY:bz_samp,qindx_X,q0_def_norm
 use collision_el,   ONLY:elemental_collision
 use parallel_m,     ONLY:PAR_BS_nT_col_grps,PP_redux_wait,&
&                         PAR_IND_T_groups,PAR_Xk_bz_index,&
&                         PAR_COM_eh_INDEX,PAR_COM_Xk_ibz_INDEX
 use vec_operate,    ONLY:v_norm
 !
#include<memory.h>
 !
 type(DIPOLE_t),               intent(in) :: Dip
 type(bz_samp),                intent(in) :: Xk
 type(elemental_collision), intent(inout) :: BSE_scatt
 integer,    intent(in) :: iq
 complex(SP),intent(in) :: w(BSS_n_freqs) 
 !
 ! Work Space
 !
 complex(SP)       :: E_plus_W0,res,E_plus_W(2/n_sp_pol),DIP_projected,field_dir(3)      
 integer           ::  ik_bz,ok_bz,ik,ok,isymm,osymm,iGo,iv,ic,i_sp_pol,i_sp_pol_rev,i_T_g,i_T,n_SOC,i_pert_SOC,io_err
 !
 n_SOC=1
 i_sp_pol_rev=1
 !
 if (trim(global_gauge)=='length'   ) field_dir=Dip%q0/v_norm(Dip%q0)
 if (trim(global_gauge)=='velocity' ) call error(' velocity gauge not implemented for dichroism')
 if( .not.BS_perturbative_SOC.or. n_sp_pol==2) n_SOC=1
 if(      BS_perturbative_SOC.and.n_sp_pol==1) n_SOC=2
 !
 if (l_rpa_IP) call live_timing('IP Dichroism',PAR_BS_nT_col_grps)   ! EM: writes run status in log file
 !
 do i_T_g=1,BS_nT_grps
   !    
   if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
   !    
   do i_T=1,BS_T_grp(i_T_g)%size    
     !    
     ik_bz    = BS_T_grp(i_T_g)%table(i_T,1)
     iv       = BS_T_grp(i_T_g)%table(i_T,2)
     ic       = BS_T_grp(i_T_g)%table(i_T,3)
     i_sp_pol = BS_T_grp(i_T_g)%table(i_T,4)  ! This is i_sp_pol_c
     !
     if(.not.allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E(i_T)
     if(     allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E(i_T)-cI*BS_T_grp(i_T_g)%W(i_T)
     !
     if (.not.BS_perturbative_SOC) E_plus_W=E_plus_W0
     !
     if (     BS_perturbative_SOC) E_plus_W(:)=E_plus_W0+BS_T_grp(i_T_g)%E_SOC_corr(:,i_T)
     !
     !
     if(iq==1) then   ! EM: "iq==1":  kpt=Gamma (molecules)
       !
       DIP_projected=dot_product(field_dir,DIP_rotated(ic,iv,ik_bz,i_sp_pol,"DIP_L",Xk))
       !
     else
       !
       call error(' finite q not implemented for dichroism')
       !
     endif
     !
     BS_T_grp(i_T_g)%dipoles_R_dichroism(i_T)=conjg(DIP_projected)    ! along field_dir  (then trace...)
     !
     res=(BS_T_grp(i_T_g)%dipoles_R(i_T)/(q0_def_norm*cI))* &  
       conjg(BS_T_grp(i_T_g)%dipoles_R_dichroism(i_T))*BS_T_grp(i_T_g)%f(i_T)  ! EM:  x*L = DIP_x*DIP_orbital;  f(i_T) = occupations
     !
     if (allocated(BS_T_grp(i_T_g)%Z)) res=res*BS_T_grp(i_T_g)%Z(i_T)
     !
     do i_pert_SOC=1,n_SOC
       X_dichroism(:,3)=X_dichroism(:,3)-(cI*8*pi/3*SPEED_OF_LIGHT)*(res/(w(:)-E_plus_W(i_pert_SOC)))  ! EM: w(:) = omega;  beta_ij=-(i*hbar*me)/(omega*qe^2)*G_ij;  DeltaN=(8*pi*N*omega/3*c)*Tr(beta_ij);  N = molecular density = ??
     enddo
    !
    ! DEBUG <
    ! write(*,*) i_T,1,'R',X_dichroism(1,3)
    ! DEBUG > 
    !
     !
     if (BSS_add_antiresonant.or.BS_K_coupling) then
       !
       BS_T_grp(i_T_g)%dipoles_A_dichroism(i_T)=DIP_projected
       !
       res=(BS_T_grp(i_T_g)%dipoles_A(i_T)/(q0_def_norm*cI))*&
       conjg(BS_T_grp(i_T_g)%dipoles_A_dichroism(i_T))*BS_T_grp(i_T_g)%f(i_T)
       !
       if (allocated(BS_T_grp(i_T_g)%Z)) res=res*BS_T_grp(i_T_g)%Z(i_T)
       !
       do i_pert_SOC=1,n_SOC
         X_dichroism(:,3)=X_dichroism(:,3)+(cI*8*pi/3*SPEED_OF_LIGHT)*(res/(w(:)+conjg(E_plus_W(i_pert_SOC))))  ! EM: w(:) = omega;  beta_ij=-(i*hbar*me)/(omega*qe^2)*G_ij;  DeltaN=(8*pi*N*omega/3*c)*Tr(beta_ij);  N = molecular density = ??
       enddo
     !
     ! DEBUG <
     ! write(*,*) i_T,1,'A',X_dichroism(1,3)
     ! DEBUG >
     !
       !
     endif
     !
   enddo
   !
   if (l_rpa_IP) call live_timing(steps=1)
   !
 enddo
 !
 if (l_rpa_IP) call live_timing()
 !
 ! ALL 2 ALL
 !
 call PP_redux_wait( X_dichroism(:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait( X_dichroism(:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
 !
 X_dichroism(:,3)=X_dichroism(:,3)*Co_factor/(4._SP*pi)
 !
end subroutine
