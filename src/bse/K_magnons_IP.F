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
subroutine K_magnons_IP(iq,Xk,BSE_scatt,w,Dip)
 !
 use pars,           ONLY:SP,cZERO,cI,pi
 use LIVE_t,         ONLY:live_timing
 use drivers,        ONLY:l_rpa_IP
 use DIPOLES,        ONLY:DIPOLE_t,DIP_rotated
 use X_m,            ONLY:X_magnons,global_gauge
 use BS_solvers,     ONLY:BSS_add_antiresonant,BSS_n_freqs,Co_factor
 use BS,             ONLY:BS_T_grp,BS_nT_grps,BS_K_coupling,BS_perturbative_SOC
 use electrons,      ONLY:n_sp_pol
 use R_lattice,      ONLY:bz_samp,qindx_X
 use collision_el,   ONLY:elemental_collision
 use parallel_m,     ONLY:PAR_BS_nT_col_grps,PP_redux_wait,&
&                         PAR_IND_T_groups,PAR_Xk_bz_index,&
&                         PAR_COM_eh_INDEX,PAR_COM_Xk_ibz_INDEX
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
 real(SP)          :: conjg_fac
 complex(SP)       :: E_plus_W0,res(2),E_plus_W(2/n_sp_pol),DIP_expanded(3),DIP_Splus,DIP_Smins
 integer           :: ik_bz,ok_bz,ik,ok,isymm,osymm,iGo,iv,ic,i_sp_pol,i_sp_pol_rev,i_T_g,i_T,n_SOC,i_pert_SOC,io_err
 !
 n_SOC=1
 i_sp_pol_rev=1
 !
 if( .not.BS_perturbative_SOC.or. n_sp_pol==2) n_SOC=1
 if(      BS_perturbative_SOC.and.n_sp_pol==1) n_SOC=2
 !
 if (l_rpa_IP) call live_timing('IP Magnons',PAR_BS_nT_col_grps)
 !
 do i_T_g=1,BS_nT_grps
   !    
   if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
   !    
   do i_T=1,BS_T_grp(i_T_g)%size    
     !    
     ik_bz = BS_T_grp(i_T_g)%table(i_T,1)
     iv    = BS_T_grp(i_T_g)%table(i_T,2)
     ic    = BS_T_grp(i_T_g)%table(i_T,3)
     i_sp_pol = BS_T_grp(i_T_g)%table(i_T,4)  ! This is i_sp_pol_c
     !
     if(.not.allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E_SFLIP(i_T)
     if(     allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E_SFLIP(i_T)-cI*BS_T_grp(i_T_g)%W(i_T)
     !
     if (.not.BS_perturbative_SOC) E_plus_W=E_plus_W0
     !
     if (     BS_perturbative_SOC) E_plus_W(:)=E_plus_W0+BS_T_grp(i_T_g)%E_SOC_corr(:,i_T)
     !
     if(i_sp_pol==1.and.n_sp_pol==2) i_sp_pol_rev=2
     if(i_sp_pol==2.and.n_sp_pol==2) i_sp_pol_rev=1
     !
     ! notice:
     ! In optics q=0 requires a special treatment due to the non analytic behaviour
     ! This is needed for magnons.
     !
     if(iq==1) then
       !
       DIP_expanded   = DIP_rotated(ic,iv,ik_bz,i_sp_pol,"DIP_S",Xk)
       !
     else
       !
       ok_bz=qindx_X(iq,ik_bz,1)
       iGo  =qindx_X(iq,ik_bz,2)
       !
       ik   =Xk%sstar(ik_bz,1) 
       isymm=Xk%sstar(ik_bz,2) 
       !
       ok   =Xk%sstar(ok_bz,1)
       osymm=Xk%sstar(ok_bz,2)
       !
       BSE_scatt%is=(/ic,ik,isymm,i_sp_pol/)
       BSE_scatt%os=(/iv,ok,osymm,i_sp_pol_rev/)
       BSE_scatt%qs=(/iGo,iq,1/)
       !
       call scatter_Bamp_spin(BSE_scatt,'x')
       DIP_expanded(1)=-BSE_scatt%rhotw(1)
       call scatter_Bamp_spin(BSE_scatt,'y')
       DIP_expanded(2)=-BSE_scatt%rhotw(1)
       !The following is commented because it is not used at present
       !BSE_scatt%os(4)=(i_sp_pol)
       !call scatter_Bamp_spin(BSE_scatt,'z')
       !DIP_expanded(3)=-BSE_scatt%rhotw(1)
       !
     endif
     !
     DIP_Splus = 0.5_SP*(DIP_expanded(1)+cI*DIP_expanded(2))
     DIP_Smins = 0.5_SP*(DIP_expanded(1)-cI*DIP_expanded(2))
     !
     BS_T_grp(i_T_g)%dipoles_R_mag(1,i_T)=conjg(DIP_Smins)
     BS_T_grp(i_T_g)%dipoles_R_mag(2,i_T)=conjg(DIP_Splus)
     !
     ! DIP_Smins(c,v,k) is  <v|S-|c>(i_sp_pol_c) while I need  <c|S-|v>(i_sp_pol_c) = conjg(<v|S+|c>(i_sp_pol_v))
     ! DIP_Splus(c,v,k) is  <v|S+|c>(i_sp_pol_c) while I need  <c|S+|v>(i_sp_pol_c) = conjg(<v|S-|c>(i_sp_pol_v))
     !
     ! Residuals: (Smp) <v|S-|c>(i_sp_pol_c)<c|S+|v>(i_sp_pol_c) = dip_R_magn(1)*conjg(dip_R_magn(1))
     !            (Spm) <v|S+|c>(i_sp_pol_c)<c|S-|v>(i_sp_pol_c) = dip_R_magn(2)*conjg(dip_R_magn(2))
     !
     res(1)=BS_T_grp(i_T_g)%dipoles_R_mag(1,i_T)*conjg(BS_T_grp(i_T_g)%dipoles_R_mag(1,i_T))*BS_T_grp(i_T_g)%f_SFLIP(i_T)
     res(2)=BS_T_grp(i_T_g)%dipoles_R_mag(2,i_T)*conjg(BS_T_grp(i_T_g)%dipoles_R_mag(2,i_T))*BS_T_grp(i_T_g)%f_SFLIP(i_T)
     if (allocated(BS_T_grp(i_T_g)%Z)) res=res*BS_T_grp(i_T_g)%Z(i_T)
     !
     do i_pert_SOC=1,n_SOC
       X_magnons(:,1,3)=X_magnons(:,1,3)-res(1)/(w(:)-E_plus_W(i_pert_SOC))
       X_magnons(:,2,3)=X_magnons(:,2,3)-res(2)/(w(:)-E_plus_W(i_pert_SOC))
     enddo
     !
     if (BSS_add_antiresonant.or.BS_K_coupling) then
       !
       ! Residuals: (Smp) <c|S-|v>(i_sp_pol_c)<v|S+|c>(i_sp_pol_c) = dip_A_magn(1)*dip_A_magn(1)
       !            (Spm) <c|S+|v>(i_sp_pol_c)<v|S-|c>(i_sp_pol_c) = dip_A_magn(2)*dip_A_magn(2)
       !
       BS_T_grp(i_T_g)%dipoles_A_mag(1,i_T)=DIP_Splus ! Here I'm using again <c|S-|v>(i_sp_pol_c) = conjg(<v|S+|c>(i_sp_pol_v))
       BS_T_grp(i_T_g)%dipoles_A_mag(2,i_T)=DIP_Smins ! Here I'm using again <c|S+|v>(i_sp_pol_c) = conjg(<v|S-|c>(i_sp_pol_v))
       !
       ! I'm also using f_SFLIP(i_sp_pol_c)=-f_SFLIP(i_sp_pol_v)
       !                E_SFLIP(i_sp_pol_c)=-E_SFLIP(i_sp_pol_v)
       !
       ! Both -1 factors appear explicitly in the code
       !
       res(1)=BS_T_grp(i_T_g)%dipoles_A_mag(1,i_T)*conjg(BS_T_grp(i_T_g)%dipoles_A_mag(1,i_T))*BS_T_grp(i_T_g)%f_SFLIP(i_T)
       res(2)=BS_T_grp(i_T_g)%dipoles_A_mag(2,i_T)*conjg(BS_T_grp(i_T_g)%dipoles_A_mag(2,i_T))*BS_T_grp(i_T_g)%f_SFLIP(i_T)
       if (allocated(BS_T_grp(i_T_g)%Z)) res=res*BS_T_grp(i_T_g)%Z(i_T)
       !
       do i_pert_SOC=1,n_SOC
         X_magnons(:,1,3)=X_magnons(:,1,3)+res(1)/(w(:)+conjg(E_plus_W(i_pert_SOC)))
         X_magnons(:,2,3)=X_magnons(:,2,3)+res(2)/(w(:)+conjg(E_plus_W(i_pert_SOC)))
       enddo
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
 call PP_redux_wait( X_magnons(:,:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait( X_magnons(:,:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
 !
 X_magnons(:,:,3)=X_magnons(:,:,3)*Co_factor/(4._SP*pi)
 !
end subroutine
