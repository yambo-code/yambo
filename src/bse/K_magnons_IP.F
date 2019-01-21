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
subroutine K_magnons_IP(iq,w,Dip)
 !
 use pars,           ONLY:SP,cZERO,cI
 use LIVE_t,         ONLY:live_timing
 use drivers,        ONLY:l_rpa_IP
 use DIPOLES,        ONLY:DIPOLE_t
 use X_m,            ONLY:X_magnons,global_gauge
 use MAGNONS,        ONLY:DIP_Splus,DIP_Smins,MAGNONS_alloc
 use BS_solvers,     ONLY:BSS_add_antiresonant,BSS_n_freqs,Co_factor
 use BS,             ONLY:BS_T_grp,BS_nT_grps,BS_K_coupling
 use electrons,      ONLY:E_SOC_nbands,n_sp_pol
 use R_lattice,      ONLY:bare_qpg
 use parallel_m,     ONLY:PAR_BS_nT_col_grps,PP_redux_wait,&
&                         PAR_IND_T_groups,PAR_Xk_bz_index,&
&                         PAR_COM_eh_INDEX,PAR_COM_Xk_ibz_INDEX
 !
#include<memory.h>
 !
 type(DIPOLE_t), intent(in) :: Dip
 integer,    intent(in) :: iq
 complex(SP),intent(in) :: w(BSS_n_freqs) 
 !
 ! Work Space
 !
 real(SP)          :: conjg_fac
 complex(SP)       :: E_plus_W0,res(2),E_plus_W(2/n_sp_pol)
 integer           :: ik_bz,iv,ic,i_sp_pol,i_sp_pol_rev,i_T,i_T_g,ik_mem,i_pert_SOC,n_SOC
 !
 YAMBO_ALLOC(X_magnons,(BSS_n_freqs,2,4))
 X_magnons=cZERO
 !
 X_magnons(:,1,1)=w(:)
 X_magnons(:,2,1)=w(:)
 !
 n_SOC=1
 i_sp_pol_rev=1
 !
 if(E_SOC_nbands/=0.and.n_sp_pol==1) n_SOC=2
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
     i_sp_pol = BS_T_grp(i_T_g)%table(i_T,4)
     !
     ik_mem= PAR_Xk_bz_index(ik_bz)
     !
     if(.not.allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E_SFLIP(i_T)
     if(     allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E_SFLIP(i_T)-cI*BS_T_grp(i_T_g)%W(i_T)
     !
     if (E_SOC_nbands==0) E_plus_W=E_plus_W0
     !
     if (E_SOC_nbands/=0) E_plus_W(:)=E_plus_W0+BS_T_grp(i_T_g)%E_SOC_corr(:,i_T)
     !
     !
     ! DIP_Splus(c,v,k) is  <v|S+|c>(i_sp_pol_v) while I need  <c|S+|v>(i_sp_pol_c) = conjg(<v|S-|c>(i_sp_pol_c_rev))
     ! DIP_Smins(c,v,k) is  <v|S-|c>(i_sp_pol_v) while I need  <c|S-|v>(i_sp_pol_c) = conjg(<v|S+|c>(i_sp_pol_c_rev))
     !
     if(i_sp_pol==1.and.n_sp_pol==2) i_sp_pol_rev=2
     if(i_sp_pol==2.and.n_sp_pol==2) i_sp_pol_rev=1
     !
     BS_T_grp(i_T_g)%dipoles_R_mag(1,i_T)=conjg(DIP_Smins(ic,iv,ik_mem,i_sp_pol_rev))  ! <c|S+|v>(i_sp_pol_c)
     BS_T_grp(i_T_g)%dipoles_R_mag(2,i_T)=conjg(DIP_Splus(ic,iv,ik_mem,i_sp_pol_rev))  ! <c|S-|v>(i_sp_pol_c)
     if (BSS_add_antiresonant.or.BS_K_coupling) then
       BS_T_grp(i_T_g)%dipoles_A_mag(1,i_T)=DIP_Splus(ic,iv,ik_mem,i_sp_pol)  ! <v|S+|c>(i_sp_pol_c)
       BS_T_grp(i_T_g)%dipoles_A_mag(2,i_T)=DIP_Smins(ic,iv,ik_mem,i_sp_pol)  ! <v|S-|c>(i_sp_pol_c)
     endif
     !
     ! Residuals: (1) <c|S+|v><v|S-|c> = dip_R_magn(1)*dip_A_magn(2)
     !            (2) <c|S-|v><v|S+|c> = dip_R_magn(2)*dip_A_magn(1)
     !
     res(1)=BS_T_grp(i_T_g)%dipoles_R_mag(1,i_T)*BS_T_grp(i_T_g)%dipoles_A_mag(2,i_T)*BS_T_grp(i_T_g)%f_SFLIP(i_T)
     res(2)=BS_T_grp(i_T_g)%dipoles_R_mag(2,i_T)*BS_T_grp(i_T_g)%dipoles_A_mag(1,i_T)*BS_T_grp(i_T_g)%f_SFLIP(i_T)
     if (allocated(BS_T_grp(i_T_g)%Z)) res=res*BS_T_grp(i_T_g)%Z(i_T)
     !
     do i_pert_SOC=1,n_SOC
       X_magnons(:,1,3)=X_magnons(:,1,3)-res(1)/(w(:)-E_plus_W(i_pert_SOC))
       X_magnons(:,2,3)=X_magnons(:,2,3)-res(2)/(w(:)-E_plus_W(i_pert_SOC))
     enddo
     !
     if (BSS_add_antiresonant.or.BS_K_coupling) then
       !
       ! Residuals: (1) <v|S+|c><c|S-|v> = dip_A_magn(1)*dip_R_magn(2)
       !            (2) <v|S-|c><c|S+|v> = dip_A_magn(2)*dip_R_magn(1)
       !
       res(1)=BS_T_grp(i_T_g)%dipoles_A_mag(1,i_T)*BS_T_grp(i_T_g)%dipoles_R_mag(2,i_T)*BS_T_grp(i_T_g)%f_SFLIP(i_T)
       res(2)=BS_T_grp(i_T_g)%dipoles_A_mag(2,i_T)*BS_T_grp(i_T_g)%dipoles_R_mag(1,i_T)*BS_T_grp(i_T_g)%f_SFLIP(i_T)
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
 ! CLEAN
 !
 if (iq==1) call MAGNONS_alloc('DIP_Splus')
 !
end subroutine
