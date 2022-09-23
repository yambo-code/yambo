!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
logical function K_Transition_check(Ken,Dip,iq,ik,ikp,iv,ic,i_sp_c,i_res_ares,&
&                                   E_eh,E_eh_SOC_corr,f_eh,f_eh_RES,f_eh_PL)
 !
 use pars,         ONLY:SP 
 use X_m,          ONLY:global_gauge
 use DIPOLES,      ONLY:DIPOLE_t
 use electrons,    ONLY:levels,n_sp_pol,filled_tresh,spin_occ
 use BS,           ONLY:BS_not_const_eh_f,BS_perturbative_SOC,BS_eh_en,l_BS_magnons
#if defined _RT
 use drivers,      ONLY:l_rt_carriers_in_use
 use RT_control,   ONLY:NEQ_Residuals,NEQ_Kernel,EQ_Transitions,EQ_NoOcc
#endif
 !
 implicit none
 !
 type(levels),   intent(in) :: Ken
 type(DIPOLE_t), intent(in) :: Dip
 integer,        intent(in) :: iq,ik,ikp,iv,ic,i_sp_c,i_res_ares
 real(SP),       intent(out):: E_eh,E_eh_SOC_corr(2/n_sp_pol),f_eh,f_eh_RES,f_eh_PL
 !
 ! Work Space
 !
 integer :: i_sp_v,iv_SOC(2/n_sp_pol),ic_SOC(2/n_sp_pol),pert_range(2/n_sp_pol,2)
 real(SP):: Eo_eh,E_v_SOC(2/n_sp_pol),E_c_SOC(2/n_sp_pol),f_eh_REF,f_e,f_h,f_check
 !
 K_Transition_check=.TRUE.
 !
 i_sp_v=i_sp_c
 if(l_BS_magnons) i_sp_v=mod(i_sp_c,n_sp_pol)+1
 !
 E_eh= Ken%E(ic,ik ,i_sp_c)-Ken%E(iv,ikp,i_sp_v)
 if (     allocated(Ken%Eo)) Eo_eh= Ken%Eo(ic,ik ,i_sp_c)-Ken%Eo(iv,ikp,i_sp_v)
 if (.not.allocated(Ken%Eo)) Eo_eh= E_eh
 !
 ! Small energy transitions. Dipoles in length gauge are not defined
 if ((.not.l_BS_magnons).and.iq==1) then
   if ( abs(Eo_eh) < Dip%Energy_treshold .and. trim(global_gauge)=="length") K_Transition_check=.FALSE.
 endif
 ! Null energy transitions are always removed
 if ( abs(E_eh) == 0._SP )  K_Transition_check=.FALSE.
 !
 if( BS_perturbative_SOC ) then
   if(n_sp_pol==1) pert_range=reshape((/1,2,1,2/),(/2,2/))
   if(n_sp_pol==2) pert_range=reshape((/i_sp_c,1 /),(/1,2/))
   iv_SOC(pert_range(:,2))=Ken%table_SOC(pert_range(:,1),iv,ikp)
   ic_SOC(pert_range(:,2))=Ken%table_SOC(pert_range(:,1),ic,ik )
   if(     all(iv_SOC/=0)) E_v_SOC(:)=Ken%E_SOC(iv_SOC(:),ikp)
   if(     all(ic_SOC/=0)) E_c_SOC(:)=Ken%E_SOC(ic_SOC(:),ik )
   if(.not.all(iv_SOC/=0)) E_v_SOC(:)=Ken%E(iv,ikp,i_sp_v)
   if(.not.all(ic_SOC/=0)) E_c_SOC(:)=Ken%E(ic,ik ,i_sp_c)
   E_eh_SOC_corr(:)=E_c_SOC(:)-E_v_SOC(:)-Eo_eh
 endif
 !
 ! Occupations Factors
 !=====================
 !
 f_eh_REF =(Ken%f(iv,ikp,i_sp_v)-Ken%f(ic,ik ,i_sp_c))/spin_occ
 f_eh     =f_eh_REF
 f_eh_RES =f_eh_REF
 f_check  =f_eh_REF
 !
 ! f_eh  enter the excitonic Hamiltonian
 ! f_eh_RES enter the residual of the dielectric constant
 !
#if defined _RT
 if (l_rt_carriers_in_use.and.allocated(Ken%fo)) then
   !
   if (     EQ_Transitions) f_eh_REF=(Ken%fo(iv,ikp,i_sp_v)-Ken%fo(ic,ik ,i_sp_c))/spin_occ
   if (.not.EQ_Transitions) f_eh_REF=(Ken%f (iv,ikp,i_sp_v)-Ken%f (ic,ik ,i_sp_c))/spin_occ
   if (     NEQ_Kernel    ) f_eh =(Ken%f (iv,ikp,i_sp_v)-Ken%f (ic,ik ,i_sp_c))/spin_occ
   if (.not.NEQ_Kernel    ) f_eh =(Ken%fo(iv,ikp,i_sp_v)-Ken%fo(ic,ik ,i_sp_c))/spin_occ
   if (     NEQ_Residuals ) f_eh_RES=(Ken%f (iv,ikp,i_sp_v)-Ken%f (ic,ik ,i_sp_c))/spin_occ
   if (.not.NEQ_Residuals ) f_eh_RES=(Ken%fo(iv,ikp,i_sp_v)-Ken%fo(ic,ik ,i_sp_c))/spin_occ
 endif
#endif
 !
 if(i_res_ares==1) then
   f_e = (         Ken%f(iv,ikp,i_sp_v))/spin_occ
   f_h = (spin_occ-Ken%f(ic,ik ,i_sp_c))/spin_occ
 else
   f_e = (         Ken%f(ic,ik ,i_sp_c))/spin_occ
   f_h = (spin_occ-Ken%f(iv,ikp,i_sp_v))/spin_occ
 endif
 !
 f_eh_PL = Ken%f(ic,ikp,i_sp_c)*(spin_occ-Ken%f(iv,ik ,i_sp_v))/spin_occ/spin_occ
 !
#if defined _RT
 if(EQ_NoOcc) then
   f_eh_REF=1._SP
   f_eh    =1._SP
   f_eh_RES=1._SP
   f_check =1._SP
   f_e     =1._SP
   f_h     =1._SP
 endif
#endif
 !
 ! A 0.001 % fractional occupation swiches on the particular treatment of finite temp. BSE 
 if ( abs(abs(f_eh_REF)-1._SP)> filled_tresh ) BS_not_const_eh_f=.TRUE.
 !
 ! f_eh<0 transitions are included in the anti-resonant part
 if ( f_eh_REF*(-1._SP)**(i_res_ares-1)< 0._SP ) K_Transition_check=.FALSE.
 !
 !   f_eh=0 transitions give no contribution to absorption at the IP level
 ! and they do not mix with others neither since the kernel is weighted by f_eh
 !   However they contribute to the JDOS. We need to take only the E>0 component,
 ! since the E<0 component is then accounted for by the anti-resonant part
 if ( f_eh_REF==0._SP .and. E_eh*(-1._SP)**(i_res_ares-1)<0._SP ) K_Transition_check=.FALSE.
 !
#if defined _RT
 !
 ! In case of population inversion, it may be problematic if f_eh>0. and f_eh_RES<0.
 if ( f_eh_REF*f_eh<0._SP .or. f_eh_REF*f_eh_RES<0._SP ) then
    call error(' I cannot split the R and K BSE contribution.'//&
&              ' ACTION: Use the same occupations in K and R.')
 endif
 !
#endif
 !
#if defined _RT
 if (.not.l_rt_carriers_in_use) then
#endif
   !
   ! If I've included in valence states that are almost empty
   ! or in conduction states that are almost full, those are neglected
   !
   if ( (f_e<=filled_tresh) .or. (f_h<=filled_tresh) ) K_Transition_check=.FALSE.
   !
#if defined _RT
 endif
#endif
 !
 if (any((/abs(E_eh)<BS_eh_en(1).and.BS_eh_en(1)>0._SP,&
&          abs(E_eh)>BS_eh_en(2).and.BS_eh_en(2)>0._SP/))) K_Transition_check=.FALSE.
 !
end function
