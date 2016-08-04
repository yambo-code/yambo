!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine RT_Observables(en,k,dG_lesser,on_the_fly_operators)
 !
 use pars,           ONLY:SP,cZERO
 use units,          ONLY:HA2EV,HA2KEL
 use drivers,        ONLY:l_sc_is_libDFT,l_elel_scatt,l_elph_scatt,l_use_Hxc_collisions,l_sc_hartree
 use QP_m,           ONLY:QP_nk
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:P_square
 use SC,             ONLY:V_hartree_sc,Ho
 use RT_control,     ONLY:RT_do_it
 use rt_interfaces,  ONLY:RT_average_operator
 use real_time,      ONLY:RT_Tfit_occupations,RT_Efit_occupations,RT_nk,RT_Nk_ibz,RT_ibz_coo,&
&                         XC0,HARTREE0,DbGd_ibz_coo,l_RT_uses_E_FineGd,REF_V_hartree_sc,   &
&                         RT_Tfit_occupations,RT_Efit_occupations,RT_E_occupations,&
&                         RT_H_occupations,G_lesser_reference,RT_Vnl_xc,RT_bands
 use electrons,      ONLY:levels,n_sp_pol
 use xc_functionals, ONLY:E_xc,XC_potential_driver
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use parallel_m,     ONLY:PAR_Xk_nibz,master_cpu
 use RT_interfaces,  ONLY:RT_average_operator
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: dG_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 complex(SP),   intent(out):: on_the_fly_operators(10)
 !
 ! WorkSpace
 !
 integer     :: ib,ik,ik_rim,iE_rim
 real(SP)    :: Energy
 complex(SP) :: G_full(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk),&
&               dE_tot,dE_kin,dE_ion,dE_h,dE_xc,dE_hxc,N_holes,N_elecs,N_err,VALUE_tmp(2),E_xc_ref,&
&               Ho_limited(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),QP_nk,n_sp_pol)
 !
 if ( .not.RT_do_it('OUT') ) return      
 !
 G_full=dG_lesser+G_lesser_reference
 !
#if defined _TIMING
 call timing('RT observables',OPR='start')
#endif
 !
 dE_tot    =cZERO
 dE_kin    =cZERO
 dE_ion    =cZERO
 dE_h      =cZERO
 dE_xc     =cZERO
 dE_hxc    =cZERO
 N_holes   =cZERO
 N_elecs   =cZERO
 N_err     =cZERO
 !
 ! Non-interaction kinetc energy
 !
 ! dE_Kin = - i Tr { P^2 dG^<}/2  
 !
 call RT_average_operator(O_VALUE=dE_kin,O_MATRIX=P_square,G=dG_lesser,en=en,k=k,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 !
 dE_kin=dE_kin/2._SP
 !  
 ! dE_ion = - i Tr {Ho dG^<} - dEkin
 !
 Ho_limited=Ho(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),:,:)
 call RT_average_operator(O_VALUE=dE_ion,O_MATRIX=Ho_limited,G=dG_lesser,en=en,k=k,Double_Grid='ENERGY',nK_mem=k%nibz)
 !
 dE_ion = dE_ion - dE_kin
 !
 if (.not.l_use_Hxc_collisions) then
   !
   if (.not.l_sc_hartree)   call RT_average_operator(O_VALUE=dE_h ,O_MATRIX=HARTREE0,G=dG_lesser,en=en,k=k,nK_mem=k%nibz)
   !
   if (.not.l_sc_is_libDFT) call RT_average_operator(O_VALUE=dE_xc,O_MATRIX=XC0     ,G=dG_lesser,en=en,k=k,nK_mem=k%nibz)
   !
   if (l_sc_hartree) then
     !
     ! Hartree potential Energy:
     !
     !  E_h[rho]=1/2 <V_h[rho]rho>= 1/2 int rho(r)rho(r')/|r-r'|
     !
     ! dE_h[rho]=E_h[rho]-E_h[rho0]
     !          =<V_h[rho0]delta_rho> + <V_h[delta_rho]delta_rho>
     !
     call RT_average_operator(O_VALUE=VALUE_tmp(1),O_REAL=REF_V_hartree_sc,G=dG_lesser,en=en,k=k,nK_mem=k%nibz)
     call RT_average_operator(O_VALUE=VALUE_tmp(2),O_REAL=    V_hartree_sc,G=dG_lesser,en=en,k=k,nK_mem=k%nibz)
     !
     dE_h=VALUE_tmp(1)+VALUE_tmp(2)/2._SP
     !
   endif
   !
   if (l_sc_is_libDFT) then
     !
     ! Exc Energy from dE_xc[rho_n] energy density ... to be fixed, E_xc_ref is wrong
     !
     call XC_potential_driver(en,k,WF_KIND,WF_xc_functional,0,&
  &                           MUTE="+report +log +io_in +io_out",unMUTE=master_cpu)
     !
     call RT_average_operator(O_VALUE=E_xc_ref,O_REAL=E_xc,G=G_lesser_reference,en=en,k=k,nK_mem=k%nibz)
     call RT_average_operator(O_VALUE=dE_xc   ,O_REAL=E_xc,G=G_full            ,en=en,k=k,nK_mem=k%nibz)
     !
     dE_xc=dE_xc-E_xc_ref
     !
   endif
   !
   dE_tot = dE_ion + dE_kin + dE_h + dE_xc
   !
 else
   !
   ! HF or SEX energy
   ! Davide: Is this correctly defined also in the SEX case 
   !         or should I use the Galitskii Migdal equation ?
   !
   call RT_average_operator(O_VALUE=dE_hxc,O_MATRIX=RT_Vnl_xc(:,:,RT_ibz_coo(:,1),:),G=dG_lesser,en=en,k=k,nK_mem=k%nibz)
   !
   !dE_hxc=dE_hxc+dE_h-dE_xc
   !
   dE_tot = dE_ion + dE_kin + dE_hxc
   !
 endif
 !
 ! Electrons and holes temperatures
 !
 if (l_elel_scatt.or.l_elph_scatt) then
   !
   call RT_occupations_FIT(RT_Tfit_occupations,RT_Efit_occupations,RT_E_occupations)
   !
   if (RT_Tfit_occupations(1)>50000/HA2KEL.or.RT_Tfit_occupations(1)<0._SP) RT_Tfit_occupations(1)=0._SP
   if (RT_Tfit_occupations(2)>50000/HA2KEL.or.RT_Tfit_occupations(2)<0._SP) RT_Tfit_occupations(2)=0._SP
   !
 endif
 !
 ! Number of Holes in the valence band and
 ! Number of Electrons in the conduction band
 ! Should be extended for metals...
 !
 do ik=1,en%nk
   do ib=RT_bands(1),RT_bands(2)
     VALUE_tmp=cZERO
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       if (l_RT_uses_E_FineGd) then
         iE_rim=ik_rim-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
         Energy=en%E_FineGd(ib,iE_rim,1)
       else
         Energy=en%E(ib,ik,1)
       endif
       if ( Energy<=0._SP ) VALUE_tmp(1)=VALUE_tmp(1)+RT_H_occupations(ib,ik_rim)
       if ( Energy> 0._SP ) VALUE_tmp(2)=VALUE_tmp(2)+RT_E_occupations(ib,ik_rim)
     enddo
     N_holes=N_holes+VALUE_tmp(1)*k%weights(ik)/real(RT_Nk_ibz(ik),SP)
     N_elecs=N_elecs+VALUE_tmp(2)*k%weights(ik)/real(RT_Nk_ibz(ik),SP)
   enddo
 enddo
 !
 N_err=N_elecs-N_holes
 !
 !
 ! Transfer the shadow variables
 !
 if(.not.l_use_Hxc_collisions) on_the_fly_operators(1:5)=(/dE_kin,dE_ion,dE_h,dE_xc,dE_tot/)*HA2EV
 if(     l_use_Hxc_collisions) on_the_fly_operators(1:5)=(/dE_kin,dE_ion,dE_hxc    ,dE_tot,dE_tot/)*HA2EV
 on_the_fly_operators(6:8)=(/N_err,N_holes,N_elecs/)
 on_the_fly_operators(9:10)=cmplx(RT_Tfit_occupations(:),0._SP)*HA2KEL
 !
#if defined _TIMING
 call timing('RT observables',OPR='stop')
#endif
 !
end subroutine RT_Observables
