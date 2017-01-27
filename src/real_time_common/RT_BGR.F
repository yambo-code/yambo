!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_BGR(dG,E,k)
 !
 ! This routine calculates the BGR
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use drivers,        ONLY:l_sc_fock,l_sc_sex
 use hamiltonian,    ONLY:V_hartree_sc,V_xc_sc,rho_n
 use R_lattice,      ONLY:nkibz,bz_samp
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 use electrons,      ONLY:levels,n_sp_pol,E_duplicate,E_reset,spin
 use parallel_m,     ONLY:master_cpu
 use QP_m,           ONLY:QP_table,QP_n_states
 use wave_func,      ONLY:WF
 use real_time,      ONLY:l_initial_equilibrium,REF_V_xc_sc,BGR,&
&                         REF_V_hartree_sc,REF_Vnl_xc,BGR_Hartree,BGR_V_xc,BGR_Self_Energy,&
&                         l_RT_uses_E_DbGd,DbGd_ibz_coo,E_reference,RT_nk,RT_ibz_coo,&
&                         eval_HARTREE,eval_DFT,RT_ibz_coo,RT_all2ibz,l_BGR,RT_Vnl_xc,&
&                         RT_bands
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 use parallel_m,     ONLY:PP_redux_wait
 use interfaces,     ONLY:OCCUPATIONS_Gaps
 !
 implicit none
 !
 type(levels),  intent(inout) :: E
 type(bz_samp), intent(in)    :: k
 complex(SP),   intent(in)    :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Space
 !
 complex(SP)  :: H(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),n_sp_pol)
 real(SP)     :: deltaE
 integer      :: ib,ik,ik_RT,i_qp,ibp,I_dir(2),ik_ibz,ik_DbGd,i_sp
 !
 if(trim(RT_potential)=='IP'.or..not.eval_BGR) return
 !
 BGR_Self_Energy=rZERO
 BGR_Hartree=rZERO
 BGR_V_xc=rZERO
 !
#if defined _TIMING
 call timing('RT BGR',OPR='start')
#endif
 !
 if (.not.l_use_Hxc_collisions) then
   !
   do ik=1,nkibz
     !
     ! Hartree contribution to BGR
     !
     if (eval_HARTREE) then
       H=cZERO
       call V_real_space_to_H(ik,1,V_hartree_sc,H,WF,'def')
       do ib=RT_bands(1),RT_bands(2)
         BGR_Hartree(ib,ik,1)=real(H(ib,ib,1),SP)
       enddo
     endif
     !
     ! XC contribution to BGR
     !
     if (eval_DFT) then
       H=cZERO
       call V_real_space_to_H(ik,1,V_xc_sc,H,WF,'xc ')
       do ib=RT_bands(1),RT_bands(2)
         BGR_V_xc(ib,ik,1)=real(H(ib,ib,1),SP)
       enddo
     endif
     !
   enddo
   !
   ! All to All
   !
   call PP_redux_wait(BGR_Hartree)
   call PP_redux_wait(BGR_V_xc)
   !
 endif
 !
 if (l_use_Hxc_collisions) then
   !
   call COLLISIONS_compose_rt(dG)

   do ik=1,nkibz
     do ik_RT =RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       do ib=RT_bands(1),RT_bands(2)
         BGR_Self_Energy(ib,ik,1)=real(RT_Vnl_xc(ib,ib,ik_RT,i_sp),SP)
       enddo
     enddo
   enddo
   !
 endif
 !
 if (.not.l_initial_equilibrium) then
   !
   if (l_BGR) then
     !
     ! Corrections of the Energy Levels
     !
     do ik=1,RT_nk
       !
       ik_ibz=RT_all2ibz(ik)
       ik_DbGd=ik-RT_ibz_coo(ik_ibz,1)+DbGd_ibz_coo(ik_ibz,1)
       !
       do ib=RT_bands(1),RT_bands(2)
         !
         deltaE=BGR_Hartree(ib,ik_ibz,1)+BGR_V_xc(ib,ik_ibz,1)+BGR_Self_Energy(ib,ik_ibz,1)
         !
         if (     l_RT_uses_E_DbGd) E%E_DbGd(ib,ik_DbGd,1)=E_reference%E_DbGd(ib,ik_DbGd,1)+deltaE
         if (.not.l_RT_uses_E_DbGd) E%E(ib,ik_ibz,1)      =E_reference%E(ib,ik_ibz,1)      +deltaE
         !
       enddo
       !
     enddo
     !
   endif
   !
   ! Evaluation of the contribution to the minimal gap
   !
   call OCCUPATIONS_Gaps(E,I_dir=I_dir)
   !
   ib=I_dir(1)
   ik=I_dir(2)
   !
   BGR(1)=E%E_dir_gap(1)
   BGR(2)=E%E_ind_gap(1)
   BGR(3)=    BGR_Hartree(ib+1,ik,1)-    BGR_Hartree(ib,ik,1)
   BGR(4)=       BGR_V_xc(ib+1,ik,1)-       BGR_V_xc(ib,ik,1)
   BGR(5)=BGR_Self_Energy(ib+1,ik,1)-BGR_Self_Energy(ib,ik,1)
   !
 endif
 !
#if defined _TIMING
 call timing('RT BGR',OPR='stop')
#endif
 !
end subroutine RT_BGR
