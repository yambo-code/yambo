!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_Hamiltonian(E,k,q,FL_V_bands,w0)
 !
 ! Build electron-field operator
 ! call FL_Build_W_operator
 !
 ! Add diagonal part
 !                                        
 use pars,           ONLY:SP,cZERO,pi,cONE
 use units,          ONLY:SPEED_OF_LIGHT
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use zeros,          ONLY:zero_dfl
 use drivers,        ONLY:l_use_Hxc_collisions
 use hamiltonian,    ONLY:rho_n,H_nl_sc,V_hartree_sc,V_xc_sc
 use QP_m,           ONLY:QP_nk
 use X_m,            ONLY:global_gauge,X_t
 use fields,         ONLY:A_ext,A_tot,Efield
 use real_time,      ONLY:eval_DFT,eval_HARTREE,l_RT_induced_Field,rho_reference,&
&                         Ho_plus_Sigma,Phase_LifeTime,RT_Vnl_xc,RT_nbands         
 use wave_func,      ONLY:WF
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use nl_optics,      ONLY:Correlation,V_xc_0,full_rho,IPA,E_full,LRC,JGM,LRCW,l_use_DIPOLES,NL_LRC_alpha,   &
&                         NL_initial_P,E_tot,E_ext,E_ks,E_xc_0,I_relax,Alpha_ED,dG,eval_dG, & 
&                         NL_bands,VAL_BANDS,NL_P
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,FL_states,FL_Ho_plus_Sigma
 use global_XC,      ONLY:WF_xc_functional,WF_kind
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_Xk_ibz_index
 use parallel_int,   ONLY:PP_redux_wait
 use wrapper_omp,    ONLY:M_plus_alpha_M_omp
 use collision_ext,  ONLY:COLLISIONS_have_HARTREE
 use H_interfaces,   ONLY:V_real_space_to_H
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 complex(SP),   intent(in)    :: FL_V_bands(NL_bands(2),n_tot_fl_modes,E%nbf,QP_nk,n_sp_pol)
 real(SP),      intent(in)    :: w0
 !
 ! Working Space
 !
 integer     :: ik_bz,i_sp_pol,is,i1,ik_mem
 !aim137
 integer     :: i_fl,i_fks
 complex(SP) :: E_vec_pot(3),E_xc_pot(3)
 real :: alpha_
 character(4) :: filename
 !
#if defined _TIMING
 call timing('FL Hamiltonian',OPR='start')
#endif
 !
 ! aim137: deleted beyond-IPA stuff
 !
 ! Evaluate the Polarization 
 ! ===================================================================
 !
 ! aim137: deleted call to Build_berry_polarization_NEQ
 ! aim137: Sm1_plus,Sm1_minus and All_S_det matrices were updated here as well
 !
 ! Evaluate Floquet Overlaps
 ! ===================================================================
 !
 FL_S_minus=cZERO
 FL_S_plus =cZERO
 !
 do ik_bz=1,QP_nk
   !
   !if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
   !
   do is=1,n_sp_pol
     !
     call FL_overlaps(E,k,is,ik_bz,FL_V_bands,FL_S_plus(:,:,:,:,ik_bz,is),FL_S_minus(:,:,:,:,ik_bz,is)) 
     !
   enddo
   !
 enddo
 !
 ! Evaluate the external/total field
 !=================================
 !
 !call RT_Ext_fields(A_ext,Time)
 !
 A_tot=A_ext
 !
 E_ext=-A_ext%vecpot_vel/SPEED_OF_LIGHT
 !
 E_tot=E_ext
 E_ks =E_ext
 !
 ! aim137: deleted more beyond-IPA stuff
 ! aim137: deleted G_lesser
 !
 ! Build the Hamiltonian
 ! =====================
 !
 Ho_plus_Sigma=cZERO
 I_relax      =cZERO
 !
 do i_sp_pol=1,n_sp_pol
  do ik_bz=1,QP_nk
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik_bz)) cycle
   !
   ik_mem=PAR_Xk_ibz_index(ik_bz)
   !
   ! Here I fill H_nl_sc with...
   !
   H_nl_sc=cZERO
   !
   ! Correlation part Hartree + TDDFT
   ! ================================
   !
   ! aim137: deleted more beyond-IPA stuff
   !
   ! Add Ho = T+V_ion+V_h+V_xc
   !==========================
   !
   ! aim137: omp features commented out for now.
! $omp parallel do default(shared), private(i1)
   do i1=NL_bands(1),NL_bands(2) 
     do i_fl=1,n_tot_fl_modes
       i_fks = i_fl + (i1-1)*n_tot_fl_modes
       !
       H_nl_sc(i_fks,i_fks) = &
&              H_nl_sc(i_fks,i_fks) & 
&            + E_full(i1,ik_bz,i_sp_pol) &
&            - real(i_fl,SP) * w0  !aim137: assuming units of w0 and E_full are compatible. check.
       !
     enddo
   enddo
! $omp end parallel do
   !
   if(l_use_DIPOLES.or.trim(global_gauge)=='velocity') then
     !    
     !aim137 no se que es esto
     !call RT_apply_field(ik_bz,i_sp_pol,H_nl_sc,A_tot)
     !
   else
     !
     !aim137 commented out for now
     call FL_build_W_operator(FL_V_bands,E,k,A_tot,ik_bz,i_sp_pol,H_nl_sc)
     !
   endif                                                         
   !
   ! aim137 deleted more beyond-IPA stuff
   ! aim137 deleted damping related stuff
   !
   ! Store the Hamiltonian
   ! =====================
!aim137: omp features disabled for now
! $omp parallel do default(shared), private(i1)
   do i1=NL_bands(1),NL_bands(2) 
     do i_fl=1,n_tot_fl_modes
       i_fks = i_fl + (i1-1)*n_tot_fl_modes
       FL_Ho_plus_Sigma(:,i_fks,ik_mem,i_sp_pol)=H_nl_sc(:,i_fks)
     enddo
   enddo
! $omp end parallel do
   !
  enddo  !---- loop on k-point
 enddo
 !
 if(Phase_LifeTime<zero_dfl) call PP_redux_wait(I_relax,COMM=PAR_COM_Xk_ibz_INDEX%COMM ) 
 !
#if defined _TIMING
 call timing('FL Hamiltonian',OPR='stop')
#endif
 !
  !aim137 DEBUG
do ik_bz=1,QP_nk
write(filename,'(i0)') ik_bz
open(111,file='FL_Ho_plus_Sigma'//trim(adjustl(filename))//'.dat')
do i_fks=FL_states(1),FL_states(2)
write(111,*) FL_Ho_plus_Sigma(i_fks,:,ik_bz,1)
enddo
close(111)
enddo
 !aim137 END DEBUG
end subroutine FL_Hamiltonian
