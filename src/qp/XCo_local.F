!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine XCo_local(E,Xk)
 !
 ! Local V_xc
 !
 use pars,          ONLY:SP,pi
 use com,           ONLY:error
 use electrons,     ONLY:levels,n_spin,n_spinor,spin
 use QP_m,          ONLY:QP_Vxc,QP_n_states,QP_table
 use R_lattice,     ONLY:bz_samp
 use FFT_m,         ONLY:fft_size
 use wave_func,     ONLY:WF
 use xc_functionals,ONLY:E_xc,V_xc,xc_string,XC_potential_driver,magn,XC_potential_driver
 use global_XC,     ONLY:WF_kind,WF_xc_functional,WF_exx_fraction,WF_exx_screening
 use wrapper_omp,   ONLY:Vstar_dot_V_omp
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_WF_linear 
 use pseudo,        ONLY:pp_is_uspp
#if defined _RT || defined _SC
 use drivers,       ONLY:l_real_time,l_sc_run
 use hamiltonian,   ONLY:H_ref_bands
#endif
#if defined _RT
 use drivers,       ONLY:l_real_time
 use parallel_m,    ONLY:PAR_IND_B_mat_ordered,PAR_IND_WF_k 
 use matrix_operate,ONLY:UP_matrix_index,matrix_index
#endif
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 type(levels) ::E       
 type(bz_samp)::Xk
 !
 ! Work Space
 !
#if defined _RT
 integer     ::nb
#endif
 integer     ::i1,ifft,iwf,iwfp,ib,ibp,ik,i_sp_pol
 real(SP)    ::E_xc_
 complex(SP) ::c_ws(fft_size,n_spinor)
 !
 ! Spin support (collinear and not collinear)
 !
 complex(SP), allocatable :: V_xc_mat(:,:,:)

 call timing('XCo_local',OPR="start")
 !
 ! Allocation 
 !
 if (.not.allocated(V_xc)) then
   YAMBO_ALLOC(V_xc,(fft_size,n_spin))
 endif
 if (.not.allocated(E_xc)) then
   YAMBO_ALLOC(E_xc,(fft_size))
 endif
 if (.not.allocated(magn)) then
   YAMBO_ALLOC(magn,(fft_size,3))
 endif
 !
 ! Precompiler options in case a non-DFT potential is required
 !
   !
#if defined _RT
   if (l_real_time) then
     call XC_potential_driver(E,Xk,WF_KIND,WF_xc_functional,1)
   else
#endif
     ! evaluate E_xc as a double check against DFT
     call XC_potential_driver(E,Xk,WF_KIND,WF_xc_functional,0,&
&         exx_fraction=WF_exx_fraction,exx_screening=WF_exx_screening)
     ! evaluate v_xc
     call XC_potential_driver(E,Xk,WF_KIND,WF_xc_functional,1,&
&                             exx_fraction=WF_exx_fraction,exx_screening=WF_exx_screening)
#if defined _RT
   endif
#endif
   !
 !
 if (n_spin==1) then
   ! 
   do i1=1,QP_n_states
     !
     ib =QP_table(i1,1)
     ibp=QP_table(i1,2)
     ik =QP_table(i1,3)
     !
#if defined _RT
     if (l_real_time) then
       !
       if(ib>ibp) then
         QP_Vxc(i1) = conjg(QP_Vxc(i1-1))
         cycle
       endif
       !
       if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
       if (.not.PAR_IND_B_mat_ordered%element_1D( UP_matrix_index(ib-H_ref_bands(1)+1,ibp-H_ref_bands(1)+1)-1 )) cycle
       !
     else
#endif
       !
       if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
       !
#if defined _RT
     endif
#endif
     !
     ! <iwf|V_xc|iwfp>
     !
     iwf =WF%index(ib,ik,1)
     iwfp=WF%index(ibp,ik,1)
     ! 
!$omp parallel do default(shared), private(ifft)
     do ifft=1,fft_size
       c_ws(ifft,1)=V_xc(ifft,1)*WF%c(ifft,1,iwfp)
     enddo
!$omp end parallel do
     !
     QP_Vxc(i1)=Vstar_dot_V_omp(fft_size*n_spinor,WF%c(:,:,iwf),c_ws)
     !
     if (pp_is_uspp) then
       call PP_vloc_augment(QP_Vxc(i1),V_xc,ik,ib,ibp)
     endif
     !
   enddo
   !
   call PP_redux_wait(QP_Vxc)
   !
   call LOCAL_clean( )
   !
   call timing('XCo_local',OPR="stop")
   return
   !
 endif
 !
#if defined _RT
 nb=H_ref_bands(2)-H_ref_bands(1)+1
#endif
 !
 if (n_spin==2) then
   !
   YAMBO_ALLOC(V_xc_mat,(fft_size,n_spin,n_spin))
   !
   call Build_V_xc_mat(V_xc,V_xc_mat)
   !
   do i1=1,QP_n_states
     !
     ib =QP_table(i1,1)
     ibp=QP_table(i1,2)
     ik =QP_table(i1,3)
     i_sp_pol=spin(QP_table(i1,:))
     !
#if defined _RT
     if (l_real_time) then
       if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
       if (.not.PAR_IND_B_mat_ordered%element_1D( UP_matrix_index(ib-H_ref_bands(1)+1,ibp-H_ref_bands(1)+1)-1 )) cycle
     else
#endif
       if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
#if defined _RT
     endif
#endif
     !
     if (n_spinor==1) then
       !
       iwf =WF%index(ib,ik,i_sp_pol)
       iwfp=WF%index(ibp,ik,i_sp_pol)
       !
       c_ws(:,1)=V_xc_mat(:,i_sp_pol,i_sp_pol)*WF%c(:,1,iwfp)
       !
       QP_Vxc(i1)=Vstar_dot_V_omp(fft_size*n_spinor,WF%c(:,:,iwf),c_ws)
       !
       if (pp_is_uspp) then
         call error('[PPs] USPP: nspin=2 and n_spinor=1 not implemented')
       endif
       !
     else
       !
       iwf=WF%index(ib,ik,1)
       iwfp=WF%index(ibp,ik,1)
       !
       c_ws(:,1)=V_xc_mat(:,1,1)*WF%c(:,1,iwfp)+V_xc_mat(:,1,2)*WF%c(:,2,iwfp)
       c_ws(:,2)=V_xc_mat(:,2,1)*WF%c(:,2,iwfp)+V_xc_mat(:,2,2)*WF%c(:,2,iwfp)
       !
       QP_Vxc(i1)=Vstar_dot_V_omp(fft_size*n_spinor,WF%c(:,:,iwf),c_ws)
       !
       if (pp_is_uspp) then
         call error('[PPs] USPP: nspin=2 and n_spinor>1 not implemented')
       endif
       !
     endif
     !
   enddo
   !
   YAMBO_FREE(V_xc_mat)
   !
   call LOCAL_clean( )
   !
   call PP_redux_wait(QP_Vxc)
   !
 endif
 !
 call timing('XCo_local',OPR="stop")
 return
 !
 contains
   !
   subroutine LOCAL_clean()
     !
#if defined _SC || defined _RT
     if (.not.(l_sc_run.or.l_real_time)) then
#endif
       YAMBO_FREE(V_xc)
       YAMBO_FREE(E_xc)
       YAMBO_FREE(magn)
#if defined _SC || defined _RT
     endif
#endif
   end subroutine
   !
end subroutine
