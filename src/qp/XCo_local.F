!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use electrons,     ONLY:levels,n_spin,n_spinor,spin
 use QP_m,          ONLY:QP_Vxc,QP_n_states,QP_table
 use R_lattice,     ONLY:bz_samp
 use FFT_m,         ONLY:fft_size
 use wave_func,     ONLY:WF
 use xc_functionals,ONLY:V_xc,xc_string,XC_potential_driver,magn,XC_potential_driver
 use global_XC,     ONLY:WF_kind,WF_xc_functional,WF_exx_fraction,WF_exx_screening
 use wrapper_omp,   ONLY:Vstar_dot_V_omp
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_WF_linear 
 !
 implicit none
 !
 type(levels) ::E       
 type(bz_samp)::Xk
 !
 ! Work Space
 !
 integer     ::i1,ifft,iwf,iwfp,iwf_sp1,iwfp_sp1,iwf_sp2,iwfp_sp2,ib,ibp,ik,i_sp
 complex(SP) ::c_ws(fft_size,n_spin)
 !
 ! Spin support (collinear and not collinear)
 !
 complex(SP), allocatable :: V_xc_mat(:,:,:)
 !
 ! Allocation 
 !
 if (.not.allocated(V_xc))   allocate(V_xc(fft_size,n_spin))
 !
 if (n_spin>1.and..not.allocated(magn)) allocate(magn(fft_size,3))
 !
 ! Precompiler options in case a non-DFT potential is required
 !
   !
     call XC_potential_driver(E,Xk,WF_KIND,WF_xc_functional,1,&
&         exx_fraction=WF_exx_fraction,exx_screening=WF_exx_screening)
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
       !
       if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
       !
     !
     ! <iwf|V_xc|iwfp>
     !
     iwf =WF%index(ib,ik,1)
     iwfp=WF%index(ibp,ik,1)
     ! 
#if defined _OPENMP
!$omp parallel do default(shared), private(ifft)
#endif
     do ifft=1,fft_size
       c_ws(ifft,1)=V_xc(ifft,1)*WF%c(ifft,iwfp)
     enddo
#if defined _OPENMP
!$omp end parallel do
#endif
     !
     QP_Vxc(i1)=Vstar_dot_V_omp(fft_size,WF%c(:,iwf),c_ws(:,1))
     !
   enddo
   !
   call PP_redux_wait(QP_Vxc)
   !
   return
   !
 endif
 !
 !
 if (n_spin==2) then
   !
   allocate(V_xc_mat(fft_size,n_spin,n_spin))
   !
   call Build_V_xc_mat(V_xc,V_xc_mat)
   !
   do i1=1,QP_n_states
     !
     ib =QP_table(i1,1)
     ibp=QP_table(i1,2)
     ik =QP_table(i1,3)
     !
       if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
     !
     if (n_spinor==1) then
       !
       i_sp=spin(QP_table(i1,:))
       iwf =WF%index(ib,ik,i_sp)
       iwfp=WF%index(ibp,ik,i_sp)
       !
       c_ws(:,1)=V_xc_mat(:,i_sp,i_sp)*WF%c(:,iwfp)
       !
       QP_Vxc(i1)=Vstar_dot_V_omp(fft_size,WF%c(:,iwf),c_ws(:,1))
       !
     else
       !
       iwf_sp1=WF%index(ib,ik,1)
       iwf_sp2=WF%index(ib,ik,2)
       iwfp_sp1=WF%index(ibp,ik,1)
       iwfp_sp2=WF%index(ibp,ik,2)
       !
       c_ws(:,1)=V_xc_mat(:,1,1)*WF%c(:,iwfp_sp1)+V_xc_mat(:,1,2)*WF%c(:,iwfp_sp2)
       c_ws(:,2)=V_xc_mat(:,2,1)*WF%c(:,iwfp_sp1)+V_xc_mat(:,2,2)*WF%c(:,iwfp_sp2)
       !
       QP_Vxc(i1)=Vstar_dot_V_omp(fft_size,WF%c(:,iwf_sp1),c_ws(:,1))+&
&                 Vstar_dot_V_omp(fft_size,WF%c(:,iwf_sp2),c_ws(:,2))
       !
     endif
     !
   enddo
   !
   deallocate(V_xc_mat)
   !
 endif
 !
 ! Clean
 !
   deallocate(V_xc)
   if(n_spin>1) deallocate(magn)
 !
 call PP_redux_wait(QP_Vxc)
 !
end subroutine
