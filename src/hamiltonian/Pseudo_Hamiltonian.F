!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine Pseudo_Hamiltonian(Xk,Xen,l_equilibrium)
 !
 use pars,          ONLY:SP,cZERO
 use drivers,       ONLY:l_sc_magnetic,l_real_time
 use electrons,     ONLY:levels,n_spinor,n_sp_pol,spin
 use wrapper,       ONLY:V_dot_V
 use wave_func,     ONLY:WF
 use FFT_m,         ONLY:fft_size
 use pseudo,        ONLY:pp_kbv_dim_yambo
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:n_atoms
 use LIVE_t,        ONLY:live_timing
 use hamiltonian,   ONLY:H_ref_bands,Hzero
 use parallel_int,  ONLY:PP_redux_wait
 use interfaces,    ONLY:WF_load,WF_free
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_RD_CL,OP_IF_START_RD_CL_IF_END,manage_action
 use timing_m,      ONLY:timing
#if defined _SC
 use parallel_m,    ONLY:PAR_IND_QP,HEAD_QP_cpu
 use QP_m,          ONLY:QP_table,QP_n_states
#endif
#if defined _RT
 use parallel_m,    ONLY:PAR_G_k_range
 use real_time,     ONLY:Ho_plus_sigma,H_pseudo_eq
#endif
 !
#include<memory.h>
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 logical       :: l_equilibrium
 !
 ! Work Space
 !
 complex(SP),allocatable :: kbv_real_space(:,:,:),kbv_ks_space(:),kbv_gauged(:,:),gauge_factor(:,:)
 complex(SP)             :: H_pseudo_ik(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2)), &
&                           H_pseudo(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2),Xk%nibz,n_sp_pol)
 integer                 :: kbv_real_space_table(pp_kbv_dim_yambo),n_par_el
 !
 ! Dummies
 integer                 :: i1,ib,ibp,ik,i_spinor,i_sp_pol,iwf,i_atom,ifrag,i_qp
 logical                 :: PAR_IND_local(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 !
 ! I / O
 integer                 :: ID,io_KB_real_space_err,ACT
 integer, external       :: io_KB_real_space
 !
#if defined _SC
 if(l_sc_magnetic) call fft_setup(0,1,.true.)  ! fft_size
#endif
 !
 YAMBO_ALLOC(kbv_real_space,(fft_size,n_spinor,pp_kbv_dim_yambo))
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),ID=ID)
 io_KB_real_space_err=io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
 !
 if(io_KB_real_space_err/=0) then
   call io_control(ACTION=OP_RD_CL,SEC=(/1/),ID=ID)
   io_KB_real_space_err=io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
   YAMBO_FREE(kbv_real_space)
   return
 endif
 !
 call PP_redux_wait()
 !
 YAMBO_ALLOC(kbv_ks_space,(H_ref_bands(1):H_ref_bands(2)))
 YAMBO_ALLOC(kbv_gauged,(fft_size,n_spinor))
 YAMBO_ALLOC(gauge_factor,(fft_size,n_atoms))
 !
 H_pseudo=cZERO
 !
#if defined _SC
 if(l_sc_magnetic) then
   !
   PAR_IND_local=.false.
   do i_qp=1,QP_n_states
     if (.not.HEAD_QP_cpu)                 cycle
     if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
     ib =QP_table(i_qp,1)
     ibp=QP_table(i_qp,2)
     PAR_IND_local(ib,ibp)=.TRUE.
   enddo 
   n_par_el=n_sp_pol*Xk%nibz*(count(PAR_IND_LOCAL))
   if (n_par_el>0) call live_timing('MAG-pseudo',n_par_el)
   !      
 endif
#endif
 !
 PAR_IND_local=.true.
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz
     !
     ifrag=ik+(i_sp_pol-1)*Xk%nibz
     !
#if defined _RT
     if(l_real_time) call timing('Pseudo kbv I/O',OPR='start')
#endif
     !
     ACT=manage_action(OP_IF_START_RD_CL_IF_END,ifrag,1,Xk%nibz*n_sp_pol)
     call io_control(ACTION=ACT,SEC=(/ifrag+1/),ID=ID)
     io_KB_real_space_err=io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
     !
#if defined _SC
     if(l_sc_magnetic) then
       !
       PAR_IND_local=.false.
       do i_qp=1,QP_n_states
         if (.not.HEAD_QP_cpu)                 cycle
         if (     QP_table(i_qp,3) /=ik      ) cycle
         if (spin(QP_table(i_qp,:))/=i_sp_pol) cycle
         if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
         ib =QP_table(i_qp,1)
         ibp=QP_table(i_qp,2)
         PAR_IND_local(ib,ibp)=.TRUE.
         PAR_IND_local(ibp,ib)=.TRUE.
       enddo 
       !
       call WF_load(WF,0,1,(/H_ref_bands(1),H_ref_bands(2)/),(/ik,ik/),sp_pol_to_load=(/i_sp_pol,i_sp_pol/),&
       &  title='-Pseudo',impose_free_and_alloc=.TRUE.,keep_states_to_load=.true.)
       !
     endif
#endif
     !
#if defined _RT
     if(l_real_time) then
       call timing('Pseudo kbv I/O',OPR='stop')
       call timing('Pseudo correction',OPR='start')
     endif
#endif
     !
     if(.not.l_equilibrium) call Pseudo_KB_gauge_factor(gauge_factor)
     !
     do i1=1,pp_kbv_dim_yambo
       !
       kbv_ks_space=cZERO
       !
       i_atom=kbv_real_space_table(i1)
       !
       do i_spinor=1,n_spinor
         if(.not.l_equilibrium) kbv_gauged(:,i_spinor)=kbv_real_space(:,i_spinor,i1)*gauge_factor(:,i_atom)
         if(     l_equilibrium) kbv_gauged(:,i_spinor)=kbv_real_space(:,i_spinor,i1)
       enddo
       !
       H_pseudo_ik=cZERO
       !
       do ib=H_ref_bands(1),H_ref_bands(2)
         !
         iwf=WF%index(ib,ik,i_sp_pol)
         !
         if (iwf==0) cycle
         !
         kbv_ks_space(ib)=V_dot_V(fft_size*n_spinor,WF%c(:,:,iwf),kbv_gauged)
         !
         do ibp=H_ref_bands(1),ib
           !
           if (.not.PAR_ind_local(ib,ibp) ) cycle
           !
           H_pseudo_ik(ibp,ib)=conjg(kbv_ks_space(ibp))*kbv_ks_space(ib)
           if(ib/=ibp) H_pseudo_ik(ib,ibp)=conjg(H_pseudo_ik(ibp,ib))
           !
         enddo   ! ibp
       enddo   ! ib
       !
       H_pseudo(:,:,ik,i_sp_pol)=H_pseudo(:,:,ik,i_sp_pol)+H_pseudo_ik(:,:)
       !
     enddo   ! i1
     !
#if defined _RT
     if(l_real_time) call timing('Pseudo correction',OPR='stop')
#endif
     !
#if defined _SC
   if(l_sc_magnetic) then
     call live_timing(steps=1)
     call WF_free(WF)
   endif
#endif
     !
   enddo   ! ik
   !
 enddo  ! i_sp_pol
 !
#if defined _SC
 if(l_sc_magnetic) then
   if (n_par_el>0) call live_timing()
   if (.not.l_equilibrium) Hzero=Hzero+H_pseudo
   if (     l_equilibrium) Hzero=Hzero-H_pseudo
 endif
#endif
 !
#if defined _RT
 if(l_real_time) then
   if (l_equilibrium) then
     H_pseudo_eq  =H_pseudo
   else
     Ho_plus_sigma=Ho_plus_sigma+(H_pseudo(:,:,PAR_G_k_range(1):PAR_G_k_range(2),1:1)-&
&                                 H_pseudo_eq(:,:,PAR_G_k_range(1):PAR_G_k_range(2),1:1))
   endif
 endif
#endif
 !
 ! Free mem
 YAMBO_FREE(kbv_ks_space)
 YAMBO_FREE(kbv_real_space)
 YAMBO_FREE(kbv_gauged)
 YAMBO_FREE(gauge_factor)
 !
end subroutine Pseudo_Hamiltonian
