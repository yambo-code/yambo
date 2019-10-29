!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_G_lesser(en,kpt,qpt)
 !
 use pars,           ONLY:SP,DP,schlen,cZERO,cI
 use units,          ONLY:FS2AUT,HA2EV
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 use matrix_operate, ONLY:hermitian
 use stderr,         ONLY:intc,real2ch
 use wrapper,        ONLY:M_by_M
 use com,            ONLY:msg,error
 use IO_m,           ONLY:DUMP,OP_RD_CL,OP_IF_START_RD_CL_IF_END,NONE,io_control,LOG
 use RT_control,     ONLY:TIME_string,RT_apply,SAVE_G_history,GF_RT_IO_t
 use real_time,      ONLY:RT_nk,RT_bands,RT_nbands,NE_i_time,NE_time,dG_lesser,G_lesser_reference,RT_step,&
&                         RT_dyn_step,G_MEM_steps,Ho_plus_Sigma,I1_matrix
 use YPP_real_time,  ONLY:RT_time,RT_conf,OBS_el
 use interfaces,     ONLY:WF_load,WF_free,el_density_and_current
 use wave_func,      ONLY:WF,wf_ng
 use interfaces,     ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 !
#include<memory.h>
 !
 type(levels)  :: en
 type(bz_samp) :: kpt,qpt
 type(OBS_el)  :: FT
 !
 ! Workspace
 !
 integer               :: ID,ik,ib1,ib2,io_err,i_T(2),i_w,i_T1,i_T2,i_Tc,i_dT,i_kind,j_kind
 character(schlen)     :: ch_ws(3)
 logical               :: include_eq_occ
 !
 ! Allocs
 !
 complex(SP), allocatable :: rho_T(:,:,:,:),H_rho_T(:,:,:,:),&
&                            G_lesser_k_tmtp(:,:),G_lesser_k_w(:,:),&
&                            G_ret(:,:,:,:),G_adv(:,:,:,:),&
&                            G_tmp(:,:,:)
 !
 integer, external     :: io_RT_components
 !
 call section('*','Time dependent density plot')
 !
 call parser('IncludeEQocc',include_eq_occ)
 !
 call RT_k_grid(en,kpt,qpt)
 !
 ! I/O
 !=====
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) 
 io_err=io_RT_components('G',ID)
 if(.not.SAVE_G_history) &
   &  call error(" History of rho not found. G_lesser cannot be computed")
 !
 RT_dyn_step=RT_step
 call RT_time_configuration_setup("G_lesser",GF_RT_IO_t)
 !
 ch_ws(1)="Reconstructing G_lesser(t,t') from rho"
 !
 call msg('s',trim(ch_ws(1)))
 !
 RT_nbands=RT_bands(2)-RT_bands(1)+1
 !
 YAMBO_ALLOC(G_tmp,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),2))
 G_tmp=cZERO
 !
 YAMBO_ALLOC(  rho_T,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,2))
 YAMBO_ALLOC(H_rho_T,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,2))
 rho_T=cZERO
 H_rho_T=cZERO
 !
 YAMBO_ALLOC(Ho_plus_Sigma,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,1))
 Ho_plus_Sigma=cZERO
 YAMBO_ALLOC(dG_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,G_MEM_steps))
 dG_lesser=cZERO
 !
 if(include_eq_occ) then
   YAMBO_ALLOC(G_lesser_reference,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2/),ID=ID) 
   io_err=io_RT_components('REF',ID)
 endif
 !
 YAMBO_ALLOC(G_ret,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,2))
 YAMBO_ALLOC(G_adv,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,2))
 !
 ! I need to keep k resolution to interpolate on a path
 !
 YAMBO_ALLOC(G_lesser_k_tmtp,(RT_nk,RT_conf%T_n_steps)) ! G<(k,t-t') for a given T
 YAMBO_ALLOC(G_lesser_k_w,   (RT_nk,RT_conf%W_n_steps)) ! G<(k,w)    for a given T
 G_lesser_k_tmtp=cZERO
 G_lesser_k_w   =cZERO
 !
 YAMBO_ALLOC(I1_matrix,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)))
 I1_matrix=cZERO
 do ib1=RT_bands(1),RT_bands(2)
   I1_matrix(ib1,ib1)=1._SP
 enddo
 !
 ! Parallel Setups & WF_load
 !===========================
 call PARALLEL_global_indexes(en,kpt,qpt," ",RESET=.TRUE.)
 call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 call WF_load(WF,wf_ng,1,RT_bands,(/1,kpt%nibz/),space='R',title='-WF',impose_free_and_alloc=.TRUE.)
 !
 call msg('s','G<(k,w) at time '//trim(real2ch(real((RT_conf%T_range(1)+RT_conf%T_range(2))/2._SP/FS2AUT,SP)))//' fs')
 !
 ! First step i_Tc
 !
 i_Tc=(RT_conf%T_n_steps+1)/2
 if(mod(RT_conf%T_n_steps,2)==0) call error("Number of time steps must be odd")
 !
 NE_time=RT_time(i_Tc)
 NE_i_time=nint(NE_time/RT_step)+1
 !
 ! Load rho and H[rho]
 !
#if defined _PAR_IO
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,RT_nk+1/),ID=ID)
 io_err=io_RT_components('G',ID)
#else
 do ik = 1, RT_nk 
   call io_control(ACTION=OP_IF_START_RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID)
   io_err=io_RT_components('G',ID)
 enddo
#endif
 !
 if(     include_eq_occ) rho_T(:,:,:,1)=-cI*(dG_lesser(:,:,:,1)+G_lesser_reference)
 if(.not.include_eq_occ) rho_T(:,:,:,1)=-cI* dG_lesser(:,:,:,1)
 !
 H_rho_T(:,:,:,1)=Ho_plus_sigma(:,:,:,1)
 !
 write(*,*) 'H '
 do ib1=RT_bands(1),RT_bands(2)
   write(*,*) real(Ho_plus_sigma(:,ib1,1,1))*HA2EV,aimag(Ho_plus_sigma(:,ib1,1,1))*HA2EV
 enddo
 !
 ! Build G_ret and G_adv
 !
 do i_kind=1,2
   do ik=1,RT_nk
     G_ret(:,:,ik,i_kind)=-cI*I1_matrix(:,:)
     G_adv(:,:,ik,i_kind)= cI*I1_matrix(:,:)
   enddo 
 enddo
 !
 ! Build G_lesser
 !
 do ik=1,RT_nk
   G_tmp=cZERO
   call M_by_M('n','n',RT_nbands,G_ret(:,:,ik,1),rho_T(:,:,ik,1),G_tmp(:,:,1))
   call M_by_M('n','n',RT_nbands,rho_T(:,:,ik,1),G_adv(:,:,ik,1),G_tmp(:,:,2))
   !
   do ib1=RT_bands(1),RT_bands(2)
     do ib2=RT_bands(1),RT_bands(2)
       G_lesser_k_tmtp(ik,i_Tc)=G_lesser_k_tmtp(ik,i_Tc)-G_tmp(ib1,ib2,1)+G_tmp(ib1,ib2,2)
     enddo
   enddo
 enddo
 !
 do i_T1=i_Tc+1,RT_conf%T_n_steps
   !
   i_T2=i_Tc+(i_Tc-i_T1)
   !
   i_T=(/i_T1,i_T2/)
   !
   i_dT=(i_T(1)-i_T(2))/2
   !
   ! Build G_ret and G_adv
   !
   do i_kind=1,2
     !
     NE_time=RT_time(i_T(i_kind))
     NE_i_time=nint(NE_time/RT_step)+1
     !
     ! Load rho and H[rho]
     !
#if defined _PAR_IO
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,RT_nk+1/),ID=ID)
     io_err=io_RT_components('G',ID)
#else
     do ik = 1, RT_nk 
       call io_control(ACTION=OP_IF_START_RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID)
       io_err=io_RT_components('G',ID)
     enddo
#endif
     !
     if(     include_eq_occ) rho_T(:,:,:,i_kind)=-cI*(dG_lesser(:,:,:,G_MEM_steps)+G_lesser_reference)
     if(.not.include_eq_occ) rho_T(:,:,:,i_kind)=-cI* dG_lesser(:,:,:,G_MEM_steps)
     !
     H_rho_T(:,:,:,i_kind)=Ho_plus_sigma(:,:,:,1)
     !
   enddo
   !
   ! Here I construct
   !   - G^{(r)}(t1,t2) kind 1
   !   - G^{(r)}(t2,t1) kind 2
   !
   ! e^{-ih_{HSEX}[rho(T)]dt} G^{(r)}_prev e^{-ih_{HSEX}[rho(T)]dt} with dt=RT_conf%delta_T
   !
   do ik=1,RT_nk
     call RT_multiply_eH_Gret_eH(+H_rho_T(:,:,ik,2),G_ret(:,:,ik,1),+H_rho_T(:,:,ik,1),RT_conf%delta_T/2._SP,3)  ! i_T1=i_Tc+i_dT ;  i_T2=i_Tc-i_dT
     call RT_multiply_eH_Gret_eH(-H_rho_T(:,:,ik,1),G_ret(:,:,ik,2),-H_rho_T(:,:,ik,2),RT_conf%delta_T/2._SP,3)  ! i_T1=i_Tc-i_dT ;  i_T2=i_Tc+i_dT (to check)
     !G_ret(:,:,ik,2)=(G_ret(:,:,ik,1))
   enddo
   !
   do i_kind=1,2
     !
     if(i_kind==1) j_kind=2
     if(i_kind==2) j_kind=1
     !
     do ik=1,RT_nk
       G_adv(:,:,ik,j_kind)=hermitian(G_ret(:,:,ik,i_kind))
     enddo
     !
     ! Build G_lesser
     !
     do ik=1,RT_nk
       !
       G_tmp=cZERO
       call M_by_M('n','n',RT_nbands,G_ret(:,:,ik,i_kind),rho_T(:,:,ik,j_kind),G_tmp(:,:,1))
       call M_by_M('n','n',RT_nbands,rho_T(:,:,ik,i_kind),G_adv(:,:,ik,i_kind),G_tmp(:,:,2))
       !
       do ib1=RT_bands(1),RT_bands(2)
         do ib2=RT_bands(1),RT_bands(2)
           G_lesser_k_tmtp(ik,i_T(i_kind))=G_lesser_k_tmtp(ik,i_T(i_kind))-G_tmp(ib1,ib2,1)+G_tmp(ib1,ib2,2)
         enddo
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 ! 1D FFT setup
 !--------------
 FT%title="G<(t,t')"
 call RT_1D_Fourier_setup(FT,RT_conf,RT_nk) 
 !
 do ik=1,RT_nk
   FT%F_T(:,ik)=cmplx(G_lesser_k_tmtp(ik,:),kind=DP)
 enddo
 !
 call RT_1D_Fourier_Transform('T2W',FT,RT_conf,RT_nk)
 !
 do ik=1,RT_nk
   !
   do i_dT=1,RT_conf%T_n_steps
     write(100+ik,*) RT_time(i_dT)/FS2AUT,real(FT%F_T(i_dT,ik),SP),real(aimag(FT%F_T(i_dT,ik)),SP)
   enddo
   !
   do i_w=1,RT_conf%W_n_steps
     write(200+ik,*) real(FT%W(i_w))*HA2EV,real(-cI*FT%F_W(i_w,ik),SP),real(aimag(-cI*FT%F_W(i_w,ik)),SP)
   enddo
   !
 enddo
 !
 YAMBO_FREE(rho_T)
 YAMBO_FREE(H_rho_T)
 !
 YAMBO_FREE(dG_lesser)
 YAMBO_FREE(G_lesser_reference)
 !
 YAMBO_FREE(G_tmp)
 !
 YAMBO_FREE(G_lesser_k_w)
 YAMBO_FREE(G_lesser_k_tmtp)
 !
 call WF_free(WF)
 !
end subroutine RT_G_lesser
