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
subroutine RT_G_two_times_build(en,kpt,qpt)
 !
 use pars,           ONLY:SP,DP,cZERO,cI,cONE,cZERO,schlen
 use units,          ONLY:FS2AUT,HA2EV
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 use matrix_operate, ONLY:hermitian
 use stderr,         ONLY:intc,real2ch
 use wrapper,        ONLY:M_by_M
 use com,            ONLY:msg,of_open_close
 use IO_m,           ONLY:DUMP,OP_RD_CL,OP_IF_START_RD_CL_IF_END,NONE,io_control,LOG
 use RT_control,     ONLY:TIME_string,RT_apply,SAVE_G_history,GF_RT_IO_t
 use real_time,      ONLY:RT_nk,RT_bands,RT_nbands,NE_i_time,NE_time,dG_lesser,G_lesser_reference,RT_step,&
&                         RT_dyn_step,G_MEM_steps,Ho_plus_Sigma,I1_matrix,RT_all2ibz
 use YPP_real_time,  ONLY:RT_time,RT_conf,GreenF_T_and_W
 use LIVE_t,         ONLY:live_timing
 !
#include<memory.h>
 !
 type(levels),  intent(in)    :: en
 type(bz_samp), intent(in)    :: kpt,qpt
 !
 ! Workspace
 !
 real(SP)              :: T_c,REF_diff,TIME_diff
 integer               :: ID,ik,ik_RT,ib1,ib2,io_err,i_T(2),i_w,i_T1,i_T2,i_Tc,i_dT,i_kind,i_t_tmp,Nt
 character(2)          :: function_name
 logical               :: include_eq_occ,keep_cc,keep_vv,keep_cv,keep_vc,&
&                         build_G_ret,build_G_adv,build_G_les,build_G_grt,build_Spect
 character(schlen)     :: headings(3),file_name
 real(SP)              :: odata(3)
 !
 ! Allocs
 !
 complex(SP), allocatable :: rho_T(:,:,:),b_rho_T(:,:,:),H_rho_T(:,:,:,:),&
&                            F_k_tmtp(:,:),G_ret(:,:,:),G_tmp(:,:),TMP_M(:,:,:),&
&                            G_w(:)
 !
 integer, external     :: io_RT_components
 !
 call section('*','Two times Green function reconstruction')
 !
 call parser('IncludeEQocc',include_eq_occ)
 call parser('BuildSpec',   build_Spect)
 call parser('BuildGret',   build_G_ret)
 call parser('BuildGadv',   build_G_adv)
 call parser('BuildGles',   build_G_les)
 call parser('BuildGgrt',   build_G_grt)
 call parser('KeepCC' ,     keep_cc)
 call parser('KeepVV',      keep_vv)
 call parser('KeepCV' ,     keep_cv)
 call parser('KeepVC',      keep_vc)
 !
 call RT_k_grid(en,kpt,qpt)
 !
 ! I/O
 !=====
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1,2/),ID=ID) 
 io_err=io_RT_components('G',ID)
 if(.not.SAVE_G_history) &
   &  call error(" History of rho not found. Two times function cannot be computed")
 !
 RT_dyn_step=RT_step
 call RT_time_configuration_setup("Glesser",GF_RT_IO_t)
 !
 if (build_G_les) function_name="G<"
 if (build_G_grt) function_name="G>"
 if (build_G_ret) function_name="Gr"
 if (build_G_adv) function_name="G<"
 if (build_Spect) function_name=" A"
 !
 call msg('s',"Reconstructing "//function_name//"(t,t') from rho")
 !
 RT_nbands=RT_bands(2)-RT_bands(1)+1
 !
 YAMBO_ALLOC(G_tmp,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)))
 G_tmp=cZERO
 !
 YAMBO_ALLOC(  rho_T,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
 YAMBO_ALLOC(b_rho_T,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
 YAMBO_ALLOC(H_rho_T,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,4))
   rho_T=cZERO
 b_rho_T=cZERO
 H_rho_T=cZERO
 !
 YAMBO_ALLOC(Ho_plus_Sigma,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,1))
 Ho_plus_Sigma=cZERO
 YAMBO_ALLOC(dG_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,1))
 dG_lesser=cZERO
 !
 if(include_eq_occ) then
   YAMBO_ALLOC(G_lesser_reference,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID) 
   io_err=io_RT_components('REF',ID)
 endif
 !
 YAMBO_ALLOC(G_ret,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
 G_ret=cZERO
 !
 ! I need to keep k resolution to interpolate on a path
 !
 YAMBO_ALLOC(F_k_tmtp,(RT_nk,RT_conf%T_n_steps)) ! F(k,t-t') for a given T
 F_k_tmtp=cZERO
 !
 YAMBO_ALLOC(TMP_M,    (RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),2))
 YAMBO_ALLOC(I1_matrix,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)))
 I1_matrix=cZERO
 do ib1=RT_bands(1),RT_bands(2)
   I1_matrix(ib1,ib1)=1._SP
 enddo
 !
 ! First step i_Tc
 !
 T_c=(RT_conf%T_range(1)+RT_conf%T_range(2))/2._SP
 i_Tc=(RT_conf%T_n_steps+1)/2
 if(mod(RT_conf%T_n_steps,2)==0) call error("Number of time steps must be odd")
 !
 call msg('s',function_name//"(k,w) at time "//trim(real2ch(real(T_c/FS2AUT,SP)))//" fs")
 !
 Nt=GF_RT_IO_t%N
 !
 REF_diff=GF_RT_IO_t%Time(Nt)-GF_RT_IO_t%Time(Nt-G_MEM_steps)
 do i_t_tmp=1,Nt
   TIME_diff= abs ( GF_RT_IO_t%Time(i_t_tmp) - RT_time(i_Tc) )
   if (abs(TIME_diff)<REF_diff ) then 
     REF_diff=TIME_diff
     GF_RT_IO_t%N=i_t_tmp
   endif
 enddo
 !
 ! Load rho and H[rho]
 !
#if defined _PAR_IO
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/3/),ID=ID)
 io_err=io_RT_components('G',ID)
#else
 do ik = 1, RT_nk 
   call io_control(ACTION=OP_IF_START_RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID)
   io_err=io_RT_components('G',ID)
 enddo
#endif
 !
 ! rho and 1-rho
 !
 do ik=1,RT_nk
   !
   if(     include_eq_occ) TMP_M(:,:,1)=           -cI*G_lesser_reference(:,:,ik)
   if(.not.include_eq_occ) TMP_M(:,:,1)=  cZERO
   !
   if(     include_eq_occ) TMP_M(:,:,2)=( I1_matrix+cI*G_lesser_reference(:,:,ik) )
   if(.not.include_eq_occ) TMP_M(:,:,2)=( I1_matrix                               )
   !
   b_rho_T(:,:,ik)= TMP_M(:,:,2)-rho_T(:,:,ik)
     rho_T(:,:,ik)= TMP_M(:,:,1)+rho_T(:,:,ik)
   !
 enddo
 !
 ! Build G_ret
 !
 do ik=1,RT_nk
   G_ret(:,:,ik)=-cI*I1_matrix(:,:)
 enddo 
 !
 H_rho_T(:,:,:,3)=Ho_plus_sigma(:,:,:,1)
 H_rho_T(:,:,:,4)=Ho_plus_sigma(:,:,:,1)
 !
 !
 ! Build G_lesser / G_ret / Spectr
 !
 do ik=1,RT_nk
   !
   if(build_G_les) then
     call M_by_M('n','n',RT_nbands,-G_ret(:,:,ik),rho_T(:,:,ik),cZERO,G_tmp)
     call M_by_M('n','c',RT_nbands,+rho_T(:,:,ik),G_ret(:,:,ik),cONE ,G_tmp)
   endif
   !
   if(build_G_grt) then
     call M_by_M('n','n',RT_nbands,+G_ret(:,:,ik),b_rho_T(:,:,ik),G_tmp)
     call M_by_M('n','c',RT_nbands,-b_rho_T(:,:,ik),G_ret(:,:,ik),G_tmp)
   endif
   !
   if(build_G_ret) G_tmp=G_ret(:,:,ik)
   if(build_G_adv) G_tmp=              hermitian(G_ret(:,:,ik))
   if(build_Spect) G_tmp=G_ret(:,:,ik)-hermitian(G_ret(:,:,ik))
   !
   do ib1=RT_bands(1),RT_bands(2)
     do ib2=RT_bands(1),RT_bands(2)
       if((ib1<=en%nbf.and.ib2<=en%nbf).and.(.not.keep_vv) ) cycle
       if((ib1> en%nbf.and.ib2> en%nbf).and.(.not.keep_cc) ) cycle
       if((ib1> en%nbf.and.ib2<=en%nbf).and.(.not.keep_cv) ) cycle
       if((ib1<=en%nbf.and.ib2> en%nbf).and.(.not.keep_vc) ) cycle
       !if ( build_G_ret .and. (ib1> en%nbf.and.ib2<=en%nbf).and. keep_cv ) G_tmp(ib1,ib2)=G_tmp(ib1,ib2)*rho_T(ib2,ib1,ik)
       !if ( build_G_ret .and. (ib1<=en%nbf.and.ib2> en%nbf).and. keep_vc ) G_tmp(ib1,ib2)=G_tmp(ib1,ib2)*rho_T(ib2,ib1,ik)
       F_k_tmtp(ik,i_Tc)=F_k_tmtp(ik,i_Tc)+G_tmp(ib1,ib2)
     enddo
   enddo
   !
 enddo
 !
 !
 call live_timing("Build "//function_name//"(t,t'):",(RT_conf%T_n_steps-i_Tc))
 !
 do i_T1=i_Tc+1,RT_conf%T_n_steps
   !
   i_T2=i_Tc+(i_Tc-i_T1)
   !
   i_T=(/i_T1,i_T2/)
   !
   !i_dT=(i_T(1)-i_T(2))/2
   !
   ! Build G_ret(t,t') with t>t'
   !
   do i_kind=1,2
     !
     REF_diff=GF_RT_IO_t%Time(Nt)-GF_RT_IO_t%Time(Nt-G_MEM_steps)
     do i_t_tmp=1,Nt
       TIME_diff= abs ( GF_RT_IO_t%Time(i_t_tmp) - RT_time(i_T(i_kind)) )
       if (abs(TIME_diff)<REF_diff ) then 
         REF_diff=TIME_diff
         GF_RT_IO_t%N=i_t_tmp
       endif
     enddo
     !
     ! Load rho and H[rho]
     !
#if defined _PAR_IO
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/3/),ID=ID)
     io_err=io_RT_components('G',ID)
#else
     do ik = 1, RT_nk 
       call io_control(ACTION=OP_IF_START_RD_CL_IF_END,COM=NONE,SEC=(/ik+2/),ID=ID)
       io_err=io_RT_components('G',ID)
     enddo
#endif
     !
     H_rho_T(:,:,:,i_kind)=Ho_plus_sigma(:,:,:,1)
     !
     if(i_kind==2) then
       !
       rho_T(:,:,:)=-cI* dG_lesser(:,:,:,1)
       !
       ! rho and 1-rho
       !
       do ik=1,RT_nk
         !
         if(     include_eq_occ) TMP_M(:,:,1)=           -cI*G_lesser_reference(:,:,ik)
         if(.not.include_eq_occ) TMP_M(:,:,1)=  cZERO
         !
         if(     include_eq_occ) TMP_M(:,:,2)=( I1_matrix+cI*G_lesser_reference(:,:,ik) )
         if(.not.include_eq_occ) TMP_M(:,:,2)=( I1_matrix                               )
         !
         b_rho_T(:,:,ik)= TMP_M(:,:,2)-rho_T(:,:,ik)
           rho_T(:,:,ik)= TMP_M(:,:,1)+rho_T(:,:,ik)
         !
       enddo
       !
     endif
     !
   enddo
   !
   ! G^{(r)}(t+dt,t'-dt) = e^{-ih_{HSEX}[rho(t+dt)]dt} G^{(r)}(t,t') e^{-ih_{HSEX}[rho(t'-dt)]dt} 
   !
   do ik=1,RT_nk
     TMP_M(:,:,1)=(H_rho_T(:,:,ik,1)+H_rho_T(:,:,ik,3))/2._SP
     TMP_M(:,:,2)=(H_rho_T(:,:,ik,2)+H_rho_T(:,:,ik,4))/2._SP
     call RT_apply_Texp(TMP_M(:,:,1),G_ret(:,:,ik),TMP_M(:,:,2),RT_conf%delta_T,RT_conf%damp_factor,5)  
   enddo
   !
   ! Store previous H_rho_T
   !
   H_rho_T(:,:,:,3)=H_rho_T(:,:,:,1)
   H_rho_T(:,:,:,4)=H_rho_T(:,:,:,2)
   !
   do ik=1,RT_nk
     !
     if( build_G_les ) then
       !
       ! Build G<
       ! PRA 92, 033419 (2015), Eq.(19a)
       !
       call M_by_M('n','n',RT_nbands,-G_ret(:,:,ik),rho_T(:,:,ik),G_tmp)
       !
     endif
     !
     if( build_G_grt ) then
       !
       ! Build G>
       ! PRA 92, 033419 (2015), Eq.(19b)
       !
       call M_by_M('n','n',RT_nbands,+G_ret(:,:,ik),b_rho_T(:,:,ik),G_tmp)
       !
     endif
     !
     if( build_G_ret ) G_tmp= G_ret(:,:,ik)
     if( build_G_adv ) G_tmp=-G_ret(:,:,ik)
     if( build_Spect ) G_tmp= G_ret(:,:,ik)
     !
     ! PRA 92, 033419 (2015), Below Eq.(19)
     !
     !  G(a)(t',t)= hermitian(G(r)(t,t'))
     !  G<  (t',t)=-hermitian(G<  (t,t'))
     !  G>  (t',t)=-hermitian(G>  (t,t'))
     !
     do ib1=RT_bands(1),RT_bands(2)
       do ib2=RT_bands(1),RT_bands(2)
         if((ib1<=en%nbf.and.ib2<=en%nbf).and.(.not.keep_vv) ) cycle
         if((ib1> en%nbf.and.ib2> en%nbf).and.(.not.keep_cc) ) cycle
         if((ib1> en%nbf.and.ib2<=en%nbf).and.(.not.keep_cv) ) cycle
         if((ib1<=en%nbf.and.ib2> en%nbf).and.(.not.keep_vc) ) cycle
         !
         !if ( build_G_ret .and. (ib1> en%nbf.and.ib2<=en%nbf).and. keep_cv ) G_tmp(ib1,ib2)=G_tmp(ib1,ib2)*rho_T(ib2,ib1,ik)
         !if ( build_G_ret .and. (ib1<=en%nbf.and.ib2> en%nbf).and. keep_vc ) G_tmp(ib1,ib2)=G_tmp(ib1,ib2)*rho_T(ib2,ib1,ik)
         !if ( build_G_adv .and. (ib1> en%nbf.and.ib2<=en%nbf).and. keep_cv ) G_tmp(ib2,ib1)=rho_T(ib1,ib2,ik)*G_tmp(ib1,ib2)
         !if ( build_G_adv .and. (ib1<=en%nbf.and.ib2> en%nbf).and. keep_vc ) G_tmp(ib2,ib1)=rho_T(ib1,ib2,ik)*G_tmp(ib1,ib2)
         !
         if( .not.build_G_adv ) F_k_tmtp(ik,i_T(1))=F_k_tmtp(ik,i_T(1))+      G_tmp(ib1,ib2)  ! G_adv is zero for t>t'
         if( .not.build_G_ret ) F_k_tmtp(ik,i_T(2))=F_k_tmtp(ik,i_T(2))-conjg(G_tmp(ib2,ib1)) ! G_ret is zero for t<t'
         !
       enddo !ib1
     enddo   !ib2
     !
   enddo     !ik
   !
   call live_timing(steps=1)
   !
 enddo       !iT1
 !
 call live_timing()
 !
 YAMBO_FREE(TMP_M)
 YAMBO_FREE(I1_matrix)
 !
 YAMBO_FREE(  rho_T)
 YAMBO_FREE(b_rho_T)
 YAMBO_FREE(H_rho_T)
 !
 YAMBO_FREE(G_tmp)
 YAMBO_FREE(G_ret)
 YAMBO_FREE(dG_lesser)
 YAMBO_FREE(G_lesser_reference)
 !
 ! 1D FFT setup
 !--------------
 !
 GreenF_T_and_W%title=function_name//"(t,t')"
 RT_time=(RT_time-T_c)*2._SP
 !
 call RT_1D_Fourier_setup(GreenF_T_and_W,RT_conf,RT_nk) 
 !
 do ik=1,RT_nk
   GreenF_T_and_W%F_T(:,ik)=cmplx(F_k_tmtp(ik,:),kind=DP)
 enddo
 !
 YAMBO_FREE(F_k_tmtp)
 !
 call RT_1D_Fourier_Transform('T2W',GreenF_T_and_W,RT_conf,1)
 !
 ! Print G(w) integrated over k
 !
 file_name="G_w_integrated"
 call of_open_close(trim(file_name),'ot')
 headings=(/" E[eV]  ","Im[G(w)]","Re[G(w)]"/)
 call msg('o '//trim(file_name),"#",headings,INDENT=0,USE_TABS=.true.)
 !
 YAMBO_ALLOC(G_w,(RT_conf%W_n_steps))
 !
 G_w=cZERO
 do ik_RT=1,RT_nk
   ik=RT_all2ibz(ik_RT)
   G_w(:)=G_w(:)+GreenF_T_and_W%F_W(:,ik_RT)*kpt%weights(ik)
 enddo
 !
 do i_w=1,RT_conf%W_n_steps
   odata=(/real(GreenF_T_and_W%W(i_w),SP)*HA2EV,real(-cI*G_w(i_w),SP),real(G_w(i_w),SP)/)
   call msg('o '//trim(file_name),'',odata,INDENT=-2,USE_TABS=.true.)
 enddo
 !
 call of_open_close(trim(file_name))
 !
 YAMBO_FREE(G_w)
 !
 ! Print G(w,k) for specific k  (only gamma for now)
 !
 file_name="G_k1_w"
 call of_open_close(trim(file_name),'ot')
 headings=(/" E[eV]  ","Im[G(w)]","Re[G(w)]"/)
 call msg('o '//trim(file_name),"#",headings,INDENT=0,USE_TABS=.true.)
 !
 do i_w=1,RT_conf%W_n_steps
   odata=(/real(GreenF_T_and_W%W(i_w),SP)*HA2EV,    &
   &      -real(cI*GreenF_T_and_W%F_W(i_w,1),SP),   &
   &       real(   GreenF_T_and_W%F_W(i_w,1),SP)     /)
   call msg('o '//trim(file_name),'',odata,INDENT=-2,USE_TABS=.true.)
 enddo
 !
 call of_open_close(trim(file_name))
 !
 ! DEBUG <
 do ik=1,RT_nk
   !
   if(ik>99) cycle
   !
   do i_dT=1,RT_conf%T_n_steps
     write(100+ik,*) RT_time(i_dT)/FS2AUT,&
     &  real(GreenF_T_and_W%F_T(i_dT,ik),SP),real(-cI*GreenF_T_and_W%F_T(i_dT,ik),SP)
   enddo
   !
   do i_w=1,RT_conf%W_n_steps
     write(200+ik,*) real(GreenF_T_and_W%W(i_w))*HA2EV,&
     & real(-cI*GreenF_T_and_W%F_W(i_w,ik),SP),real(GreenF_T_and_W%F_W(i_w,ik),SP)
   enddo
   !
 enddo
 ! DEBUG >
 !
end subroutine RT_G_two_times_build
