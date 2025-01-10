! 
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SC_mean_potential(Xen)
 !
 use pars,           ONLY:SP,lchlen
 Use stderr,         ONLY:intc
 use wave_func,      ONLY:WF,wf_ng
 use FFT_m,          ONLY:fft_size,fft_dim
 use electrons,      ONLY:levels,n_spinor,spin,n_sp_pol
 use QP_m,           ONLY:QP_table,QP_n_states
 use YPPm,           ONLY:v2plot,output_fname,plot_dim,use_xcrysden,&
&                         use_gnuplot,use_cube,nr,l_sp_wf,deg_energy,l_norm_to_one,&
&                         plot_title
 use com,            ONLY:msg,of_open_close
 use YPPm,           ONLY:V_value,l_mean_potential
 use QP_m,           ONLY:QP_nk
 use wave_func,      ONLY:wf_ng
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD,RD_CL_IF_END,LOG,NONE,manage_action,DUMP
 use SC,             ONLY:V_mean,SC_bands,SC_fft_size
 use R_lattice,      ONLY:qindx_S
 !
#include<y_memory.h>
 !
 type(levels)  ::Xen
 !
 ! Work Space
 !
 integer               :: i_qp,ik,i_sp_pol,i_frag,ib,ibp,ir,i_wf,nb_to_load(2),nkpt_to_load(2),ik_ref
 character(lchlen)     :: ch_ws(2)
 integer               :: ID,IO_ACT,io_V
 integer, external     :: io_SC_components
 !
 !
 if (l_mean_potential) then
   !
   call io_control(ACTION=OP_RD,COM=LOG,MODE=DUMP, SEC=(/1/),ID=ID)
   io_V=io_SC_components('MP',Xen,ID)
   !
   ! The SC FFT size is decided in SC_WF_and_dipole_dimensions. I dunno why
   ! QP_ng_Sx is not used there.
   !
   call msg("s",'FFT size evaluation...')
   call fft_setup(wf_ng,maxval(qindx_S(:,:,2)),.true.)
   if (SC_fft_size/=fft_size) call error('Mismatch in FFT size')
   call msg("l",'done')
   nr=fft_dim
   YAMBO_ALLOC(V_mean,(fft_size,SC_bands(2)))
   YAMBO_ALLOC(v2plot,(fft_size))
   !
 endif
 !
 ! WAVEFUNCTIONS (and Mean Potential)
 !====================================
 !
 ik_ref=-1
 ch_ws(2)='sp_wf'
 if (l_mean_potential) then
   if (trim(V_value)=="RE".or.trim(V_value)=="re") ch_ws(2)='V_re'
   if (trim(V_value)=="IM".or.trim(V_value)=="im") ch_ws(2)='V_im'
 endif
 !
 if (l_sp_wf.or.l_mean_potential) then
   !
   v2plot=0.
   !
   if (l_mean_potential) call section('*','Mean Potential Plot')
   !
   if (n_spinor==2.and.l_mean_potential) then
     call warning ('Non collinear spin support still not implemented')
     return
   endif
   !
   i_qp=1
   !
   do while (i_qp<=QP_n_states) 
     !
     ! n   =QP_table(i_qp,1)
     ! k   =QP_table(i_qp,3)
     ! sp  =QP_table(i_qp,4)
     !
     ib    =QP_table(i_qp,1)
     ik    =QP_table(i_qp,3)
     i_sp_pol=spin(QP_table(i_qp,:))
     !
     i_qp=i_qp+1
     !
     if (l_mean_potential.and.ik/=ik_ref) then
       i_frag=ik+(i_sp_pol-1)*QP_nk
       IO_ACT=manage_action(RD_CL_IF_END,i_frag,1,QP_nk*n_sp_pol)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/1+i_frag/),ID=ID)
       io_V=io_SC_components('MP',Xen,ID)
       ik_ref=ik
     endif
     !
     if (l_mean_potential) then
       v2plot=0._SP
       if (trim(V_value)=="RE".or.trim(V_value)=="re") forall(ir=1:fft_size) v2plot(ir)=real ( V_mean(ir,ib) )
       if (trim(V_value)=="IM".or.trim(V_value)=="im") forall(ir=1:fft_size) v2plot(ir)=aimag( V_mean(ir,ib) )
     endif
     !
     ibp=ib+1
     if (ib==Xen%nb) ibp=ib
     if (ib/=Xen%nb.and.abs(Xen%E(ib,ik,i_sp_pol)-Xen%E(ibp,ik,i_sp_pol))<deg_energy) then
       cycle
     else
       !
       if (n_sp_pol==2) then
         if (i_sp_pol==1) ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_UP_'//trim(intc(plot_dim))
         if (i_sp_pol==2) ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_DN_'//trim(intc(plot_dim))
         if (i_sp_pol==1) plot_title='k '//trim(intc(ik))//' b '//trim(intc(ib))//' UP'
         if (i_sp_pol==2) plot_title='k '//trim(intc(ik))//' b '//trim(intc(ib))//' DN'
       else
         ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_'//trim(intc(plot_dim)) 
       endif
       !
       if (use_cube) output_fname=trim(ch_ws(1))//'d.cube'
       if (use_xcrysden) output_fname=trim(ch_ws(1))//'d.xsf'
       if (use_gnuplot)  output_fname=trim(ch_ws(1))//'d'
       !
       if (use_cube) then 
         call of_open_close(trim(output_fname),'o')
       else
         call of_open_close(trim(output_fname),'ot')
         call msg('o wf',"#")
       endif
       !
       call plot_check_and_launch(.false.)
       !
       call of_open_close(trim(output_fname))
       !
     endif
     !
   enddo
    !
 endif
 !
 YAMBO_FREE(V_mean)
 !
end subroutine
