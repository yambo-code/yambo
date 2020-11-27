!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_density(en,kpt,qpt)
 !
 use pars,           ONLY:SP,schlen,cZERO
 use units,          ONLY:FS2AUT
 use R_lattice,      ONLY:bz_samp
 use FFT_m,          ONLY:fft_size,fft_dim
 use electrons,      ONLY:levels,n_spin
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc,real2ch
 use com,            ONLY:msg,of_open_close
 use IO_m,           ONLY:DUMP,OP_RD_CL,OP_IF_START_RD_CL_IF_END,NONE,io_control,LOG,&
&                         manage_action
 use RT_control,     ONLY:TIME_string,RT_apply,SAVE_G_history,GF_RT_IO_t,CARR_RT_IO_t
 use RT_occupations, ONLY:RT_el_occ,RT_occupation_alloc,RT_occupation_free
 use real_time,      ONLY:RT_nk,RT_bands,NE_i_time,dG_lesser,G_lesser_reference,RT_step,G_MEM_steps
 use YPPm,           ONLY:v2plot,output_fname,plot_dim,plot_title,&
&                         use_xcrysden,use_gnuplot,use_cube,nr,nr_tot,l_norm_to_one
 use YPP_real_time,  ONLY:RT_time,RT_conf
 use interfaces,     ONLY:WF_load,WF_free,el_density_and_current
 use wave_func,      ONLY:WF,wf_ng
 use parallel_int,   ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 !
#include<memory.h>
 !
 type(levels)  :: en
 type(bz_samp) :: kpt,qpt
 !
 ! Workspace
 !
 integer               :: ID,ik,io_err,i_t,i_t_tmp,i_dir,Nt,IO_ACT
 real(SP)              :: TIME_diff,REF_diff
 character(schlen)     :: ch_ws(3)
 logical               :: load_G_history,include_eq_occ
 !
 ! Allocs
 !
 real(SP), allocatable    :: drho(:),delta_magn(:,:)
 complex(SP), allocatable :: G_loc(:,:,:)
 !
 integer, external     :: io_RT_components
 !
 call parser('LoadGhistory',load_G_history)
 call parser('IncludeEQocc',include_eq_occ)
 !
 call section('*','Time dependent density plot')
 !
 call RT_k_grid(en,kpt,qpt)
 !
 ! I/O
 !=====
 if(load_G_history) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1,2/),ID=ID) 
   io_err=io_RT_components('G',ID)
   if(.not.SAVE_G_history) then
     call warning(" History of G_lesser not found. Density will be computed using the occupations")
     load_G_history=.false.
   endif
 endif
 !
 if(load_G_history) then
   ch_ws(1)="Computing the density from the G lesser"
   YAMBO_ALLOC(G_loc,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
   G_loc=cZERO
   YAMBO_ALLOC(dG_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,1))
   dG_lesser=cZERO
   if(include_eq_occ) then
     YAMBO_ALLOC(G_lesser_reference,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
     G_lesser_reference=cZERO
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2/),ID=ID) 
     io_err=io_RT_components('REF',ID)
   endif
   call msg('s',trim(ch_ws(1)))
   call RT_time_configuration_setup("G-DENSITY",GF_RT_IO_t)
 else
   ch_ws(1)="Computing the density from the occupations"
   call RT_apply(RT_bands,En,kpt,IO_COM=LOG,TIMES_ONLY=.TRUE.)
   call RT_occupation_alloc("electrons",RT_el_occ,RT_bands,RT_nk)
   call msg('s',trim(ch_ws(1)))
   call RT_time_configuration_setup("f-DENSITY",CARR_RT_IO_t)
 endif
 !
 !
 ! Parallel Setups & WF_load
 !===========================
 call PARALLEL_global_indexes(en,kpt,qpt," ",RESET=.TRUE.)
 call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 call WF_load(WF,wf_ng,1,RT_bands,(/1,kpt%nibz/),space='R',title='-WF',impose_free_and_alloc=.TRUE.)
 !
 YAMBO_ALLOC(drho,(fft_size))
 drho=0._SP
 !
 if(n_spin>1.and.load_G_history) then
   call build_spin_sop()
   YAMBO_ALLOC(delta_magn,(fft_size,3))
 endif
 !
 nr_tot = fft_size
 nr     = fft_dim
 YAMBO_ALLOC(v2plot,(nr_tot))
 !
 l_norm_to_one=.false.
 !
 call plot_check_and_launch(.true.)
 !
 Nt=GF_RT_IO_t%N
 !
 do i_T=1,RT_conf%T_n_steps
   !
   call msg('s','RT density at time '//trim(real2ch(real(RT_time(i_T)/FS2AUT,SP)))//' fs')
   !
   if (load_G_history) then
     !
     REF_diff=GF_RT_IO_t%Time(Nt)-GF_RT_IO_t%Time(Nt-G_MEM_steps)
     do i_t_tmp=1,Nt
       TIME_diff= abs ( GF_RT_IO_t%Time(i_t_tmp) - RT_time(i_T) )
       if (abs(TIME_diff)<REF_diff ) then 
         REF_diff=TIME_diff
         GF_RT_IO_t%N=i_t_tmp
       endif
     enddo
     !
#if defined _PAR_IO
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/3/),ID=ID)
     io_err=io_RT_components('G',ID)
#else
     do ik = 1, RT_nk 
       IO_ACT=manage_action(OP_IF_START_RD_CL_IF_END,ik,1,RT_nk)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+2/),ID=ID)
       io_err=io_RT_components('G',ID)
     enddo
#endif
     !
     if(     include_eq_occ) G_loc=dG_lesser(:,:,:,1)+G_lesser_reference
     if(.not.include_eq_occ) G_loc=dG_lesser(:,:,:,1)
     !
     call el_density_matrix(G_loc,en,kpt,drho,RT_bands(1))
     !
     if(n_spin>1) call el_magnetization_matrix(G_loc,en,kpt,delta_magn,RT_bands(1))
     !
   else
     !
     call RT_apply(RT_bands,en,kpt,Time=RT_time(i_T),keep_RT_carriers=.TRUE.)
     !
     call el_density_and_current(en,kpt,rho=drho,bands=RT_bands)
     !
     ! To be coded
     !if(n_spin>1) call el_magnetization(en,kpt,delta_magn,bands=RT_bands)
     !
   endif
   !
   v2plot=drho
   !
   ch_ws(2)='RT_density_'//trim(intc(plot_dim))//'d@t'//trim(TIME_string( RT_time(i_T) ))
   if (use_cube)     output_fname=trim(ch_ws(2))//'.cube'
   if (use_xcrysden) output_fname=trim(ch_ws(2))//'.xsf'
   if (use_gnuplot)  output_fname=trim(ch_ws(2))//''
   !
   if (use_cube) then 
     call of_open_close(trim(output_fname),'o')
   else
     call of_open_close(trim(output_fname),'ot')
     call msg('o RT_den',"# "//trim(ch_ws(1)))
     call msg('o RT_den',"#")
   endif
   !
   plot_title='RT_density'
   call plot_check_and_launch(.false.)
   !
   call of_open_close(trim(output_fname))
   !
   if(n_spin>1.and.load_G_history) then
     !
     do i_dir=1,3
       !
       v2plot=delta_magn(:,i_dir)
       !
       if(i_dir==1) ch_ws(2)='RT_magne_x'
       if(i_dir==2) ch_ws(2)='RT_magne_y'
       if(i_dir==3) ch_ws(2)='RT_magne_z'
       ch_ws(3)=trim(ch_ws(2))//'_'//trim(intc(plot_dim))//'d@t'//trim(TIME_string( (NE_i_time-1)*RT_step ))
       !
       if (use_cube)     output_fname=trim(ch_ws(3))//'.cube'
       if (use_xcrysden) output_fname=trim(ch_ws(3))//'.xsf'
       if (use_gnuplot)  output_fname=trim(ch_ws(3))//''
       !
       if (use_cube) then 
         call of_open_close(trim(output_fname),'o')
       else
         call of_open_close(trim(output_fname),'ot')
         call msg('o RT_magn',"# "//trim(ch_ws(1)))
         call msg('o RT_magn',"#")
       endif
       !
       plot_title=trim(ch_ws(2)//" ")
       call plot_check_and_launch(.false.)
       !
       call of_open_close(trim(output_fname))
       !
     enddo
     !
   endif
   !
 enddo
 !
 if (load_G_history) then
   YAMBO_FREE(dG_lesser)
   YAMBO_FREE(G_lesser_reference)
   YAMBO_FREE(G_loc)
 else
   call RT_occupation_free(RT_el_occ)
 endif
 YAMBO_FREE(drho)
 !
 if(n_spin>1.and.load_G_history) then
   YAMBO_ALLOC(delta_magn,(fft_size,3))
 endif
 !
 call WF_free(WF)
 !
end subroutine RT_density
