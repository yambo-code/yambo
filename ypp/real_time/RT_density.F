!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use R_lattice,      ONLY:bz_samp
 use FFT_m,          ONLY:fft_size,fft_dim
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc
 use com,            ONLY:msg,of_open_close,warning
 use IO_m,           ONLY:DUMP,OP_RD_CL,OP_IF_START_RD_CL_IF_END,NONE,io_control,LOG
 use RT_control,     ONLY:TIME_string,RT_apply,SAVE_G_history,GF_RT_IO_t
 use real_time,      ONLY:RT_nk,RT_bands,NE_i_time,NE_time,dG_lesser,RT_step,&
&                         RT_dyn_step,G_MEM_steps,RT_delta0_occupations
 use YPP,            ONLY:v2plot,output_fname,plot_dim,plot_title,&
&                         use_xcrysden,use_gnuplot,use_cube,nr,nr_tot,l_norm_to_one
 use YPP_real_time,  ONLY:RT_time,RT_conf
 use interfaces,     ONLY:WF_load,WF_free,el_density_and_current
 use wave_func,      ONLY:WF,wf_ng
 use interfaces,     ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 !
#include<memory.h>
 !
 type(levels)  :: en
 type(bz_samp) :: kpt,qpt
 !
 ! Workspace
 !
 integer               :: ID,ik,io_err,i_t
 real(SP), allocatable :: drho(:)
 character(schlen)     :: ch_ws(2)
 logical               :: load_G_history
 !
 integer, external     :: io_RT_components,RT_k_grid
 !
 call parser('LoadGhistory',load_G_history)
 !
 call section('*','Time dependent density plot')
 !
 RT_nk=RT_k_grid(en,kpt,qpt)
 !
 ! I/O
 !=====
 if(load_G_history) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) 
   io_err=io_RT_components('G',ID)
   if(.not.SAVE_G_history) then
     call warning(" History of G_lesser not found. Density will be computed using the occupations")
     load_G_history=.false.
   endif
 endif
 !
 if(load_G_history) then
   ch_ws(1)="Computing the density from the G lesser"
   YAMBO_ALLOC(dG_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,G_MEM_steps))
   dG_lesser=cZERO
 else
   ch_ws(1)="Computing the density from the occupations"
   call RT_apply(RT_bands,En,kpt,IO_COM=LOG,keep_RT_carriers=.TRUE.)
   YAMBO_ALLOC(RT_delta0_occupations,(RT_bands(1):RT_bands(2),RT_nk))
 endif
 !
 call msg('s',trim(ch_ws(1)))
 !
 RT_dyn_step=RT_step
 call RT_time_configuration_setup("DENSITY",GF_RT_IO_t)
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
 nr_tot = fft_size
 nr     = fft_dim
 YAMBO_ALLOC(v2plot,(nr_tot))
 !
 l_norm_to_one=.false.
 !
 call plot_check_and_launch(.true.)
 !
 do i_T=1,RT_conf%T_n_steps
   !
   NE_time=RT_time(i_T)
   NE_i_time=nint(NE_time/RT_step)+1
   !
   if (load_G_history) then
     !
     do ik = 1, RT_nk 
       call io_control(ACTION=OP_IF_START_RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID)
       io_err=io_RT_components('G',ID)
     enddo
     !
     call el_density_matrix(dG_lesser(:,:,:,1),en,kpt,drho,RT_bands(1))
     !
   else
     !
     call RT_apply(RT_bands,en,kpt,Time=NE_time,keep_RT_carriers=.TRUE.)
     !
     call el_density_and_current(en,kpt,rho=drho,bands=RT_bands)
     !
   endif
   !
   v2plot=drho
   !
   ch_ws(2)='RT_density_'//trim(intc(plot_dim))//'d@t'//trim(TIME_string( (NE_i_time-1)*RT_step ))
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
 enddo
 !
 if (load_G_history) then
   YAMBO_FREE(dG_lesser)
 else
   YAMBO_FREE(RT_delta0_occupations)
 endif
 YAMBO_FREE(drho)
 !
 call WF_free(WF)
 !
end subroutine RT_density
