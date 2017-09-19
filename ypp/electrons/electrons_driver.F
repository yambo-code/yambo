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
subroutine electrons_driver(Xk,Xen,Xq)
 !
 use pars,           ONLY:SP,lchlen
 use units,          ONLY:AU2nAmpere
 Use stderr,         ONLY:intc
 use wave_func,      ONLY:WF,wf_ng
 use FFT_m,          ONLY:fft_size,fft_dim
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:i_time_rev
 use com,            ONLY:error
 use electrons,      ONLY:levels,n_spinor,spin,n_sp_pol
 use QP_m,           ONLY:QP_table,QP_n_states,QP_state
 use YPP,            ONLY:l_density,l_mag,v2plot,output_fname,plot_dim,use_xcrysden,&
&                         use_gnuplot,use_cube,nr,l_sp_wf,deg_energy,mag_dir,l_norm_to_one,&
&                         plot_title,l_dos,l_bands,l_current,current_dir,plot_is_1D
 use com,            ONLY:msg,of_open_close,warning
 use functions,      ONLY:Fermi_fnc_derivative
 use xc_functionals, ONLY:magn
#if defined _YPP_RT
 use pars,           ONLY:cZERO
 use YPP_real_time,  ONLY:l_RT_bands
#endif
#if defined _YPP_ELPH
 use YPP_ELPH,       ONLY:elph_residual_treshold
 use pars,           ONLY:cZERO
 use ELPH,           ONLY:elph_Ham_bands
 use com,            ONLY:jobstr,file_exists
 use R_lattice,      ONLY:nkibz
#endif
 use interfaces,     ONLY:WF_load,el_density_and_current,PARALLEL_WF_index,PARALLEL_global_indexes,PARALLEL_WF_distribute
 !
#include<memory.h>
 !
 type(bz_samp) ::Xk,Xq
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: el_den(:),el_cur(:,:)
 integer               :: i_qp,ik,i_sp_pol,ib,ibp,ir,i_wf,mag_i_dir,nb_to_load(2),nkpt_to_load(2),ik_ref
 logical               :: flag
 character(lchlen)     :: ch_ws(2)
#if defined _YPP_ELPH
 integer               :: iq_s,iq_ibz
 logical               :: l_polaronic_wf
#endif
 !
#if defined _YPP_ELPH
 !
 !Polaronic Wf
 !
 integer, parameter      :: n_max_res=100
 integer                 :: n_top_residuals,top_residual(n_max_res),i_st,H_size,i_c
 character(lchlen)       :: db_name
 integer                 :: ok_ibz,il,ok_s
 real(SP)                :: polaronic_residual(n_max_res),max_residual
 complex(SP)             :: B_n_ok
 complex(SP), allocatable:: v2plot_c(:),wf_rot(:)
 !
#endif
 !
 ! Doing something?
 !==================
 !
 flag=any((/l_mag,l_density,l_sp_wf,l_dos,l_bands,l_current/))
 !===
 !
 if (.not.flag) return
 !
 call plot_check_and_launch(.true.)
 !
 ! QP indexes loading
 !====================
 !
 nb_to_load=(/1,Xen%nbm/)
 nkpt_to_load=(/1,Xk%nibz/)
 !
 flag=any((/l_sp_wf,l_bands/))
 !===
 !
 if (flag) then
   !
   call QP_state_table_setup(Xen)
   nb_to_load   =(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)
   nkpt_to_load =(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)
   !
#if defined _YPP_ELPH
   l_polaronic_wf=.FALSE.
   db_name=trim(jobstr)//'/TOP_RESIDUAL_b'//trim(intc(QP_table(1,1)))//'_k'//trim(intc(QP_table(1,3)))
   if (file_exists(trim(db_name))) then
     l_polaronic_wf=.TRUE.
     open(unit=100,file=trim(db_name),form='unformatted')
     read(100) elph_Ham_bands
     close(100)
     nb_to_load    =elph_Ham_bands
     nkpt_to_load  =(/1,nkibz/)
   endif
#endif
   !
 endif
 !
 ! WFs
 !=====
 !
 flag=.not.l_dos.and..not.l_bands
 !===
 !
 if(flag) then
   !
   call PARALLEL_global_indexes(Xen,Xk,Xq," ",RESET=.TRUE.)
   call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
   call PARALLEL_WF_index( )
   !
   call WF_load(WF,wf_ng,1,nb_to_load,nkpt_to_load,space='R',title='-WF',impose_free_and_alloc=.TRUE.)
   nr=fft_dim
   YAMBO_ALLOC(v2plot,(fft_size))
#if defined _YPP_ELPH
   if (l_polaronic_wf) then
     YAMBO_ALLOC(v2plot_c,(fft_size))
     YAMBO_ALLOC(wf_rot,(fft_size))
   endif
#endif
 endif
 !
 !
 ! DOS 
 !=====
 if (l_dos)     call electrons_dos(Xk,Xen)
 !
 ! DENSITY
 !=========
 if (l_density) call electrons_density(Xk,Xen)
 !
 ! BANDS interpolation
 !======================
 !
 if(l_bands) then
   !
   if ( i_time_rev==1 ) call error(' Bands interpolation does not work with T-rev symmetry')
   !
   call electrons_bands(Xk,Xen,nb_to_load)
   !
   return
   !
 endif
 !
 !
 ! WAVEFUNCTIONS (and Mean Potential)
 !====================================
 !
 ik_ref=-1
 ch_ws(2)='sp_wf'
 !
 if (l_sp_wf &
#if defined _YPP_ELPH
&    .and..not.l_polaronic_wf &
#endif
&   ) then
   !
   v2plot=0.
   !
   if (l_sp_wf)          call section('*','Single Particle wavefunction Plot')
   !
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
     !
       i_wf=WF%index(ib,ik,i_sp_pol)
     !
       forall(ir=1:fft_size) v2plot(ir)=real( WF%c(ir,1,i_wf)*conjg( WF%c(ir,1,i_wf) ) )
       if (n_spinor==2) forall(ir=1:fft_size) v2plot(ir)=v2plot(ir) + real( WF%c(ir,2,i_wf)*conjg( WF%c(ir,2,i_wf) ) )
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
 ! MAGNETIZATION 
 !===============
 !
 if (l_mag) then
   !
   mag_i_dir=-1
   if (mag_dir=='X'.or.mag_dir=='x') mag_i_dir=1
   if (mag_dir=='Y'.or.mag_dir=='y') mag_i_dir=2
   if (mag_dir=='Z'.or.mag_dir=='z') mag_i_dir=3
   if (mag_i_dir<0) goto 1
   !
   YAMBO_ALLOC(magn,(fft_size,3))
   !
   call section('*','Single Particle Magnetization along '//mag_dir)
   !
   call el_magnetization(Xen,Xk,magn)
   !
   v2plot=magn(:,mag_i_dir)
   !
   if (use_cube) output_fname='mag_'//trim(mag_dir)//'_'//trim(intc(plot_dim))//'d.cube'
   if (use_xcrysden) output_fname='mag_'//trim(mag_dir)//'_'//trim(intc(plot_dim))//'d.xsf'
   if (use_gnuplot)  output_fname='mag_'//trim(mag_dir)//'_'//trim(intc(plot_dim))//'d'
   !
   l_norm_to_one=.false.
   !
   if (use_cube) then 
     call of_open_close(trim(output_fname),'o')
   else
     call of_open_close(trim(output_fname),'ot')
     call msg('o mag',"#")
   endif
   !
   plot_title='magnetization'
   !
   call plot_check_and_launch(.false.)
   !
   call of_open_close(trim(output_fname))
   !
   if (n_sp_pol==2.and.l_density) then
     !
     call section('*','Spin Polarized densities')
     !
     do i_sp_pol=1,2
       !
       ! rho DN
       if (i_sp_pol==1) then
         v2plot=(el_den(:)-magn(:,3))/2.
         ch_ws(1)='density_DN_'//trim(intc(plot_dim))
       else
         !
         ! rho UP
         v2plot=(el_den(:)+magn(:,3))/2.
         ch_ws(1)='density_UP_'//trim(intc(plot_dim))
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
         call msg('o density_UP density_DN',"#")
       endif
       !
       if (i_sp_pol==1) plot_title='den DN'
       if (i_sp_pol==2) plot_title='den UP'
       call plot_check_and_launch(.false.)
       !
       call of_open_close(trim(output_fname))
       !
     enddo
     !
   endif
   !
   !
 endif
 !
 !
1 continue
 call WF_free(WF)
 YAMBO_FREE(v2plot)
 YAMBO_FREE(QP_table)
 YAMBO_FREE(QP_state)
 YAMBO_FREE(magn)
 YAMBO_FREE(el_den)
 YAMBO_FREE(el_cur)
#if defined _YPP_ELPH 
 YAMBO_FREE(v2plot_c)
#endif
 plot_title=' '
 !
end subroutine
