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
#if defined _YPP_SC
 use YPP,            ONLY:V_value,l_mean_potential
 use wave_func,      ONLY:wf_ng
 use IO_m,           ONLY:OP_RD,RD_CL_IF_END,LOG,NONE,io_control,DUMP
 use SC,             ONLY:V_mean,SC_bands,SC_fft_size
 use R_lattice,      ONLY:qindx_S
#endif
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
#if defined _YPP_MAGNETIC
 use YPP,            ONLY:l_angular_momentum,l_position
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
#if defined _YPP_SC
 integer               :: ID,io_V
 integer, external     :: io_SC_components
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
#if defined _YPP_MAGNETIC
 if (.not.flag) flag=any((/l_angular_momentum,l_position/))
#endif
#if defined _YPP_SC
 if (.not.flag) flag=l_mean_potential
#endif
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
#if defined _YPP_MAGNETIC
 if (.not.flag) flag=any((/l_angular_momentum,l_position/))
#endif
 !GPL_EXCLUDE_START
#if defined _YPP_RT
 if (.not.flag) flag=l_RT_bands
#endif
 !GPL_EXCLUDE_END
#if defined _YPP_SC
 if (.not.flag) flag=l_mean_potential
#endif
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
#if defined _YPP_MAGNETIC
 if (flag) flag=.not.any((/l_angular_momentum,l_position/))
#endif
#if defined _YPP_SC
 if (flag) flag=.not.l_mean_potential
#endif
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
#if defined _YPP_SC 
 if (l_mean_potential) then
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
 endif
#endif
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
 ! GPL_EXCLUDE_START
 !
 ! SYMMETRIZED WAVEFUNCTIONS (summed over all symmetries and degenerate states)
 !==============================================================================
 !
#if defined _YPP_ELPH
 if (l_polaronic_wf) then
   !
   i_qp=1
   !
   l_norm_to_one=.true.
   !
   do while (i_qp<=QP_n_states) 
     !
     ib    =QP_table(i_qp,1)
     ik    =QP_table(i_qp,3)
     i_sp_pol=spin(QP_table(i_qp,:))
     !
     db_name=trim(jobstr)//'/TOP_RESIDUAL_b'//trim(intc(ib))//'_k'//trim(intc(ik))
     !
     i_qp=i_qp+1
     !
     if (.not.file_exists(trim(db_name))) cycle
     !  
     do i_st=1,2
       n_top_residuals=1
       open(unit=100,file=trim(db_name),form='unformatted')
       do while(.true.)
         read(100,end=2) top_residual(n_top_residuals),polaronic_residual(n_top_residuals)
         if  (polaronic_residual(n_top_residuals)>elph_residual_treshold/100.*max_residual) n_top_residuals=n_top_residuals+1
       enddo
2      n_top_residuals=n_top_residuals-1
       close(100)
       if (i_st==1) max_residual=maxval(polaronic_residual(:n_top_residuals))
     enddo 
     ! 
     call msg('s',':: Found '//trim(intc(n_top_residuals))//' residuals@k'//trim(intc(ik))//&
&                 '_b'//trim(intc(ib))//':', top_residual(:n_top_residuals))
     !
     do i_st=1,n_top_residuals
       !
       if  (polaronic_residual(i_st)<elph_residual_treshold/100.*maxval(polaronic_residual(:n_top_residuals))) cycle
       !
       v2plot_c=cZERO
       !
       db_name=trim(jobstr)//'/EIGENV_I'//trim(intc(top_residual(i_st)))//'_k'//trim(intc(ik))
       open(unit=100,file=trim(db_name),form='unformatted')
       read(100) H_size
       do i_c=1,H_size
         read(100) ibp,ok_ibz,ok_s,il,iq_ibz,iq_s,B_n_ok
         !
         if (il==0) then
           !
           i_wf=WF%index(ib,ik,1)
           !
           forall(ir=1:fft_size) v2plot_c(ir)=v2plot_c(ir)+WF%c(ir,1,i_wf)
           !
         else
           !
           call WF_apply_symm((/ibp,ok_ibz,ok_s,1/),wf_rot)
           !
           forall(ir=1:fft_size) v2plot_c(ir)=v2plot_c(ir)+B_n_ok*wf_rot(ir)
           !
         endif
         !
       enddo
       close(100)
       !
       v2plot  =abs(real(v2plot_c))
       !
       ch_ws(1)='sp_wf_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_'//trim(intc(plot_dim))//&
&               '_STATE'//trim(intc(top_residual(i_st)))//'_'//trim(intc(plot_dim))
       if (use_xcrysden) output_fname=trim(ch_ws(1))//'d.xsf'
       if (use_gnuplot)  output_fname=trim(ch_ws(1))//'d'
       !
       call of_open_close(trim(output_fname),'ot')
       call msg('o wf',"#")
       !
       call plot_check_and_launch(.false.)
       !
       call of_open_close(trim(output_fname))
       !
     enddo
     !
   enddo
   !
 endif
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
 ! WAVEFUNCTIONS (and Mean Potential)
 !====================================
 !
 ik_ref=-1
 ch_ws(2)='sp_wf'
#if defined _YPP_SC
 if (l_mean_potential) then
   if (trim(V_value)=="RE".or.trim(V_value)=="re") ch_ws(2)='V_re'
   if (trim(V_value)=="IM".or.trim(V_value)=="im") ch_ws(2)='V_im'
 endif
#endif
 !
 if (l_sp_wf &
#if defined _YPP_SC
&    .or.l_mean_potential &
#endif
#if defined _YPP_ELPH
&    .and..not.l_polaronic_wf &
#endif
&   ) then
   !
   v2plot=0.
   !
   if (l_sp_wf)          call section('*','Single Particle wavefunction Plot')
#if defined _YPP_SC
   if (l_mean_potential) call section('*','Mean Potential Plot')
#endif
   !
#if defined _YPP_SC
   if (n_spinor==2.and.l_mean_potential) then
     call warning ('Non collinear spin support still not implemented')
     goto 1
   endif
#endif
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
#if defined _YPP_SC
     if (l_mean_potential.and.ik/=ik_ref) then
       call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1+(i_sp_pol-1)*QP_nk/),ID=ID)
       io_V=io_SC_components('MP',Xen,ID)
       ik_ref=ik
     endif
#endif
     !
#if defined _YPP_SC
     if (.not.l_mean_potential) then
#endif
       i_wf=WF%index(ib,ik,i_sp_pol)
#if defined _YPP_SC
     endif
#endif
     !
#if defined _YPP_SC
     if (l_mean_potential) then
       v2plot=0._SP
       if (trim(V_value)=="RE".or.trim(V_value)=="re") forall(ir=1:fft_size) v2plot(ir)=real ( V_mean(ir,ib) )
       if (trim(V_value)=="IM".or.trim(V_value)=="im") forall(ir=1:fft_size) v2plot(ir)=aimag( V_mean(ir,ib) )
     endif
#endif
#if defined _YPP_SC
     if (.not.l_mean_potential) then 
#endif
       forall(ir=1:fft_size) v2plot(ir)=real( WF%c(ir,1,i_wf)*conjg( WF%c(ir,1,i_wf) ) )
       if (n_spinor==2) forall(ir=1:fft_size) v2plot(ir)=v2plot(ir) + real( WF%c(ir,2,i_wf)*conjg( WF%c(ir,2,i_wf) ) )
#if defined _YPP_SC
     endif
#endif
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
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_MAGNETIC
 !
 ! Angular momentum 
 !==================
 if (l_angular_momentum) call electrons_angular_momentum(nkpt_to_load,nb_to_load)
 !
 ! <Mean> position
 !=================
 if (l_position)         call electrons_position(nkpt_to_load,nb_to_load,1.,'cylindrical','2d')
 !
#endif
 !
 ! CURRENT
 !=========
 !
 if (l_current) then
   !
   call error(' Implementation to be finalized. Presently it is not working.')
   !
   call section('*','Current Plot')
   !
   YAMBO_ALLOC(el_cur,(fft_size,3))
   !
   call el_density_and_current(Xen,Xk,J=el_cur)
   !
   v2plot=v2plot/abs(maxval( v2plot ))
   !
   call msg("s",'Current integrals (x,y,z) [nAmpere]:',&
&               (/Sum(el_cur(:,1)),Sum(el_cur(:,2)),Sum(el_cur(:,3))/)*AU2nAmpere)
   !
   l_norm_to_one=.true.
   !
   ch_ws(1)='current_'//trim(intc(plot_dim))
   if (use_cube) output_fname=trim(ch_ws(1))//'d.cube'
   if (use_xcrysden) output_fname=trim(ch_ws(1))//'d.xsf'
   if (use_gnuplot)  output_fname=trim(ch_ws(1))//'d'
   !
   if (.not.use_cube) then 
     call of_open_close(trim(output_fname),'ot')
     call msg('o current',"#")
   endif
   !
   if (any(plot_is_1D)) then
     !
     call plot_and_project_1D(el_cur,current_dir)
     !
     call of_open_close(trim(output_fname))
     !
   else
     !
     v2plot=(el_cur(:,1)*current_dir(1)+el_cur(:,2)*current_dir(2)+&
&            el_cur(:,3)*current_dir(3))*AU2nAmpere
     !
     if (use_cube) then 
       call of_open_close(trim(output_fname),'o')
     endif
     !
     plot_title='current'
     call plot_check_and_launch(.false.)
     !
   endif
   !
   call of_open_close(trim(output_fname))
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
1 continue
 call WF_free(WF)
 YAMBO_FREE(v2plot)
 YAMBO_FREE(QP_table)
 YAMBO_FREE(QP_state)
 YAMBO_FREE(magn)
 YAMBO_FREE(el_den)
 YAMBO_FREE(el_cur)
#if defined _YPP_SC 
 YAMBO_FREE(V_mean)
#endif
#if defined _YPP_ELPH 
 YAMBO_FREE(v2plot_c)
#endif
 plot_title=' '
 !
end subroutine
