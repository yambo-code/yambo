! 
! Copyright (C) 2000-2012 A. Marini and the YAMBO team
!              http://www.yambo-code.org
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
subroutine electrons_driver(Xk,Xen)
 !
 use pars,           ONLY:SP,schlen
 Use stderr,         ONLY:intc
 use wave_func,      ONLY:WF_load,wf,WF_free,wf_ng,wf_state
 use FFT_m,          ONLY:fft_size,fft_dim
 use R_lattice,      ONLY:bz_samp,nkibz
 use electrons,      ONLY:levels,n_spin,n_spinor,spin,n_sp_pol
 use QP_m,           ONLY:QP_table,QP_n_states,QP_state
 use YPP,            ONLY:l_density,l_mag,v2plot,output_fname,plot_dim,use_xcrysden,&
&                         use_gnuplot,use_cube,nr,l_sp_wf,deg_energy,mag_dir,l_norm_to_one,&
&                         plot_title,l_dos,l_angular_momentum,&
&                         l_position,l_current,l_bands
 use com,            ONLY:msg,of_open_close,warning
 use functions,      ONLY:Fermi_fnc_derivative
 use xc_functionals, ONLY:magn
#if defined _YPP_SC
 use YPP,            ONLY:V_value,l_mean_potential
 use wave_func,      ONLY:wf_ng
 use IO_m,           ONLY:OP_RD,RD_CL_IF_END,LOG,NONE,io_control,DUMP
 use SC,             ONLY:V_mean,SC_bands,SC_fft_size
 use R_lattice,      ONLY:qindx_S
 use com,            ONLY:error
#endif
#if defined _YPP_ELPH
 use electrons,      ONLY:n_bands
#endif
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: el_den(:)
 integer               :: i_qp,ik,ib,ibp,ir,i_wf,i_spin,mag_i_dir,nb_to_load(2),nkpt_to_load(2),ik_ref
 logical               :: success,flag
 character(schlen)     :: ch_ws(2)
#if defined _YPP_SC
 integer               :: ID,io_V
 integer, external     :: io_SC_components
#endif
 !
 flag=any((/l_mag,l_density,l_sp_wf,l_dos,l_bands/))
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
 ! Loading  Wf 
 !
 nb_to_load=(/1,Xen%nbm/)
 nkpt_to_load=(/1,Xk%nibz/)
 !
 flag=l_sp_wf
 !===
#if defined _YPP_MAGNETIC
 if (.not.flag) flag=any((/l_angular_momentum,l_position/))
#endif
#if defined _YPP_SC
 if (.not.flag) flag=l_mean_potential
#endif
 !
 if (flag) then
   call QP_state_table_setup(Xen)
   nb_to_load   =(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)         !(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)
   nkpt_to_load =(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)         !(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)  
#if defined _YPP_ELPH
   nb_to_load     =(/1,n_bands/)
   nkpt_to_load   =(/1,nkibz/)
#endif
 endif
 !
 flag=.not.l_dos
 !===
#if defined _YPP_MAGNETIC
 if (flag) flag=.not.any((/l_angular_momentum,l_position/))
#endif
#if defined _YPP_SC
 if (flag) flag=.not.l_mean_potential
#endif
 !
 if(flag) then
   call WF_load(wf_ng,1,nb_to_load,nkpt_to_load,space='R',title='-WF',impose_free_and_alloc=.TRUE.)
   nr=fft_dim
   allocate(v2plot(fft_size))
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
   allocate(V_mean(fft_size,SC_bands(2)),v2plot(fft_size))
 endif
#endif
 !
 ! DOS & DENSITY
 !===============
 !
 if (l_dos.or.l_density)  call electrons_dos_and_charge(Xk,Xen)
 !
 ! BANDS interpolation
 !======================
 !
 if(l_bands) call electrons_bands(Xk,Xen)
 !
 ! SYMMETRIZED WAVEFUNCTIONS (summed over all symmetries and degenerate states)
 !==============================================================================
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
&   ) then
   !
   v2plot=0.
   !
   if (l_sp_wf)          call section('*','Single Particle wavefunction Plot')
#if defined _YPP_SC
   if (l_mean_potential) call section('*','Mean Potential Plot')
#endif
   !
   if (n_spinor==2) then
     call warning ('Non collinear spin support still not implemented')
     goto 1
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
     i_spin=spin(QP_table(i_qp,:))
     !
     i_qp=i_qp+1
     !
#if defined _YPP_SC
     if (l_mean_potential.and.ik/=ik_ref) then
       call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID)
       io_V=io_SC_components('MP',Xen,ID)
       ik_ref=ik
     endif
#endif
     !
#if defined _YPP_SC
     if (.not.l_mean_potential) then
#endif
       i_wf=wf_state(ib,ik,i_spin)
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
       forall(ir=1:fft_size) v2plot(ir)=real( wf(ir,i_wf)*conjg( wf(ir,i_wf) ) )
#if defined _YPP_SC
     endif
#endif
     !
     ibp=ib+1
     if (ib==Xen%nb) ibp=ib
     if (ib/=Xen%nb.and.abs(Xen%E(ib,ik,i_spin)-Xen%E(ibp,ik,i_spin))<deg_energy) then
       cycle
     else
       !
       ! GPL_EXCLUDE_START SPIN_IGNORE
       !
       if (n_spin==2) then
         if (i_spin==1) ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_UP_'//trim(intc(plot_dim))
         if (i_spin==2) ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_DN_'//trim(intc(plot_dim))
         if (i_spin==1) plot_title='k '//trim(intc(ik))//' b '//trim(intc(ib))//' UP'
         if (i_spin==2) plot_title='k '//trim(intc(ik))//' b '//trim(intc(ib))//' DN'
       else
         !
         ! GPL_EXCLUDE_END SPIN_IGNORE
         !
         ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_'//trim(intc(plot_dim)) 
         !
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
       endif
       !
       ! GPL_EXCLUDE_END SPIN_IGNORE
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
 ! MAGNETIZATION GPL_EXCLUDE_START
 !=================================
 !
 if (l_mag) then
   !
   mag_i_dir=-1
   if (mag_dir=='X'.or.mag_dir=='x') mag_i_dir=1
   if (mag_dir=='Y'.or.mag_dir=='y') mag_i_dir=2
   if (mag_dir=='Z'.or.mag_dir=='z') mag_i_dir=3
   if (mag_i_dir<0) goto 1
   !
   allocate(magn(fft_size,3))
   !
   call section('*','Single Particle Magnetization along '//mag_dir)
   !
   call el_magnetization(Xen,Xk)
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
     do i_spin=1,2
       !
       ! rho DN
       if (i_spin==1) then
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
       if (i_spin==1) plot_title='den DN'
       if (i_spin==2) plot_title='den UP'
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
 ! GPL_EXCLUDE_END
 !
1 continue
 call WF_free()
 if (allocated(v2plot))   deallocate(v2plot)
 if (allocated(QP_table)) deallocate(QP_table,QP_state)
 if (allocated(magn))     deallocate(magn)
 if (allocated(el_den))   deallocate(el_den)
#if defined _YPP_SC 
 if (allocated(V_mean))   deallocate(V_mean)
#endif
 plot_title=' '
 !
end subroutine
