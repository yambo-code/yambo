! 
! Copyright (C) 2000-2008 A. Marini and the YAMBO team
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
 use pars,          ONLY:SP,schlen
 use stderr,        ONLY:intc
 use wave_func,     ONLY:wf_load,wf,WF_free,wf_ng,wf_state
 use FFT_m,         ONLY:fft_size,fft_dim
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym
 use electrons,     ONLY:levels,n_spin,n_spinor,spin,n_sp_pol
 use QP_m,          ONLY:QP_table,QP_n_states,QP_state
 use YPP,           ONLY:l_density,l_mag,v2plot,output_fname,plot_dim,use_xcrysden,&
&                        use_gnuplot,nr,l_sp_wf,deg_energy,mag_dir,l_norm_to_one,plot_title
 use com,           ONLY:msg,of_open_close,warning
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: el_mag(:,:),el_den(:)
 integer               :: i_qp,ik,ib,ir,is,i_wf,i_spin,mag_i_dir,nb_to_load(2)
 character(schlen)     :: ch_ws
 !
 if (.not.any((/l_mag,l_density,l_sp_wf/))) return
 !
 call plot_check_and_launch(.true.)
 !
 ! Loading  Wf 
 !
 nb_to_load=(/1,Xen%nbm/)
 if (l_sp_wf) then
   call QP_state_table_setup(Xen)
   nb_to_load=(/1,maxval(QP_table(:,1))/)
 endif
 call wf_load(wf_ng,1,nb_to_load,(/1,Xk%nibz/),space='R',title='-WF')
 nr=fft_dim
 allocate(v2plot(fft_size))
 !
 ! DENSITY
 !
 if (l_density) then
   !
   call section('*','Density Plot')
   !
   allocate(el_den(fft_size))
   !
   call el_density(Xen,Xk,el_den,.FALSE.) 
   v2plot=el_den
   !
   ch_ws='density_'//trim(intc(plot_dim))
   if (use_xcrysden) output_fname=trim(ch_ws)//'d.xsf'
   if (use_gnuplot)  output_fname=trim(ch_ws)//'d'
   !
   call of_open_close(trim(output_fname),'ot')
   call msg('o den',"#")
   !
   plot_title='density'
   call plot_check_and_launch(.false.)
   !
   call of_open_close(trim(output_fname))
   !
 endif
 !
 ! SYMMETRIZED WAVEFUNCTIONS (summed over all symmetries and degenerate states)
 !
 if (l_sp_wf) then
   !
   v2plot=0.
   !
   call section('*','Single Particle wavefunction Plot')
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
     i_wf=wf_state(ib,ik,i_spin)
     !
     forall(ir=1:fft_size) v2plot(ir)=wf(ir,i_wf)*conjg( wf(ir,i_wf) )
     !
     if (ib/=Xen%nb.and.abs(Xen%E(ib,ik,i_spin)-Xen%E(ib+1,ik,i_spin))<deg_energy) then
       cycle
     else
       !
       ! GPL_EXCLUDE_START SPIN_IGNORE
       !
       if (n_spin==2) then
         if (i_spin==1) ch_ws='sp_wf_k'//trim(intc(ik))//'_b'//&
&              trim(intc(ib))//'_UP_'//trim(intc(plot_dim))
         if (i_spin==2) ch_ws='sp_wf_k'//trim(intc(ik))//'_b'//&
&              trim(intc(ib))//'_DN_'//trim(intc(plot_dim))
         if (i_spin==1) plot_title='k '//trim(intc(ik))//' b '//trim(intc(ib))//' UP'
         if (i_spin==2) plot_title='k '//trim(intc(ik))//' b '//trim(intc(ib))//' DN'
       else
         !
         ! GPL_EXCLUDE_END SPIN_IGNORE
         !
         ch_ws='sp_wf_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_'//trim(intc(plot_dim)) 
         !
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
       endif
       !
       ! GPL_EXCLUDE_END SPIN_IGNORE
       !
       if (use_xcrysden) output_fname=trim(ch_ws)//'d.xsf'
       if (use_gnuplot)  output_fname=trim(ch_ws)//'d'
       !
       call of_open_close(trim(output_fname),'ot')
       call msg('o wf',"#")
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
 !
 if (l_mag) then
   !
   mag_i_dir=-1
   if (mag_dir=='X'.or.mag_dir=='x') mag_i_dir=1
   if (mag_dir=='Y'.or.mag_dir=='y') mag_i_dir=2
   if (mag_dir=='Z'.or.mag_dir=='z') mag_i_dir=3
   if (mag_i_dir<0) goto 1
   !
   allocate(el_mag(fft_size,3))
   !
   call section('*','Single Particle Magnetization along '//mag_dir)
   !
   call el_magnetization(Xen,Xk,el_mag)
   !
   v2plot=el_mag(:,mag_i_dir)
   !
   if (use_xcrysden) output_fname='mag_'//trim(mag_dir)//'_'//trim(intc(plot_dim))//'d.xsf'
   if (use_gnuplot)  output_fname='mag_'//trim(mag_dir)//'_'//trim(intc(plot_dim))//'d'
   !
   l_norm_to_one=.false.
   !
   call of_open_close(trim(output_fname),'ot')
   call msg('o mag',"#")
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
         v2plot=(el_den(:)-el_mag(:,3))/2.
         ch_ws='density_DN_'//trim(intc(plot_dim))
       else
         !
         ! rho UP
         v2plot=(el_den(:)+el_mag(:,3))/2.
         ch_ws='density_UP_'//trim(intc(plot_dim))
       endif
       !
       if (use_xcrysden) output_fname=trim(ch_ws)//'d.xsf'
       if (use_gnuplot)  output_fname=trim(ch_ws)//'d'
       !
       call of_open_close(trim(output_fname),'ot')
       call msg('o density_UP density_DN',"#")
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
 ! GPL_EXCLUDE_END
 !
1 continue
 call WF_free()
 deallocate(v2plot)
 if (allocated(QP_table)) deallocate(QP_table,QP_state)
 if (allocated(el_mag))   deallocate(el_mag)
 if (allocated(el_den))   deallocate(el_den)
 plot_title=' '
 !
end subroutine
