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
 use xc_functionals, ONLY:two_spin_density
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: el_den(:)
 integer               :: i_qp,ik,ib,ir,i_wf,i_spin,mag_i_dir,nb_to_load(2),nkpt_to_load(2),ik_ref
 logical               :: success,flag
 character(schlen)     :: ch_ws(2)
 !
 flag=any((/l_mag,l_density,l_sp_wf,l_dos,l_bands/))
 !===
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
 !
 if (flag) then
   call QP_state_table_setup(Xen)
   nb_to_load   =(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)         !(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)
   nkpt_to_load =(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)         !(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)  
 endif
 !
 flag=.not.l_dos
 !===
 !
 if(flag) then
   call WF_load(wf_ng,1,nb_to_load,nkpt_to_load,space='R',title='-WF',impose_free_and_alloc=.TRUE.)
   nr=fft_dim
   allocate(v2plot(fft_size))
 endif
 !
 !
 ! DOS & DENSITY
 !===============
 !
 if (l_dos.or.l_density)  call electrons_dos_and_charge(Xk,Xen)
 !
 ! BANDS interpolation
 !======================
 !
 !
 ! SYMMETRIZED WAVEFUNCTIONS (summed over all symmetries and degenerate states)
 !==============================================================================
 !
 ik_ref=-1
 ch_ws(2)='sp_wf'
 !
 if (l_sp_wf &
&   ) then
   !
   v2plot=0.
   !
   if (l_sp_wf)          call section('*','Single Particle wavefunction Plot')
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
     !
       i_wf=wf_state(ib,ik,i_spin)
     !
       forall(ir=1:fft_size) v2plot(ir)=real( wf(ir,i_wf)*conjg( wf(ir,i_wf) ) )
     !
     if (ib/=Xen%nb.and.abs(Xen%E(ib,ik,i_spin)-Xen%E(ib+1,ik,i_spin))<deg_energy) then
       cycle
     else
       !
         !
         ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_'//trim(intc(plot_dim)) 
         !
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
 !
1 continue
 call WF_free()
 if (allocated(v2plot))   deallocate(v2plot)
 if (allocated(QP_table)) deallocate(QP_table,QP_state)
 if (allocated(two_spin_density))   deallocate(two_spin_density)
 if (allocated(el_den))   deallocate(el_den)
 plot_title=' '
 !
end subroutine
