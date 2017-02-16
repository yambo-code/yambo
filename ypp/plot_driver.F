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
subroutine plot_driver(Xk,Xen)
 !
 use pars,          ONLY:SP
 use stderr,        ONLY:intc
 use wave_func,     ONLY:wf_load,wf,WF_free,wf_ng,wf_state
 use FFT_m,         ONLY:fft_size,fft_dim,fft_rot_r
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym
 use electrons,     ONLY:levels,n_spin,n_sp_pol
 use QP_m,          ONLY:QP_table,QP_n_states,QP_state
 use YPP,           ONLY:l_density,l_mag,v2plot,output_fname,plot_dim,use_xcrysden,&
&                        use_gnuplot,nr,l_sp_wf,deg_energy,mag_dir,l_norm_to_one
 use com,           ONLY:msg,of_open_close,warning
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: el_mag(:,:)
 integer :: i1,ik,ib,ir,is,i_wf,i_spin,mag_i_dir
 !
 if (.not.any((/l_mag,l_density,l_sp_wf/))) return
 !
 call plot_check_and_launch(.true.)
 !
 ! Loading  Wf 
 !
 call wf_load(wf_ng,1,(/1,Xen%nbm/),(/1,Xk%nibz/),space='R',title='-WF')
 !
 nr=fft_dim
 !
 allocate(v2plot(fft_size))
 !
 ! DENSITY
 !
 if (l_density) then
   !
   call section('*','Density Plot')
   !
   call el_density(Xen,Xk,v2plot,.FALSE.) 
   !
   if (use_xcrysden) output_fname='density_'//trim(intc(plot_dim))//'d.xsf'
   if (use_gnuplot)  output_fname='density_'//trim(intc(plot_dim))//'d'
   !
   call of_open_close(trim(output_fname),'ot')
   call msg('o den',"#")
   !
   call plot_check_and_launch(.false.)
   !
   call of_open_close(trim(output_fname))
   !
 endif
 !
 ! SYMMETRIZED WAVEFUNCTIONS (summed over all symmetries and degenerate states)
 !
 v2plot=0.
 !
 if (l_sp_wf) then
   !
   call section('*','Single Particle wavefunction Plot')
   !
   if (n_spin>1) then
     call warning ('SPIN support still not implemented')
     goto 1
   endif
   !
   call QP_state_table_setup(Xen)
   !
   i1=1
   !
   do while (i1<=QP_n_states) 
     !
     ! n   =QP_table(i1,1)
     ! k   =QP_table(i1,3)
     !
     ib =QP_table(i1,1)
     ik =QP_table(i1,3)
     !
     i_wf=wf_state(ib,ik,1)
     !
     i1=i1+1
     !
     do is=1,nsym
       forall(ir=1:fft_size) v2plot(ir)=v2plot(ir)+wf(fft_rot_r(i1,ir),i_wf)*conjg( wf(fft_rot_r(i1,ir),i_wf) )
     enddo
     !
     if (ib/=Xen%nb.and.abs(Xen%E(ib,ik,1)-Xen%E(ib+1,ik,1))<deg_energy) then
       cycle
     else
       if (use_xcrysden) output_fname='sp_wf_k'//trim(intc(ik))//'_b'//&
&                                      trim(intc(ib))//'_'//trim(intc(plot_dim))//'d.xsf'
       if (use_gnuplot)  output_fname='sp_wf_k'//trim(intc(ik))//'_b'//&
&                                      trim(intc(ib))//'_'//trim(intc(plot_dim))//'d'
       !
       call of_open_close(trim(output_fname),'ot')
       call msg('o wf',"#")
       !
       call plot_check_and_launch(.false.)
       !
       call of_open_close(trim(output_fname))
       !
       v2plot=0.
       !
     endif
   enddo
    !
 endif
 !
 ! MAGNETIZATION
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
   call plot_check_and_launch(.false.)
   !
   call of_open_close(trim(output_fname))
   !
 endif
 !
1 continue
 call WF_free()
 deallocate(v2plot)
 if (allocated(QP_table)) deallocate(QP_table,QP_state)
 if (allocated(el_mag))   deallocate(el_mag)
 !
end subroutine
