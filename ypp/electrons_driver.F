! 
! Copyright (C) 2000-2010 A. Marini and the YAMBO team
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
 use units,          ONLY:HARTREE
 use pars,           ONLY:SP,schlen,lchlen
 Use stderr,         ONLY:intc
 use wave_func,      ONLY:WF_load,wf,WF_free,wf_ng,wf_state
 use FFT_m,          ONLY:fft_size,fft_dim
 use R_lattice,      ONLY:bz_samp,nkibz
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:levels,n_spin,n_spinor,spin,n_sp_pol
 use QP_m,           ONLY:QP_table,QP_n_states,QP_state
 use YPP,            ONLY:l_density,l_mag,v2plot,output_fname,plot_dim,use_xcrysden,&
&                         use_gnuplot,use_cube,nr,l_sp_wf,deg_energy,mag_dir,l_norm_to_one,&
&                         plot_title,l_dos,dos_broadening,DOS_bands,l_angular_momentum,&
&                         l_position,l_current
 use com,            ONLY:msg,of_open_close,warning
 use functions,      ONLY:Fermi_fnc_derivative
 use xc_functionals, ONLY:two_spin_density
 use QP_CTL_m,       ONLY:QP_apply
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: el_den(:)
 real(SP)              :: el_dos(n_sp_pol),dos_E,delta_E,E_min,E_max
 integer               :: i1,i_qp,ik,ib,ir,i_wf,i_spin,mag_i_dir,nb_to_load(2),nkpt_to_load(2)
 logical               :: success
 character(schlen)     :: ch_ws,titles(4)
 !
 if (.not.any((/l_mag,l_density,l_sp_wf,l_dos&
&   /))) return
 !
 call plot_check_and_launch(.true.)
 !
 ! Loading  Wf 
 !
 nb_to_load=(/1,Xen%nbm/)
 nkpt_to_load=(/1,Xk%nibz/)
 !
 if (l_sp_wf &
&) then
   call QP_state_table_setup(Xen)
   nb_to_load   =(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)         !(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)
   nkpt_to_load =(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)         !(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)  
 endif
 if(.not.l_dos &
&) then
   call WF_load(wf_ng,1,nb_to_load,nkpt_to_load,space='R',title='-WF',impose_free_and_alloc=.TRUE.)
   nr=fft_dim
   allocate(v2plot(fft_size))
 endif
 !
 ! DOS
 !=====
 !
 if (l_dos) then
   !
   call section('*','Electronic DOS')
   !
   call QP_apply((/1,DOS_bands/),Xen,Xk,3,msg_fmt='s')
   !
   output_fname='el_dos'
   call of_open_close(trim(output_fname),'ot')
   if (n_sp_pol==1) then
     titles(1)='E[eV]'
     titles(2)='DOS'
     call msg('o dos','#',titles(:2),INDENT=0,USE_TABS=.true.)    
   else
     titles(1)='E[eV]'
     titles(2)='DOS [up]'
     titles(3)='DOS [dn]'
     titles(4)='DOS [up+dn]'
     call msg('o dos','#',titles(:4),INDENT=0,USE_TABS=.true.)    
   endif
   call msg('o dos',"#")
   !
   E_min=minval(Xen%E(:DOS_bands,:,:))-5.*dos_broadening
   E_max=maxval(Xen%E(:DOS_bands,:,:))+5.*dos_broadening
   delta_E=(E_max-E_min)/int((E_max-E_min)/dos_broadening)
   !
   do i1=1,int((E_max-E_min)/dos_broadening)
     dos_E=E_min+(i1-0.5)*delta_E
     el_dos(:)=0.
     do ib=1,DOS_bands
       do ik=1,nkibz
         do i_spin=1,n_sp_pol
           !
           el_dos(i_spin)=el_dos(i_spin)+&
&                         Fermi_fnc_derivative(Xen%E(ib,ik,i_spin)-dos_E,dos_broadening)*Xk%weights(ik)/DL_vol
           !
         enddo
       enddo
     enddo
     !
     if (n_sp_pol==1) call msg('o dos','',(/dos_E*HARTREE,el_dos(1)/),INDENT=-2,USE_TABS=.true.)
     if (n_sp_pol==2) call msg('o dos','',(/dos_E*HARTREE,el_dos(:),el_dos(1)+el_dos(2)/),INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
   call of_open_close(trim(output_fname))
   !
 endif
 !
 ! DENSITY
 !=========
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
   if (use_cube) output_fname=trim(ch_ws)//'d.cube'
   if (use_xcrysden) output_fname=trim(ch_ws)//'d.xsf'
   if (use_gnuplot)  output_fname=trim(ch_ws)//'d'
   !
   if (use_cube) then 
     call of_open_close(trim(output_fname),'o')
   else
     call of_open_close(trim(output_fname),'ot')
     call msg('o den',"#")
   endif
   !
   plot_title='density'
   call plot_check_and_launch(.false.)
   !
   call of_open_close(trim(output_fname))
   !
 endif
 !
 ! SYMMETRIZED WAVEFUNCTIONS (summed over all symmetries and degenerate states)
 !==============================================================================
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
     i_wf=wf_state(ib,ik,i_spin)
     !
     forall(ir=1:fft_size) v2plot(ir)=real( wf(ir,i_wf)*conjg( wf(ir,i_wf) ) )
     !
     i_qp=i_qp+1
     !
     if (ib/=Xen%nb.and.abs(Xen%E(ib,ik,i_spin)-Xen%E(ib+1,ik,i_spin))<deg_energy) then
       cycle
     else
       !
         !
         ch_ws='sp_wf_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_'//trim(intc(plot_dim)) 
         !
       !
       if (use_cube) output_fname=trim(ch_ws)//'d.cube'
       if (use_xcrysden) output_fname=trim(ch_ws)//'d.xsf'
       if (use_gnuplot)  output_fname=trim(ch_ws)//'d'
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
