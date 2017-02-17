! 
! Copyright (C) 2000-2013 A. Marini and the YAMBO team
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
subroutine electrons_dos_and_charge(Xk,Xen)
 !
 use units,          ONLY:HA2EV,FS2AUT
 use pars,           ONLY:SP,schlen
 Use stderr,         ONLY:intc
 use R_lattice,      ONLY:bz_samp,nkibz
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:levels,spin,n_sp_pol,spin_occ
 use FFT_m,          ONLY:fft_size
 use YPP,            ONLY:l_density,v2plot,output_fname,plot_dim,use_xcrysden,&
&                         use_gnuplot,use_cube,plot_title,l_dos,dos_broadening,dos_bands,&
&                         dos_E_range,dos_E_steps
 use com,            ONLY:msg,of_open_close,warning
 use functions,      ONLY:Fermi_fnc_derivative
 use QP_CTL_m,       ONLY:QP_apply
 use QP_m,           ONLY:OCC_ctl
 use parser_m,       ONLY:parser     
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: el_den(:)
 real(SP)              :: el_dos(n_sp_pol),dos_E,delta_E,f_occ,io_Time
 integer               :: i_E,ik,ib,i_spin,i_T,n_T_steps
 character(schlen)     :: ch_ws,titles(4)
 logical               :: l_3D_plot,l_prt_occ_only
 !
 call parser('PrtOccOnly',l_prt_occ_only)
 !
 ! DOS
 !=====
 !
 if (l_dos) then
   !
   call section('*','Electronic DOS')
   !
   l_3D_plot=.FALSE.
   n_T_steps=1
   !
   !
   call QP_apply(dos_bands,Xen,Xk,'G',msg_fmt='s')
   !
   if (l_3D_plot) then
   else
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
   endif
   !
   if (dos_E_range(1)>dos_E_range(2)) then
     dos_E_range(1)=minval(Xen%E(dos_bands(1):dos_bands(2),:,:))-5.*dos_broadening
     dos_E_range(2)=maxval(Xen%E(dos_bands(1):dos_bands(2),:,:))-5.*dos_broadening
   endif
   delta_E=(dos_E_range(2)-dos_E_range(1))/dos_E_steps
   !
     !
     do i_E=1,dos_E_steps
       !
       dos_E=dos_E_range(1)+i_E*delta_E
       !
       el_dos(:)=0.
       do ib=dos_bands(1),dos_bands(2)
         do ik=1,nkibz
           do i_spin=1,n_sp_pol
             !
             f_occ=spin_occ
             if(l_prt_occ_only) f_occ=Xen%f(ib,ik,i_spin)/2.
             !
             el_dos(i_spin)=el_dos(i_spin)+f_occ*&
&                           Fermi_fnc_derivative(Xen%E(ib,ik,i_spin)-dos_E,dos_broadening)*Xk%weights(ik)/DL_vol
             !
           enddo
         enddo
       enddo
       !
       if (l_3D_plot) then
       else
         if (n_sp_pol==1) call msg('o dos','',(/dos_E*HA2EV,el_dos(1)/),INDENT=-2,USE_TABS=.true.)
         if (n_sp_pol==2) call msg('o dos','',(/dos_E*HA2EV,el_dos(:),el_dos(1)+el_dos(2)/),INDENT=-2,USE_TABS=.true.)
       endif
       !
     enddo
     !
   !
   if (l_3D_plot) then
   else
     call of_open_close(trim(output_fname))
   endif
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
end subroutine
