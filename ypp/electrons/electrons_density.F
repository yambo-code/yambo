! 
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine electrons_density(Xk,Xen)
 !
 use pars,           ONLY:SP,schlen
 use stderr,         ONLY:intc
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use FFT_m,          ONLY:fft_size
 use YPPm,           ONLY:v2plot,output_fname,plot_dim,use_xcrysden,&
&                         use_gnuplot,use_cube,plot_title,l_norm_to_one
 use com,            ONLY:msg,of_open_close
 use interfaces,     ONLY:el_density_and_current
 !
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 ! 
 real(SP)           :: el_den(fft_size)
 character(schlen)  :: ch_ws
 !
 call section('*','Density Plot')
 !
 call el_density_and_current(Xen,Xk,rho=el_den) 
 v2plot=el_den
 l_norm_to_one=.false.
 !
 ch_ws='density_'//trim(intc(plot_dim))
 if (use_cube)     output_fname=trim(ch_ws)//'d.cube'
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
end subroutine
