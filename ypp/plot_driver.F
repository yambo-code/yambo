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
 use wave_func,     ONLY:wf_load,wf,WF_free,wf_ng 
 use FFT_m,         ONLY:fft_size,fft_dim
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels
 use YPP,           ONLY:l_density,l_mag,v2plot,output_fname,plot_dim,use_xcrysden,use_gnuplot,nr
 use com,           ONLY:msg,of_open_close
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: rho(:),el_mag(:,:)
 !
 call plot_check_and_launch(.true.)
 !
 ! Loading  Wf 
 !
 call wf_load(wf_ng,1,Xen%nbm,(/1,Xk%nibz/),space='R',title='-WF')
 !
 nr=fft_dim
 !
 allocate(v2plot(fft_size))
 !
 if (l_density) then
   !
   call el_density(Xen,Xk,v2plot,.FALSE.) 
write (*,*) Xen%f
write (*,*) Xk%weights
stop
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
!   !
!   call plot_check_and_launch(.false.)
!   !
 call WF_free()
 deallocate(v2plot)
 !
end subroutine
