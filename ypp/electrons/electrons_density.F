! 
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine electrons_density(Xk,Xen)
 !
 use pars,           ONLY:SP,schlen
 use stderr,         ONLY:intc
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use FFT_m,          ONLY:fft_size
 use YPPm,           ONLY:v2plot,output_fname,plot_dim,use_xcrysden,&
&                         use_gnuplot,use_cube,plot_title
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
