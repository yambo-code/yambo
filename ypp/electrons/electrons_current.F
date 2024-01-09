! 
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine electrons_current(Xk,Xen)
 !
 use pars,           ONLY:SP,lchlen
 use units,          ONLY:AU2nAmpere
 Use stderr,         ONLY:intc
 use FFT_m,          ONLY:fft_size
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use YPPm,           ONLY:plot_title,current_dir,plot_dim,output_fname,use_gnuplot,&
&                         use_cube,use_xcrysden,plot_is_1D,v2plot
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
 real(SP)              :: el_cur(fft_size,3)
 character(lchlen)     :: ch_ws(2)
 !
 call section('*','Current')
 !
 call error(' Implementation to be finalized. Presently it is not working.')
 !
 call section('*','Current Plot')
 !
 call el_density_and_current(Xen,Xk,J=el_cur)
 !
 v2plot=v2plot/abs(maxval( v2plot ))
 !
 call msg("s",'Current integrals (x,y,z)',&
&             (/Sum(el_cur(:,1)),Sum(el_cur(:,2)),Sum(el_cur(:,3))/)*AU2nAmpere,&
&             '[nAmpere]')
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
&          el_cur(:,3)*current_dir(3))*AU2nAmpere
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
end subroutine
