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
subroutine electrons_current(Xk,Xen)
 !
 use pars,           ONLY:SP
 use YPP,            ONLY:mag_dir,v2plot,plot_dim,use_gnuplot,use_xcrysden,output_fname, &
&                         plot_title
 use com,            ONLY:msg,of_open_close
 use stderr,         ONLY:intc
 use electrons,      ONLY:n_sp_pol,levels
 use R_lattice,      ONLY:bz_samp
 use FFT_m,          ONLY:fft_size
 use magnetic,       ONLY:x,load_cc,free_cc
 use timing,         ONLY:live_timing
 use wave_func,      ONLY:WF_load,WF_free,WF_derivative_free
 !
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 real(SP)     :: j_current(3,n_sp_pol,fft_size),r(fft_size)
 real(SP)     :: average_current(3),psi_current
 integer      :: current_i_dir,i_dir,i_spin,ifft,ik
 character(1) :: current_dir
 character(4) :: current_spin
 logical      :: plot,cartesian,cylindrical
 !
 cartesian=.true.
 cylindrical=.true.
 !
 call section('*','Single Particle Current')
 !
 j_current=0._SP
 call live_timing('Evaluate current',Xk%nibz)
 do ik=1,Xk%nibz
   !
   call WF_derivative((/1,Xen%nbm/),(/ik,ik/),(/.true.,.true.,.true./))
   call WF_load(0,1,(/1,Xen%nbm/),(/ik,ik/),title='-WF',impose_free_and_alloc=.TRUE.)
   !
   call el_current(Xen,Xk,j_current,ik)
   call live_timing(steps=1)
   !
   call WF_derivative_free()
   if(ik/=Xk%nibz) call WF_free()
   !
 enddo
 call live_timing()
 !
 average_current=0.
 psi_current=0.
 !
 if (cartesian) then
   do i_dir=1,3
     do i_spin=1,n_sp_pol
       do ifft=1,fft_size
         average_current(i_dir)=average_current(i_dir)+j_current(i_dir,i_spin,ifft)
       enddo
     enddo
   enddo
 endif
 !
 if (cylindrical) then
   call load_cc(.true.)
   r(:)=sqrt( x(:,1)**2+x(:,2)**2 )
   do i_spin=1,n_sp_pol
     do ifft=1,fft_size
         psi_current=psi_current+j_current(2,i_spin,ifft)*(x(ifft,1)/r(ifft)) &
&                               -j_current(1,i_spin,ifft)*(x(ifft,2)/r(ifft))
     enddo
   enddo
   call free_cc()
 endif
 !
 output_fname='current'
 call of_open_close(trim(output_fname),'ot')
 call msg('o cur',"#",'average current x , y , z')
 call msg('o cur',"#")
 call msg('o cur','',average_current)
 call msg('o cur',"#",'psi_current')
 call msg('o cur',"#")
 call msg('o cur','',(/psi_current/))
 call of_open_close(trim(output_fname))
 !
 ! This is not used presently, only for future runs...
 !
 plot=.false.
 if (.not.plot) return
 ! 
 current_i_dir=-1
 if (current_dir=='X'.or.current_dir=='x') current_i_dir=1
 if (current_dir=='Y'.or.current_dir=='y') current_i_dir=2
 if (current_dir=='Z'.or.current_dir=='z') current_i_dir=3
 if (current_i_dir<0) return
 !
 if (n_sp_pol>1) then
   if(current_spin=='up  ') v2plot=j_current(current_i_dir,1,:)
   if(current_spin=='down') v2plot=j_current(current_i_dir,2,:)
   if(current_spin=='tot ') v2plot=j_current(current_i_dir,1,:)+j_current(current_i_dir,2,:)
 else
   v2plot=j_current(current_i_dir,1,:)
 endif
 !
 if (use_xcrysden) output_fname='mag_'//trim(current_dir)//'_'//trim(intc(plot_dim))//'d.xsf'
 if (use_gnuplot)  output_fname='mag_'//trim(current_dir)//'_'//trim(intc(plot_dim))//'d'
 !
 call of_open_close(trim(output_fname),'ot')
 call msg('o current',"#")
 !
 plot_title='current'
 !
 call plot_check_and_launch(.false.)
 !
 call of_open_close(trim(output_fname))
 !
end subroutine
