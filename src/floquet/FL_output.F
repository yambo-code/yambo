!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_output
 !
 use pars,        ONLY:SP,lchlen,schlen
 use units,       ONLY:HA2EV,SVCMm12VMm1,AU2VMm1
 use stderr,      ONLY:intc
 use com,         ONLY:msg,of_open_close
 use interfaces,  ONLY:DESC_write
 use nl_optics,   ONLY:n_frequencies,NL_er,NL_desc,NL_estep
 use fl_optics,   ONLY:max_fl_mode,n_tot_fl_modes,FL_Chi_pol,FL_Chi_order
 !
 implicit none
 !
 ! Work space
 !
 integer           :: i_f,i_order,n_headings=7
 character(22)     :: Chi_filename
 character(2)      :: title_mode
 real(SP)          :: w_probe,Unit_of_Measure
 character(schlen) :: headings(11)
 real(SP)          :: Chi_plot(7)
 !
 title_mode='ot'
 !
 headings(1)=" E [eV]"
 headings(2:7)=(/'X/Im[cm/stV](x)','X/Re[cm/stV](x)','X/Im[cm/stV](y)','X/Re[cm/stV](y)',&
&                            'X/Im[cm/stV](z)','X/Re[cm/stV](z)'/)
 !
 do i_order=0,FL_Chi_order
   !
   Chi_filename = 'chi_q1_floquet_order_'//trim(intc(i_order))
   !
   call of_open_close(Chi_filename,title_mode)
   !
   call DESC_write('o floquet','#',NL_desc) !aim137 what is this string?
   !
   call msg('o floquet','#')
   call msg('o floquet','#',headings(1:n_headings),INDENT=0,USE_TABS=.TRUE.)
   call msg('o floquet','#')
   !
   Unit_of_Measure = 1._SP
   !
   if (i_order==0) Unit_of_Measure =  SVCMm12VMm1/AU2VMm1
   if (i_order> 1) Unit_of_Measure = (SVCMm12VMm1/AU2VMm1)**(i_order-1)
   !
   do i_f=1,n_frequencies
     !
     w_probe=(NL_er(1)+real(i_f-1,SP)*NL_estep)
     !
     Chi_plot(1)  =w_probe*HA2EV
     Chi_plot(2:7)=(/aimag(FL_Chi_pol(i_f,1,i_order+1,1)*Unit_of_Measure),&
&                     real(FL_Chi_pol(i_f,1,i_order+1,1)*Unit_of_Measure),&
&                    aimag(FL_Chi_pol(i_f,2,i_order+1,1)*Unit_of_Measure),&
&                     real(FL_Chi_pol(i_f,2,i_order+1,1)*Unit_of_Measure),&
&                    aimag(FL_Chi_pol(i_f,3,i_order+1,1)*Unit_of_Measure),&
&                     real(FL_Chi_pol(i_f,3,i_order+1,1)*Unit_of_Measure)/)
     !
     call msg('o floquet','',Chi_plot(1:n_headings),USE_TABS=.TRUE.)
     !
   enddo
   !  
   call of_open_close(Chi_filename)
   !
   !
 enddo
 !
end subroutine
