!
! Copyright (C) 2000-2010 D. Sangalli and the YAMBO team 
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
subroutine O_kerr_shadow(what,l_flag,n1,n2,c1)
 ! 
 use pars,          ONLY:SP
 use parser_m,      ONLY:parser
 use units,         ONLY:HA2EV
 use X_m,           ONLY:X_epsilon
 use X_output,      ONLY:off_file_name,kerr_file_name,X_setup_headers,headers
 use drivers,       ONLY:l_kerr
 use electrons,     ONLY:spin_orbit
 use KERR,          ONLY:KERR_alloc
 use com,           ONLY:msg,of_open_close,warning,error
 !
 implicit none
 !
 character(*)     :: what    
 !
 integer,optional :: n1,n2
 complex,optional :: c1(:)
 logical,optional :: l_flag
 !
 real(SP)         :: output_data(5)
 !
 select case(what)
   !
   case('setup')
     call parser('EvalKerr',l_kerr)
     l_flag=.not.l_kerr
     if(l_kerr) then
       ! The spin_orbit variable is presently readed from abinit in a2y
       ! However it is not written in any database
       call warning('Assuming spin_orbit=.true.')
       spin_orbit=.true.
     endif
     !
   case('X_alloc')
     if(l_kerr) call Kerr_alloc('X',(/n1,n2/))
     !
   case('X_dealloc')
     if(l_kerr) call Kerr_alloc('X')
     !
   case('Dip_dealloc')
     ! Allocation is done by Dipole_dip_dot_field
     if(l_kerr) call Kerr_alloc('DIP_q_dot_iR')
     !
   case('def_files')
     ! File names are defined in mod_X_output
     ! If .not. l_kerr I need to destroy the names
     if (.not.l_kerr) then
       off_file_name=' '
       kerr_file_name=' '
     endif
     call of_open_close(off_file_name,'ot')
     call of_open_close(kerr_file_name,'ot')
     !
   case('open_files')
     call of_open_close(off_file_name,'oa')
     call of_open_close(kerr_file_name,'oa')
     !
   case('files_header')
     call msg('o off kerr','#')
!     call X_setup_headers('off')
!     if (.not.l_flag) call msg('o off','#',headers(:5),INDENT=0,USE_TABS=.true.)    
!     if (l_flag)      call msg('o off','#',headers(:3),INDENT=0,USE_TABS=.true.)    
!     call X_setup_headers('kerr')
!     if (.not.l_flag) call msg('o kerr','#',headers(:5),INDENT=0,USE_TABS=.true.)    
!     if (l_flag)      call msg('o kerr','#',headers(:3),INDENT=0,USE_TABS=.true.)    
     call msg('o off kerr','#')
     !
   case('write_data')
     ! epsilon_xy
     output_data=(/real(c1(n1))*HA2EV,aimag(X_epsilon(3,n1)),real(X_epsilon(4,n1)),&
&                  aimag(X_epsilon(3,n1)),real(X_epsilon(4,n1))/)
     if (l_flag)      call msg('o off','',output_data(:3),INDENT=-2,USE_TABS=.true.)
     if (.not.l_flag) call msg('o off','',output_data,INDENT=-2,USE_TABS=.true.)
     ! kerr_parameters
     output_data=(/real(c1(n1))*HA2EV,aimag(X_epsilon(5,n1)),real(X_epsilon(6,n1)),&
&                  aimag(X_epsilon(5,n1)),real(X_epsilon(6,n1))/)
     if (l_flag)      call msg('o kerr','',output_data(:3),INDENT=-2,USE_TABS=.true.)
     if (.not.l_flag) call msg('o kerr','',output_data,INDENT=-2,USE_TABS=.true.)
     !
   case('close_files')
     call of_open_close(off_file_name)
     call of_open_close(kerr_file_name)
     !
 end select
 !
end subroutine 
