!
! Copyright (C) 2000-2012 D. Sangalli and the YAMBO team 
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
subroutine KERR_shadow(what,l_flag,n1,n2,c1)
 ! 
 use pars,          ONLY:SP
 use parser_m,      ONLY:parser
 use units,         ONLY:HA2EV
 use X_m,           ONLY:X_epsilon
 use X_output,      ONLY:off_file_name,kerr_file_name,X_setup_headers,headers
 use drivers,       ONLY:l_kerr,l_optics,l_bse,l_bss
 use KERR,          ONLY:KERR_alloc,chi_nn,chi_jj
 use com,           ONLY:msg,of_open_close,warning,error
 use memory_m,      ONLY:mem_est
 !
 implicit none
 !
 character(*)     :: what    
 !
 integer,optional :: n1,n2
 complex(SP),optional :: c1(:)
 logical,optional :: l_flag
 !
 real(SP)         :: output_data(5)
 !
 select case(what)
   !
   case('setup')
     call parser('EvalKerr',l_kerr)
     if(present(l_flag)) l_flag=.not.l_kerr
     !
     if ((l_optics.and.l_bse).or.l_bss) then
       !call eval_minus_G()
       call parser('Xjj',chi_jj)
       chi_nn=.not.chi_jj
     endif
     !
   case('def_files')
     ! File names are defined in mod_X_output
     ! If .not. l_kerr I need to destroy the names
     if (.not.l_kerr) then
       off_file_name=' '
       kerr_file_name=' '
     endif
     !
 end select
 !
 if(.not.l_kerr) return
 !
 select case(what)
   !
   case('X_alloc')
     call Kerr_alloc('X',(/n1,n2/))
     !
   case('X_dealloc')
     call Kerr_alloc('X')
     !
   case('Dip_dealloc')
     ! Allocation is done by Dipole_dip_dot_field
     call Kerr_alloc('DIP_q_dot_iR')
     !
   case('def_files')
     call of_open_close(off_file_name,'ot')
     call of_open_close(kerr_file_name,'ot')
     !
   case('open_files')
     call of_open_close(off_file_name,'oa')
     call of_open_close(kerr_file_name,'oa')
     !
   case('files_header')
     call msg('o off kerr','#')
     call X_setup_headers('off')
     if (.not.l_flag) call msg('o off','#',headers(:5),INDENT=0,USE_TABS=.true.)    
     if (l_flag)      call msg('o off','#',headers(:3),INDENT=0,USE_TABS=.true.)    
     call X_setup_headers('kerr')
     if (.not.l_flag) call msg('o kerr','#',headers(:5),INDENT=0,USE_TABS=.true.)    
     if (l_flag)      call msg('o kerr','#',headers(:3),INDENT=0,USE_TABS=.true.)    
     call msg('o off kerr','#')
     !
   case('write_data')
     ! epsilon_xy
     output_data=(/real(c1(n1))*HA2EV,aimag(X_epsilon(4+n2,n1)),real(X_epsilon(4+n2,n1)),&
&                  aimag(X_epsilon(3+n2,n1)),real(X_epsilon(3+n2,n1))/)
     if (l_flag)      call msg('o off','',output_data(:3),INDENT=-2,USE_TABS=.true.)
     if (.not.l_flag) call msg('o off','',output_data,INDENT=-2,USE_TABS=.true.)
     ! kerr_parameters
     output_data=(/real(c1(n1))*HA2EV,real(X_epsilon(6+n2,n1)),aimag(X_epsilon(6+n2,n1)),&
&                  real(X_epsilon(5+n2,n1)),aimag(X_epsilon(5+n2,n1))/)
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
