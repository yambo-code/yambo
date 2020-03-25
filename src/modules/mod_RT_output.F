! 
!        Copyright (C) 2000-2020 the YAMBO team
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
module RT_output_m
 !
 use drivers,    ONLY:l_elel_scatt,l_elph_scatt,l_elphoton_scatt
 use pars,       ONLY:DP,SP,schlen,lchlen,LP
 use units,      ONLY:FS2AUT
 use global_XC,  ONLY:QP_SE_NONE
 use descriptors,ONLY:IO_desc
 !
#include<memory.h>
 !
 integer, parameter       :: N_MAX_RT_o_files=100
 integer, parameter       :: N_MAX_RT_X_order=9
 integer, parameter       :: N_MAX_RT_headings=20
 !
 integer                  :: N_RT_o_files
 integer                  :: n_RT_headings(N_MAX_RT_o_files)
 character(lchlen)        :: RT_o_file_long(N_MAX_RT_o_files)
 character(schlen)        :: RT_o_file_short(N_MAX_RT_o_files)
 character(schlen)        :: RT_headings(N_MAX_RT_o_files,N_MAX_RT_headings)
 character(schlen)        :: YPP_append
 !
 logical                  :: RT_descriptions_dumped(N_MAX_RT_o_files)
 logical                  :: RT_o_file_is_open(N_MAX_RT_o_files)
 !
 type(IO_desc)            :: RT_desc
 type(IO_desc)            :: TIME_STEP_desc
 !
 contains
   !
   subroutine RT_FILE_add(name,TITLES)
     !
     character(*) , OPTIONAL :: name
     character(*) , OPTIONAL :: TITLES(:)
     !
     integer :: i_t,N
     !
     if (present(name)) then
       N_RT_o_files=N_RT_o_files+1
       RT_o_file_short(N_RT_o_files)=trim(YPP_append)//trim(name)
       RT_o_file_long(N_RT_o_files) =RT_o_file_short(N_RT_o_files)
     endif
     N=size(TITLES)
     do i_t= 1,N
       RT_headings(N_RT_o_files,n_RT_headings(N_RT_o_files)+i_t)=trim(TITLES(i_t))
     enddo
     n_RT_headings(N_RT_o_files)=n_RT_headings(N_RT_o_files)+N
     !
   end subroutine
   !
end module
