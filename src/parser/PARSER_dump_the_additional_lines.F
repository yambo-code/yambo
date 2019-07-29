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
subroutine PARSER_dump_the_additional_lines(ou)
 !
 use it_m,       ONLY:infile,nADDITIONAL_flines,ADDITIONAL_flines
 use drivers,    ONLY:infile_editing
 use parallel_m, ONLY:master_cpu
 !
 implicit none
 !
 integer :: ou
 !
 ! Work Space
 !
 integer :: i1
 !
 if (infile_editing.and.ou==12) open(unit=ou,file=trim(infile),position='append')
 do i1=1,nADDITIONAL_flines
   if (master_cpu) then
      if (ou==12) write (ou,'(a)') trim(ADDITIONAL_flines(i1))
      if (ou==11) write (ou,'(2a)') ' | ',trim(ADDITIONAL_flines(i1))
    endif
    if (ou/=11.and.ou/=12) write (ou,'(2a)') '# | ',trim(ADDITIONAL_flines(i1))
 enddo 
 if (infile_editing) close(ou)
 !
end subroutine
