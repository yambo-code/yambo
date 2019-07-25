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
subroutine INIT_DephMatrix()
 !
 ! Dephasing Matrix -> input file
 !
 use drivers,        ONLY: infile_editing
 use it_m,           ONLY: nflines,flines,infile
 use parser_m,       ONLY: parser  
 implicit none
 !
 ! 
 ! Work Space
 !
 integer :: i1,nflines_save
 logical :: l_lines_in_input
 character(17) :: comment
 !
 call parser('DephMatrix',l_lines_in_input)
 !
 !If there are already the QP lines or there's not the input file todo return
 !
 if (l_lines_in_input.and..not.infile_editing) return 
 !
 comment="[RT] Dephasing Matrix"
 !
 nflines_save=nflines
 write (flines(nflines+1),'(a,t30,a)') '%DephMatrix',&
&      '# '//trim(comment)//' (bnd1 | bnd2 | kpt | dephasing(fs) |)'
 write (flines(nflines+2),'(3(i3,a),1(f4.1,a))') 1,'|',1,'|',1,'|',-1.,'|'
 flines(nflines+3)='%'
 nflines=nflines+3
 !
 ! Write to input file
 !
 open(unit=12,file=trim(infile),position='append')
 do i1=nflines_save+1,nflines
   write (12,'(a)') trim(flines(i1))
 enddo 
 close(12)
 !
end subroutine
