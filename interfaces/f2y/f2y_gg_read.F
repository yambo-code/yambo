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
 subroutine read_gg(fhi_inp,fhi_gg,filename)
!===============================
 use pars
 use com,                   ONLY : msg
 use f2y_datatypes
 use vec_operate, ONLY : sort
 implicit none
 type(finp_type)::fhi_inp
 type(fgg_type)::fhi_gg

 integer:: ii
 character(schlen)              :: filename
 ! 
 ! Work Space
 !
 open(71,file=filename,form='unformatted',status='unknown')
 read(71) fhi_gg%ngwx,fhi_gg%ngx
 read(71) fhi_gg%nr11,fhi_gg%nr22,fhi_gg%nr33
 call allocate_fgg_ngw(fhi_inp%nkpt,fhi_gg)
 read(71) fhi_gg%ngw
 call allocate_fgg_g(fhi_gg%ngx,fhi_gg)
 read(71) fhi_gg%n1,fhi_gg%n2,fhi_gg%n3
 do ii=1,fhi_gg%ngx
  read(71) fhi_gg%g(ii),fhi_gg%gg(ii,:)
 enddo
 close(71)
 
 end subroutine
