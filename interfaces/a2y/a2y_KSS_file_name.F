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
integer function a2y_KSS_file_name(inf,KSS_file_name)
 !
 use pars,             ONLY:lchlen,schlen
 use com,              ONLY:file_exists 
 implicit none
 character(*)              :: inf
 character(lchlen)         :: KSS_file_name
 !
 ! Work Space
 !
 integer                        :: i1
 character(schlen)              :: prefix
 character(2)                   :: inum
 !
 a2y_KSS_file_name=0
 !
 KSS_file_name=inf
 if (file_exists (KSS_file_name) ) return
 !
 prefix=inf
 !
 write (KSS_file_name,'(2a)') trim(prefix),'_KSS'
 if (file_exists(KSS_file_name)) return
 !
 write (KSS_file_name,'(2a)') trim(prefix),'_out_KSS'
 if (file_exists(KSS_file_name)) return
 !
 write (KSS_file_name,'(2a)') trim(prefix),'.KSS'
 if (file_exists(KSS_file_name)) return
 !
 write (KSS_file_name,'(2a)') trim(prefix),'.out_KSS'
 if (file_exists(KSS_file_name)) return
 !
 do i1=1,99
   if (i1<10) write (inum,'(i1)') i1
   if (i1>=10) write (inum,'(i2)') i1
   !
   write (KSS_file_name,'(4a)') trim(prefix),'_DS',trim(inum),'_KSS'
   if (file_exists(KSS_file_name)) return
   !
   write (KSS_file_name,'(4a)') trim(prefix),'_out_DS',trim(inum),'_KSS'
   if (file_exists(KSS_file_name)) return
 enddo
 !
 a2y_KSS_file_name=-1
 !
end function
