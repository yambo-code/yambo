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
function com_compose_msg(pre,body,post)
 !
 use pars,       ONLY:msg_len
 !
 implicit none
 !
 character(msg_len)                  :: com_compose_msg
 character(*), intent(in)            :: body
 character(*), intent(in), optional  :: pre,post
 !
 ! Work Space
 !
 integer :: body_start,post_start,i_c,body_len,post_len,pre_len
 !
 pre_len=0
 if (present(pre)) then
   pre_len=len(pre)
 endif
 !
 body_start=0
 do i_c=1,len(body)
   if (body(i_c:i_c)/=" ") body_start=i_c
   if (body_start>0) exit
 enddo
 body_len=len_trim(body)
 !
 post_start=0
 post_len=0
 if (present(post)) then
   do i_c=1,len(post)
     if (post(i_c:i_c)/=" ") post_start=i_c
     if (post_start>0) exit
   enddo
   post_len=len_trim(post)
 endif
 !
 com_compose_msg=" "
 if (present(pre)) then
   com_compose_msg(1:pre_len)=pre
 endif
 if (body_start==0) return
 com_compose_msg(pre_len+1:pre_len+body_len-body_start+1)=trim(body(body_start:))
 if (post_start==0) return
 com_compose_msg(msg_len-post_len+post_start:msg_len)=trim(post(post_start:))
 !
end function
