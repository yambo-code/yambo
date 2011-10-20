!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
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
integer function eval_G_minus_G(iG,iGo)
 !
 ! Evaluates the  G-G' table :
 !
 !  g_vec( G_m_G(i,j) ) = g_vec(i)-g_vec(j)
 !
 ! and returns the orginal iG that is redefined in output
 ! in such a way that G_m_G(iG,j) exists for all j
 !
 use pars,         ONLY:SP,IP
 use memory_m,     ONLY:mem_est
 use vec_operate,  ONLY:v_is_zero
 use R_lattice,    ONLY:G_m_G,g_vec,ng_closed,ng_in_shell,n_g_shells
 use zeros,        ONLY:G_iku_zero
 !
 implicit none
 integer :: iG,iGo
 !
 ! Work Space
 !
 integer :: i1,i2,i3,iG_shell,iG_,iGo_
 real(SP):: v1(3)
 !
 ! Init
 !
 if (allocated(G_m_G)) then
   eval_G_minus_G=size(G_m_G,1)
   return
 endif
 !
 eval_G_minus_G=iG
 iG_=iG
 iGo_=iGo
 if (iGo==0) iGo_=iG
 !
 ! Allocation
 !
 allocate(G_m_G(iG_,iGo_))
 call mem_est("G_m_G",(/shape(G_m_G)/),(/IP/))
 !
 ! Loop
 !
1 G_m_G=0
 !
 ! Find the shell corresponding to iG
 !
 do i1=1,n_g_shells
   if (ng_in_shell(i1)==iG_) iG_shell=i1
 enddo
 !
 do i1=1,iG_
   do i2=1,iGo_
     v1=g_vec(i1,:)-g_vec(i2,:)
     do i3=1,ng_closed
       if (v_is_zero(v1-g_vec(i3,:),zero_=G_iku_zero)) then
         G_m_G(i1,i2)=i3
         exit
       endif
     enddo
     if (G_m_G(i1,i2)==0) then
       iG_shell=iG_shell-1
       iG_=ng_in_shell(iG_shell)
       eval_G_minus_G=iG_
       if (iGo==0) iGo_=iG_
       deallocate(G_m_G)
       allocate(G_m_G(iG_,iGo_))
       call mem_est("G_m_G",(/shape(G_m_G)/),(/IP/))
       goto 1
     endif
   enddo
 enddo 
 !
end function
