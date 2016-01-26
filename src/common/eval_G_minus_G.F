!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use vec_operate,  ONLY:iku_v_norm
 use LIVE_t,       ONLY:live_timing
 use R_lattice,    ONLY:G_m_G,g_vec,ng_in_shell,n_g_shells,minus_G,E_of_shell
 !
 implicit none
 !
 integer :: iG,iGo
 !
 ! Work Space
 !
 integer :: i1,i2
 integer :: iG_shell,iG_,iGo_
 real(SP):: E_of_shell_G_m_G
 real(SP):: v1(3)
 !
 integer, external :: G_index
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
1 allocate(G_m_G(iG_,iGo_))
 call mem_est("G_m_G",(/shape(G_m_G)/),(/IP/))
 !
 G_m_G=0
 !
 ! Find the shell corresponding to iG
 !
 do i1=1,n_g_shells
   if (ng_in_shell(i1)==iG_) then
     iG_shell=i1
     exit
   endif
   if (ng_in_shell(i1)> iG_) then
     iG_shell=i1-1
     iG_=ng_in_shell(iG_shell)
     if (iGo==0) iGo_=iG_
     exit
   endif
 enddo
 !
 forall(i1=1:iG_) G_m_G(i1,1)=i1
 if(iGo_>1) G_m_G(1,2:iGo_)=minus_G(2:iGo_)
 !
 do i1=iG_,2,-1
   do i2=iGo_,2,-1
     !
     v1=g_vec(i1,:)-g_vec(i2,:)
     E_of_shell_G_m_G=iku_v_norm(v1)**2/2._SP
     !
     if(E_of_shell_G_m_G/E_of_shell(n_g_shells)>(1+1.E-5_SP)) then 
       iG_shell=iG_shell-1
       iG_=ng_in_shell(iG_shell)
       eval_G_minus_G=iG_
       if (iGo==0) iGo_=iG_
       deallocate(G_m_G)
       goto 1
     endif
     !
     G_m_G(i1,i2)=G_index(v1)
     !
   enddo
 enddo 
 !
end function
