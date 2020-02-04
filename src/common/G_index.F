!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
integer function G_index(v1,accept_fail)
 !
 use pars,         ONLY:SP
 use zeros,        ONLY:G_iku_zero
 use vec_operate,  ONLY:v_is_zero,iku_v_norm
 use R_lattice,    ONLY:g_vec,ng_in_shell,n_g_shells,E_of_shell
 !
 implicit none
 !
 real(SP), intent(in)  :: v1(3)
 logical,  intent(in)  :: accept_fail
 !
 integer   :: ig,i_shell,ng1,ng2
 real(SP)  :: E_of_shell_v1
 !
 G_index=0
 !
 E_of_shell_v1=iku_v_norm(v1)**2/2._SP
 !
 if(abs(E_of_shell_v1)<1.E-5_SP) then
   G_index=1
   return
 endif
 !
 ng1=1
 ng2=ng_in_shell(n_g_shells)
 !
 do i_shell=2,n_g_shells
   if(abs(E_of_shell(i_shell)-E_of_shell_v1)<1.E-4_SP) then
     ng1=ng_in_shell(i_shell-1)
     exit
   endif
 enddo
 !
 do i_shell=n_g_shells-1,2,-1
   if(abs(E_of_shell(i_shell)-E_of_shell_v1)<1.E-4_SP) then
     ng2=ng_in_shell(minval((/i_shell+2,n_g_shells/)))
     exit
   endif
 enddo
 !
 do ig=ng1,ng2
   if (v_is_zero(v1-g_vec(ig,:),zero_=G_iku_zero)) then
     G_index=ig
     exit
   endif
 enddo
 !
 if(G_index==0) then
   if(.not.accept_fail) call error(" Error search for G=G1-G2 !!")
   if(     accept_fail) call warning(" Error search for G=G1-G2 !!")
 endif
 !
 return
 !
end function
