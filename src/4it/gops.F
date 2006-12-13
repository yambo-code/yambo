!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine gops()
!=================
!
! Output: g_rot(nsym,ng_closed)
!
! R_is G_ig = G_{g_rot(is,ig)}
!
 use pars,         ONLY:SP,IP
 use timing,       ONLY:live_timing
 use memory_m,     ONLY:mem_est
 use com,          ONLY:msg
 use D_lattice,    ONLY:nsym,alat,sop_inv
 use R_lattice,    ONLY:n_g_shells,ng_in_shell,ng_closed,&
&                       g_vec,g_rot,E_of_shell,rl_sop
 use vec_operate,  ONLY:iku_v_norm,v_is_zero
 use IO_m,         ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY
 implicit none
!ws
 integer :: i1,i2,i3,i_G_below,i_G_shell,&
&           ng_in_shell_TMP(ng_closed),n_g_shells_no_holes
 real(SP):: v1(3),G_module(2),E_of_shell_TMP(ng_closed)
!
!I/O
 integer           :: ID,io_err
 integer, external :: ioGROT

 call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_err=ioGROT(ID)

 if (io_err==0) then
   return
 else
   if (allocated(g_rot)) then
     deallocate(ng_in_shell,E_of_shell,g_rot)
     call mem_est("RL_Gshells RL_Eshells")
   endif
 endif
!
! Init
!------
 allocate(g_rot(nsym,ng_closed))
 g_rot=0
 ng_in_shell_TMP=0
 E_of_shell_TMP=0.
 G_module=0.
!
! Gamma point
!------------
 g_rot(:,1)=1
 n_g_shells=1
 ng_in_shell_TMP(1)=1
 i_G_shell=1
!
! First the shell with  a preliminar calculation of the
! number of shells only.
!
 call live_timing('RL-shells',ng_closed-1)
!=========================================
!
 do i1=2,ng_closed
   i_G_below=0
   do i2=1,nsym
     v1=matmul(rl_sop(:,:,i2),g_vec(i1,:))
     do i3=2,ng_closed
       if (v_is_zero(v1-g_vec(i3,:))) then
         g_rot(i2,i1)=i3
!
! Number of rotated G a"below" the given G
! 
         if (i3<=i1) i_G_below=i_G_below+1
!
         exit
       endif
     enddo
   enddo
!
   call live_timing(steps=1)
!
! If all the rotated G are "below" the given G
! the shell could be closed
!
   if (i_G_below==nsym) then
!
! Gamma point
!
     if (i1==1) cycle

     G_module(1)=iku_v_norm(g_vec(i_G_shell,:))
     G_module(2)=iku_v_norm(g_vec(i1,:))
!
     if (abs( G_module(2)-G_module(1) )>1.E-5) n_g_shells=n_g_shells+1
!
     i_G_shell=i1
     ng_in_shell_TMP(n_g_shells)=i_G_shell
     E_of_shell_TMP(n_g_shells)=iku_v_norm(g_vec(i_G_shell,:))*2./2.

   endif

 enddo
 call live_timing()
!
!Check for holes
!
 n_g_shells_no_holes=n_g_shells
 do i1=n_g_shells,2,-1
 
   if ( all(g_rot(:,:ng_in_shell_TMP(i1))/=0) ) then
     n_g_shells_no_holes=i1
     exit 
   endif
 
 enddo
 n_g_shells=n_g_shells_no_holes
!
! Shells Allocation & Transfer 
!==============================
!
 allocate(ng_in_shell(n_g_shells),E_of_shell(n_g_shells))
 call mem_est("RL_Gshells RL_Eshells",&
&             (/n_g_shells+nsym*ng_closed,n_g_shells/),(/IP,SP/))

 ng_in_shell=ng_in_shell_TMP(:n_g_shells)
 E_of_shell=E_of_shell_TMP(:n_g_shells)
!
 ng_closed=ng_in_shell(n_g_shells)
!
!I/O
!===
 call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID)
 io_err=ioGROT(ID)
!
 end subroutine
!
