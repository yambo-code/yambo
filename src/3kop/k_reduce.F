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
 subroutine k_reduce(k)
!======================
!
! Input:
!
!  k%ptbz (iku)
!
! Output:
!
!  k%pt k%nibz
!
 use pars,           ONLY:SP
 use vec_operate,    ONLY:c2a,rlu_v_is_zero,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:b,rl_sop,bz_samp
 implicit none
 type(bz_samp)::k
! ws
 integer :: i1,i2,is
 real(SP):: v1(3),v2(3),v3(3),kibz(k%nbz,3)

 k%nibz=0
 kloop: do i1=1,k%nbz
   call k2bz(k%ptbz(i1,:),v1,b)
   call c2a(v_in=v1,v_out=v2,mode='ki2a')
   do i2=1,k%nibz
     do is=1,nsym
       call c2a(b,matmul(rl_sop(:,:,is),kibz(i2,:)),v3,'ki2a') 
       if (rlu_v_is_zero(v2-v3)) cycle kloop
     enddo
   enddo
   k%nibz=k%nibz+1
   kibz(k%nibz,:)=v1
 enddo kloop

 allocate(k%pt(k%nibz,3))
 k%pt(:k%nibz,:)=kibz(:k%nibz,:)

 end subroutine

