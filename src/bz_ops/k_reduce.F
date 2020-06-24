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
subroutine k_reduce(k,FORCE_BZ)
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
 use LIVE_t,         ONLY:live_timing
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:b,rl_sop,bz_samp
 use zeros,          ONLY:k_rlu_zero
#include<memory.h>
 type(bz_samp)::k
 logical      ::FORCE_BZ
 !
 ! Work Space
 !
 integer :: i1,i2,i2_ref,is
 real(SP):: v1(3),k_ibz(k%nbz,3),k_bz_shifted(3)
 !
 call live_timing('BZ -> IBZ reduction',k%nbz)
 !
 k%nibz=0
 i2_ref=0
 !
 kloop: do i1=1,k%nbz
   !
   call live_timing(steps=1)
   !
   call k2bz(k%ptbz(i1,:),v1,b)
   call c2a(v_in=v1,v_out=k_bz_shifted,mode='ki2a')
   !
   if (i2_ref/=0) then
     do is=1,nsym
       call c2a(b,matmul(rl_sop(:,:,is),k_ibz(i2_ref,:)),v1,'ki2a')
       if (rlu_v_is_zero(k_bz_shifted-v1,zero_=k_rlu_zero)) cycle kloop
     enddo
   endif
   !
   do i2=1,k%nibz
     i2_ref=i2
     do is=1,nsym
       call c2a(b,matmul(rl_sop(:,:,is),k_ibz(i2,:)),v1,'ki2a') 
       if (rlu_v_is_zero(k_bz_shifted-v1,zero_=k_rlu_zero)) cycle kloop
     enddo
   enddo
   !
   k%nibz=k%nibz+1
   if(     FORCE_BZ) call k2bz(k%ptbz(i1,:),k_ibz(k%nibz,:),b)
   if(.not.FORCE_BZ) k_ibz(k%nibz,:)=k%ptbz(i1,:)
   !
 enddo kloop
 !
 call live_timing()
 !
 YAMBO_ALLOC(k%pt,(k%nibz,3))
 k%pt(:k%nibz,:)=k_ibz(:k%nibz,:)
 !
end subroutine
