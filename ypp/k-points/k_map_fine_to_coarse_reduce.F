!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
logical function k_map_fine_to_coarse_reduce(FineGd_k)
 !
 use zeros,               ONLY:k_rlu_zero
 use pars,                ONLY:SP
 use vec_operate,         ONLY:c2a,rlu_v_is_zero
 use R_lattice,           ONLY:bz_samp
 !
#include<memory.h>
 !
 type(bz_samp), intent(inout)      :: FineGd_k
 !
 ! Work Space
 !
 integer :: ik,ip,unique(FineGd_k%nbz)
 real(SP):: v(3)
 !
 unique=1
 !
 do ik=1,FineGd_k%nbz
   if (unique(ik)==0) cycle
   do ip=ik+1,FineGd_k%nbz
     if (unique(ip)==0) cycle
     v=FineGd_k%ptbz(ik,:)-FineGd_k%ptbz(ip,:)
     call c2a(v_in=v,mode='ki2a')
     if (rlu_v_is_zero(v,zero_=k_rlu_zero)) unique(ip)=0
   enddo
 enddo
 ! 
 k_map_fine_to_coarse_reduce=count(unique==1)==FineGd_k%nbz
 ! 
end function
