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
subroutine k_ibz2bz(k,units,FORCE_BZ)
 !
 ! Output
 !
 !  k%ptbz
 !
 use pars,           ONLY:SP,schlen
 use vec_operate,    ONLY:c2a,k2bz
 use R_lattice,      ONLY:rl_sop,bz_samp
 !
#include<memory.h>
 !
 type(bz_samp)::k
 character(1) ::units
 logical      ::FORCE_BZ
 !
 ! Work Space  
 !
 real(SP):: k_bz(3)
 integer :: i1
 character(schlen)::ch
 !
 write (ch,'(2a)') trim(k%description),"-ptbz"
 !
 ! Clean
 !
 if (units=="d") then
   YAMBO_FREE(k%ptbz)
   k%units=' '
   return
 endif
 !
 if (allocated(k%ptbz).and.k%units==units) return
 !
 ! First the stars...
 !
 if (.not.allocated(k%sstar)) call k_expand(k) 
 !
 ! ...then the pts
 !
 YAMBO_FREE(k%ptbz)
 YAMBO_ALLOC(k%ptbz,(k%nbz,3))
 !
 k%units=units
 !
 do i1=1,k%nbz
   !
   ! k_bz is in iku
   !
   k_bz=matmul(rl_sop(:,:,k%sstar(i1,2)),k%pt(k%sstar(i1,1),:)) 
   !
   ! Shift to the BZ
   !
   if (FORCE_BZ) call k2bz(k_bz)
   !
   if (units=='a') call c2a(v_in=k_bz,v_out=k%ptbz(i1,:),mode='ki2a')
   if (units=='i') k%ptbz(i1,:)=k_bz
   if (units=='c') call c2a(v_in=k_bz,v_out=k%ptbz(i1,:),mode='ki2c')
   !
 enddo
 !
end subroutine
