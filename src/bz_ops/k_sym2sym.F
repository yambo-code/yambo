!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine k_sym2sym(k,mode)
 !
 ! Output:
 !
 !  ik_is_table such that
 !
 !  R k_ibz = S k_ibz with S=ik_is_table(k,R)     if mode='s'
 !  R k_ibz = k_bz    with k_bz=ik_is_table(k,R)  if mode='k'
 !
 use pars,           ONLY:SP,IP
 use vec_operate,    ONLY:c2a,rlu_v_is_zero
 use D_lattice,      ONLY:nsym
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:rl_sop,bz_samp,ik_is_table
 use zeros,          ONLY:k_rlu_zero
 !
 implicit none
 type(bz_samp)::k
 character(1) ::mode
 !
 ! Work Space
 !
 integer      ::i1,i2,is1
 real(SP)     ::v(3)
 !
 if (allocated(ik_is_table)) then
   if (size(ik_is_table)/=k%nibz*nsym) then
     deallocate(ik_is_table)
     call mem_est("ik_is_table")
   else
     return
   endif
 endif
 !
 allocate(ik_is_table(k%nibz,nsym))
 call mem_est("ik_is_table",(/size(ik_is_table)/),(/IP/))
 !
 call k_ibz2bz(k,'a',.false.)
 ik_is_table=0
 do i1=1,k%nibz
   do is1=1,nsym
     call c2a(v_in=matmul(rl_sop(:,:,is1),k%pt(i1,:)),v_out=v,mode='ki2a')
     do i2=1,k%nbz
       if (rlu_v_is_zero(v-k%ptbz(i2,:),zero_=k_rlu_zero)) then
         if (mode=='s') ik_is_table(i1,is1)=k%sstar(i2,2)
         if (mode=='k') ik_is_table(i1,is1)=i2
         exit
       endif
     enddo
   enddo
 enddo
 !
 ! CLEAN
 !
 call k_ibz2bz(k,'d',.false.)
 !
end subroutine
