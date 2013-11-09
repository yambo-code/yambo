!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine COLLISIONS_alloc_and_free(what,elements,actions,I)
 !
 use pars,           ONLY:LP,cZERO
 use memory_m,       ONLY:mem_est
 use QP_m,           ONLY:QP_n_states
 use SC,             ONLY:SC_bands
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:nqbz,nkibz
 use parallel_m,     ONLY:PAR_nQPp,PAR_nQ,PAR_nQP
 use global_XC,      ONLY:SC_HF,SC_COHSEX,SE_GW_NEQ
 use collision,      ONLY:COLLISIONS_element,HF_collisions,COHSEX_collisions,GW_NEQ_collisions,&
&                         nHF_collisions,nCOHSEX_collisions,nGW_NEQ_collisions
 !
 implicit none
 !
 character(*)             :: what,elements,actions
 integer                  :: I
 !
 ! Work Space
 !
 integer :: D(3,2),I_range(2),J,size_
 logical :: l_HF,l_COHSEX,l_GW_NEQ,l_IO,l_v3,l_v1,l_table,l_all,l_alloc,&
&           l_link,l_clean,l_collision,l_distribute
 !
 if (I==0) I_range=(/1,PAR_nQP/)
 if (I==0) size_=PAR_nQP
 if (I> 0) I_range=(/I,I/)
 if (I> 0) size_=1
 !
 ! What
 !
 l_HF     =index(what,"HF")>0
 l_COHSEX =index(what,"COHSEX")>0
 l_GW_NEQ =index(what,"GW_NEQ")>0
 !
 ! Elements
 !
 l_v1   =index(elements,"v1")>0
 l_v3   =index(elements,"v3")>0
 l_all  =index(elements,"all")>0
 l_table    =index(elements,"table")>0
 l_collision=index(elements,"coll")>0
 l_IO   =index(elements,"IO")>0
 !
 ! Actions
 !
 l_alloc     =index(actions,"alloc")>0
 l_link      =index(actions,"link")>0
 l_clean     =index(actions,"clean")>0
 l_distribute=index(actions,"distribute")>0
 !
 if (l_HF) then
   if (l_collision.and.l_alloc) then
     allocate(HF_collisions(size_))
     nHF_collisions=size_
     HF_collisions(:)%kind=SC_HF
     HF_collisions(:)%name="HF"
   endif
   D(1,:)=SC_bands
   D(2,:)=SC_bands
   D(3,:)=(/1,nkibz/)
   call local_alloc_and_free(HF_collisions,nHF_collisions)
   if (l_collision.and.l_clean) then
     deallocate(HF_collisions)
     nHF_collisions=0
   endif
   if (l_IO.and.l_alloc) then
      !allocate(IO_collision%v(COLL(J)%N))
      !COLL(J)%v=cZERO
      !COLL(J)%D=D
      !allocate(COLL(J)%v(COLL(J)%N))
   endif
 endif
 !
 if (l_COHSEX) then
   if (l_collision.and.l_alloc) then
     allocate(COHSEX_collisions(size_))
     nCOHSEX_collisions=size_
     COHSEX_collisions(:)%kind=SC_COHSEX
     COHSEX_collisions(:)%name="COHSEX"
   endif
   D(1,:)=SC_bands
   D(2,:)=SC_bands
   D(3,:)=(/1,nkibz/)
   call local_alloc_and_free(COHSEX_collisions,nCOHSEX_collisions)
   if (l_collision.and.l_clean) then
     deallocate(COHSEX_collisions)
     nCOHSEX_collisions=0
   endif
 endif
 !
 if (l_GW_NEQ) then
   if (l_collision.and.l_alloc) then
     allocate(GW_NEQ_collisions(size_))
     nGW_NEQ_collisions=size_
     GW_NEQ_collisions(:)%kind=SE_GW_NEQ
     GW_NEQ_collisions(:)%name="GW_NEQ"
   endif
   D(1,:)=(/1,QP_n_states/)
   D(2,:)=(/1,nsym/)
   D(3,:)=(/1,nqbz/)
   if (l_distribute) then
     D(1,:)=(/1,PAR_nQPp/)
     D(3,:)=(/1,PAR_nQ/)
   endif
   call local_alloc_and_free(GW_NEQ_collisions,nGW_NEQ_collisions)
   if (l_collision.and.l_clean) then
     deallocate(GW_NEQ_collisions)
     nGW_NEQ_collisions=0
   endif
 endif
 !
 contains
   !
   subroutine local_alloc_and_free(COLL,N)
     integer                  :: N
     type(COLLISIONS_element) :: COLL(N)
     do J=I_range(1),I_range(2)
       if (l_alloc.and.(l_v3.or.l_all)) then
         allocate(COLL(J)%v3(D(1,1):D(1,2),D(2,1):D(2,2),D(3,1):D(3,2)))
         COLL(J)%v3=cZERO
         COLL(J)%D=D
       endif
       if (l_clean.and.(l_v3.or.l_all)) then
         deallocate(COLL(J)%v3)
       endif
       if (l_alloc.and.(l_v1.or.l_all)) then
         allocate(COLL(J)%v(COLL(J)%N))
         COLL(J)%v=cZERO
         COLL(J)%D=D
       endif
       if (l_clean.and.(l_v1.or.l_all)) then
         deallocate(COLL(J)%v)
       endif
       if (l_alloc.and.(l_table.or.l_all)) then
         allocate(COLL(J)%table(D(1,1):D(1,2),D(2,1):D(2,2),D(3,1):D(3,2)))
         COLL(J)%table=0
         COLL(J)%D=D
       endif
       if (l_clean.and.(l_table.or.l_all)) then
         deallocate(COLL(J)%table)
       endif
     enddo
     if (l_alloc.and.(l_v1.or.l_all)) then
       call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v1",(/size(COLL(1)%v)*(I_range(2)-I_range(1)+1)/))
     endif
     if (l_alloc.and.(l_v3.or.l_all)) then
       call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3",(/size(COLL(1)%v3)*(I_range(2)-I_range(1)+1)/))
     endif
     if (l_alloc.and.(l_table.or.l_all)) then
       call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_table",(/size(COLL(1)%table)*(I_range(2)-I_range(1)+1)/),(/LP/))
     endif
     if (l_clean.and.(l_v1.or.l_all)) then
       call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v1")
     endif
     if (l_clean.and.(l_v3.or.l_all)) then
       call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3")
     endif
     if (l_clean.and.(l_table.or.l_all)) then
       call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_table")
     endif
     do J=I_range(1),I_range(2)
       if (l_link.and.l_v3) then
         COLL(J)%v3 => COLL(1)%v3
       endif
       if (l_link.and.l_v1) then
         COLL(J)%v => COLL(1)%v
       endif
     enddo
   end subroutine
   !
end subroutine
