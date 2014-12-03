!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine COLLISIONS_alloc_and_free(what,elements,actions,I)
 !
 use pars,           ONLY:SP,LP,cZERO,rZero
 use memory_m,       ONLY:mem_est
 use SC,             ONLY:SC_bands
 use plasma,         ONLY:N_plasma_poles_global
 use R_lattice,      ONLY:nqbz,nkibz
 use parallel_m,     ONLY:PAR_nPlasma,PAR_nQ,PAR_nQP
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
 integer :: D(3,2),I_range(2),J,mem_size(4),size_
 logical :: l_HF,l_COHSEX,l_GW_NEQ,l_v3,l_v1,l_table,l_all,l_alloc,&
&           l_clean,l_collision,l_distribute
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
 !
 ! Actions
 !
 l_alloc     =index(actions,"alloc")>0
 l_clean     =index(actions,"clean")>0
 l_distribute=index(actions,"distribute")>0
 !
 if (l_HF) then
   D(1,:)=SC_bands
   D(2,:)=SC_bands
   D(3,:)=(/1,nkibz/)
   if (l_collision.and.l_alloc) then
     allocate(HF_collisions(size_))
     nHF_collisions=size_
     HF_collisions(:)%kind=SC_HF
     HF_collisions(:)%name="HF"
   endif
   call local_alloc_and_free(HF_collisions,nHF_collisions)
   if (l_collision.and.l_clean) then
     if (allocated(HF_collisions)) then
       call local_alloc_and_free(HF_collisions,nHF_collisions)
       deallocate(HF_collisions)
     endif
     nHF_collisions=0
   endif
 endif
 !
 if (l_COHSEX) then
   D(1,:)=SC_bands
   D(2,:)=SC_bands
   D(3,:)=(/1,nkibz/)
   if (l_collision.and.l_alloc) then
     allocate(COHSEX_collisions(size_))
     nCOHSEX_collisions=size_
     COHSEX_collisions(:)%kind=SC_COHSEX
     COHSEX_collisions(:)%name="COHSEX"
   endif
   call local_alloc_and_free(COHSEX_collisions,nCOHSEX_collisions)
   if (l_collision.and.l_clean) then
     if (allocated(COHSEX_collisions)) then
       call local_alloc_and_free(COHSEX_collisions,nCOHSEX_collisions)
       deallocate(COHSEX_collisions)
     endif
     nCOHSEX_collisions=0
   endif
 endif
 !
 if (l_GW_NEQ) then
   D(1,:)=(/1,N_plasma_poles_global/)
   D(2,:)=(/1,1/)
   D(3,:)=(/1,nqbz/)
   if (l_distribute.and.(l_v3.or.l_table)) then
     D(1,:)=(/1,PAR_nPlasma/)
     D(3,:)=(/1,PAR_nQ/)
   endif
   if (l_collision.and.l_alloc) then
     allocate(GW_NEQ_collisions(size_))
     nGW_NEQ_collisions=size_
     GW_NEQ_collisions(:)%kind=SE_GW_NEQ
     GW_NEQ_collisions(:)%name="GW_NEQ"
   endif
   call local_alloc_and_free(GW_NEQ_collisions,nGW_NEQ_collisions)
   if(l_collision.and.l_clean) then
     if (allocated(GW_NEQ_collisions)) then
       deallocate(GW_NEQ_collisions)
     endif
     nGW_NEQ_collisions=0
   endif
 endif
 !
 contains
   !
   subroutine local_alloc_and_free(COLL,N)
     integer                  :: N
     type(COLLISIONS_element) :: COLL(N)
     mem_size=0
     do J=I_range(1),I_range(2)
       if (l_alloc.and.(l_v3.or.l_all)) then
         if(l_GW_NEQ) then
           allocate(COLL(J)%v3r(D(1,1):D(1,2),D(2,1):D(2,2),D(3,1):D(3,2)))
           COLL(J)%v3r=rZERO
           mem_size(1)=mem_size(1)+size(COLL(J)%v3r)
         endif
         if(l_COHSEX.or.l_HF) then
           allocate(COLL(J)%v3c(D(1,1):D(1,2),D(2,1):D(2,2),D(3,1):D(3,2)))
           COLL(J)%v3c=cZERO
           mem_size(2)=mem_size(2)+size(COLL(J)%v3c)
         endif
         COLL(J)%D=D
       endif
       if (l_clean.and.(l_v3.or.l_all)) then
         if (l_GW_NEQ        .and.associated(COLL(J)%v3r)) deallocate(COLL(J)%v3r)
         if (l_COHSEX.or.l_HF.and.associated(COLL(J)%v3c)) deallocate(COLL(J)%v3c)
       endif
       if (l_alloc.and.(l_v1.or.l_all)) then
         allocate(COLL(J)%v(COLL(J)%N))
         COLL(J)%v=cZERO
         mem_size(3)=mem_size(3)+size(COLL(J)%v)
       endif
       if (l_clean.and.(l_v1.or.l_all)) then
         if (associated(COLL(J)%v)) deallocate(COLL(J)%v)
       endif
       if (l_alloc.and.(l_table.or.l_all)) then
         allocate(COLL(J)%table(D(1,1):D(1,2),D(2,1):D(2,2),D(3,1):D(3,2)))
         COLL(J)%table=0
         COLL(J)%D=D
         mem_size(4)=mem_size(4)+size(COLL(J)%table)
       endif
       if (l_clean.and.(l_table.or.l_all)) then
         if (associated(COLL(J)%table)) deallocate(COLL(J)%table)
       endif
     enddo
     !
     if (l_alloc) then
       if(l_v3.or.l_all) then
         if(l_GW_NEQ        ) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3r",  (/mem_size(1)/))
         if(l_COHSEX.or.l_HF) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3c",  (/mem_size(2)/))
       endif
       if (l_v1.or.l_all)     call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v1",   (/mem_size(3)/))
       if (l_table.or.l_all)  call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_table",(/mem_size(4)/),(/LP/))
     endif
     !
     if (l_clean) then
       if(l_v3.or.l_all) then
         if(l_GW_NEQ        ) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3r")
         if(l_COHSEX.or.l_HF) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3c")
       endif
       if(l_v1.or.l_all)      call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v1")
       if(l_table.or.l_all)   call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_table")
     endif
     !
   end subroutine
   !
end subroutine
