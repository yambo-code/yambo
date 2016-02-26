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
subroutine COLLISIONS_alloc_and_free(what,elements,actions,I)
 !
 use pars,           ONLY:LP,cZERO,rZERO
 use memory_m,       ONLY:mem_est
 use plasma,         ONLY:N_plasma_poles_global
 use R_lattice,      ONLY:nqbz,nkibz
 use parallel_m,     ONLY:PAR_nPlasma,PAR_nQ,PAR_nQP
 use global_XC,      ONLY:SC_HF,SC_COHSEX,SE_GW_NEQ
 use collision,      ONLY:COLLISIONS_element,Fock_COLL_element,SEX_COLL_element,GW_NEQ_COLL_element,&
&                         nFock_COLL_element,nSEX_COLL_element,nGW_NEQ_COLL_element,COLL_bands
#if defined _QED
 use QP_m,           ONLY:QP_QED_ng
 use collision,      ONLY:P_COLL_element,nP_COLL_element
 use global_XC,      ONLY:SE_GW_QED
#endif
 !
 implicit none
 !
 character(*)             :: what,elements,actions
 integer                  :: I
 !
 ! Work Space
 !
 integer :: D(3,2),COLL_range(2),J,mem_size(5),size_
 logical :: l_Fock,l_SEX,l_GW_NEQ,l_v3,l_v1,l_table,l_all,l_alloc,&
&           l_clean,l_collision,l_distribute,l_Momentum,l_complex,l_real
 !
 if (I==0) COLL_range=(/1,PAR_nQP/)
 if (I==0) size_=PAR_nQP
 if (I> 0) COLL_range=(/I,I/)
 if (I> 0) size_=1
 !
 ! What
 !
 l_Fock    =index(what,"Fock")>0
 l_SEX     =index(what,"SEX")>0
 l_GW_NEQ  =index(what,"GW_NEQ")>0
 l_Momentum=index(what,"Momentum")>0
 !
 l_complex=l_Fock.or.l_SEX
 l_real   =l_GW_NEQ.or.l_Momentum
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
 if (l_Fock) then
   D(1,:)=COLL_bands
   D(2,:)=COLL_bands
   D(3,:)=(/1,nkibz/)
   if (l_collision.and.l_alloc) then
     allocate(Fock_COLL_element(size_))
     nFock_COLL_element=size_
     Fock_COLL_element(:)%kind=SC_HF
     Fock_COLL_element(:)%name="Fock"
   endif
   call local_alloc_and_free(Fock_COLL_element,nFock_COLL_element)
   if (l_collision.and.l_clean) then
     if (allocated(Fock_COLL_element)) then
       call local_alloc_and_free(Fock_COLL_element,nFock_COLL_element)
       deallocate(Fock_COLL_element)
     endif
     nFock_COLL_element=0
   endif
 endif
 !
 if (l_SEX) then
   D(1,:)=COLL_bands
   D(2,:)=COLL_bands
   D(3,:)=(/1,nkibz/)
   if (l_collision.and.l_alloc) then
     allocate(SEX_COLL_element(size_))
     nSEX_COLL_element=size_
     SEX_COLL_element(:)%kind=SC_COHSEX
     SEX_COLL_element(:)%name="SEX"
   endif
   call local_alloc_and_free(SEX_COLL_element,nSEX_COLL_element)
   if (l_collision.and.l_clean) then
     if (allocated(SEX_COLL_element)) then
       call local_alloc_and_free(SEX_COLL_element,nSEX_COLL_element)
       deallocate(SEX_COLL_element)
     endif
     nSEX_COLL_element=0
   endif
 endif
 !
#if defined _QED
 !
 if (l_Momentum) then
    D(2,:)=COLL_bands
    D(1,:)=(/1,QP_QED_ng/)
    D(3,:)=(/1,nqbz/)
    !DEBUG> TO CODE the distribute part
    if (l_distribute) then
     !D(1,:)=(/1,PAR_nRL/)
     !D(3,:)=(/1,PAR_nQ/)
   endif
   if (l_collision.and.l_alloc) then
     allocate(P_COLL_element(size_))
     nP_COLL_element=size_
     P_COLL_element(:)%kind=SE_GW_QED
     P_COLL_element(:)%name="Momentum"
   endif
   call local_alloc_and_free(P_COLL_element,nP_COLL_element)
   if (l_collision.and.l_clean) then
     if (allocated(P_COLL_element)) then
       call local_alloc_and_free(P_COLL_element,nP_COLL_element)
       deallocate(P_COLL_element)
     endif
     nP_COLL_element=0
   endif
 endif
 !
#endif
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
     allocate(GW_NEQ_COLL_element(size_))
     nGW_NEQ_COLL_element=size_
     GW_NEQ_COLL_element(:)%kind=SE_GW_NEQ
     GW_NEQ_COLL_element(:)%name="GW_NEQ"
   endif
   call local_alloc_and_free(GW_NEQ_COLL_element,nGW_NEQ_COLL_element)
   if(l_collision.and.l_clean) then
     if (allocated(GW_NEQ_COLL_element)) then
       deallocate(GW_NEQ_COLL_element)
     endif
     nGW_NEQ_COLL_element=0
   endif
 endif
 !
 contains
   !
   subroutine local_alloc_and_free(COLL,N)
     integer                  :: N
     type(COLLISIONS_element) :: COLL(N)
     mem_size=0
     do J=COLL_range(1),COLL_range(2)
       if (l_alloc.and.(l_v3.or.l_all)) then
         if(l_real) then
           allocate(COLL(J)%v3_r(D(1,1):D(1,2),D(2,1):D(2,2),D(3,1):D(3,2)))
           COLL(J)%v3_r=rZERO
           mem_size(1)=mem_size(1)+size(COLL(J)%v3_r)
         endif
         if(l_complex) then
           allocate(COLL(J)%v3_c(D(1,1):D(1,2),D(2,1):D(2,2),D(3,1):D(3,2)))
           COLL(J)%v3_c=cZERO
           mem_size(2)=mem_size(2)+size(COLL(J)%v3_c)
         endif
         COLL(J)%D=D
       endif
       if (l_clean.and.(l_v3.or.l_all)) then
         if (l_real  .and.associated(COLL(J)%v3_r)) deallocate(COLL(J)%v3_r)
         if (l_complex.and.associated(COLL(J)%v3_c)) deallocate(COLL(J)%v3_c)
       endif
       if (l_alloc.and.(l_v1.or.l_all)) then
         if(l_real) then
           allocate(COLL(J)%v_r(COLL(J)%N))
           COLL(J)%v_r=rZERO
           mem_size(3)=mem_size(3)+size(COLL(J)%v_r)
         endif
         if(l_complex) then
           allocate(COLL(J)%v_c(COLL(J)%N))
           COLL(J)%v_c=cZERO
           mem_size(4)=mem_size(4)+size(COLL(J)%v_c)
         endif
       endif
       if (l_clean.and.(l_v1.or.l_all)) then
         if (associated(COLL(J)%v_c)) deallocate(COLL(J)%v_c)
         if (associated(COLL(J)%v_r)) deallocate(COLL(J)%v_r )
       endif
       if (l_alloc.and.(l_table.or.l_all)) then
         allocate(COLL(J)%table(D(1,1):D(1,2),D(2,1):D(2,2),D(3,1):D(3,2)))
         COLL(J)%table=0
         COLL(J)%D=D
         mem_size(5)=mem_size(5)+size(COLL(J)%table)
       endif
       if (l_clean.and.(l_table.or.l_all)) then
         if (associated(COLL(J)%table)) deallocate(COLL(J)%table)
       endif
     enddo
     !
     if (l_alloc) then
       if(l_v3.or.l_all) then
         if (l_real  )  call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3r",  (/mem_size(1)/))
         if (l_complex) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3c",  (/mem_size(2)/))
       endif
       if (l_v1.or.l_all) then
         if (l_real  )  call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_vr",   (/mem_size(3)/))
         if (l_complex) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_vc",   (/mem_size(4)/))
       endif
       if (l_table.or.l_all)  call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_table",(/mem_size(5)/),(/LP/))
     endif
     !
     if (l_clean) then
       if (l_v3.or.l_all) then
         if (l_real   ) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3r")
         if (l_complex) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3c")
       endif
       if (l_v1.or.l_all) then
         if (l_real   ) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_vr")
         if (l_complex) call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_vc")
       endif
       if (l_table.or.l_all)   call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_table")
     endif
     !
   end subroutine
   !
end subroutine
