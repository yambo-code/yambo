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
 use parallel_m,     ONLY:PAR_nPlasma,PAR_nQ
 use collision,      ONLY:COLLISIONS_element,FOCK_COLL_element,SEX_COLL_element,GW_NEQ_COLL_element,&
&                         COLL_bands,&
&                         GW_NEQ_collisions,FOCK_collisions,SEX_collisions,P_collisions,COLLISIONS_group
#if defined _QED
 use QP_m,           ONLY:QP_QED_ng
 use collision,      ONLY:P_COLL_element
#endif
 !
 implicit none
 !
 character(*)             :: what,elements,actions
 integer                  :: I
 !
 ! Work Space
 !
 integer :: D(3,2),COLL_range(2),J,K,mem_size(5),size_
 logical :: l_Fock,l_SEX,l_GW_NEQ,l_v3,l_v1,l_table,l_all,l_alloc,&
&           l_clean,l_collision,l_distribute,l_Momentum,l_complex,l_real
 !
 ! What
 !
 l_Fock    =index(what,"Fock")>0
 l_SEX     =index(what,"SEX")>0
 l_GW_NEQ  =index(what,"GW_NEQ")>0
 l_Momentum=index(what,"Momentum")>0
 !
 ! Dimension
 !
 if (I==0) then
   if (l_Fock)     COLL_range=(/1,FOCK_collisions%PAR_N/)
   if (l_SEX)      COLL_range=(/1,SEX_collisions%PAR_N/)
   if (l_GW_NEQ)   COLL_range=(/1,GW_NEQ_collisions%PAR_N/)
   if (l_Momentum) COLL_range=(/1,P_collisions%PAR_N/)
   size_=COLL_range(2)
 endif
 if (I> 0) COLL_range=(/I,I/)
 if (I> 0) size_=1
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
     allocate(FOCK_COLL_element(size_))
     FOCK_COLL_element%N=size_
   endif
   call local_alloc_and_free(FOCK_COLL_element,FOCK_collisions)
   if (l_collision.and.l_clean) then
     if (allocated(FOCK_COLL_element)) then
       call local_alloc_and_free(FOCK_COLL_element,FOCK_collisions)
       deallocate(FOCK_COLL_element)
     endif
     FOCK_COLL_element%N=0
   endif
 endif
 !
 if (l_SEX) then
   D(1,:)=COLL_bands
   D(2,:)=COLL_bands
   D(3,:)=(/1,nkibz/)
   if (l_collision.and.l_alloc) then
     allocate(SEX_COLL_element(size_))
     SEX_COLL_element%N=size_
   endif
   call local_alloc_and_free(SEX_COLL_element,SEX_collisions)
   if (l_collision.and.l_clean) then
     if (allocated(SEX_COLL_element)) then
       call local_alloc_and_free(SEX_COLL_element,SEX_collisions)
       deallocate(SEX_COLL_element)
     endif
     SEX_COLL_element%N=0
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
     P_COLL_element%N=size_
   endif
   call local_alloc_and_free(P_COLL_element,P_collisions)
   if (l_collision.and.l_clean) then
     if (allocated(P_COLL_element)) then
       call local_alloc_and_free(P_COLL_element,P_collisions)
       deallocate(P_COLL_element)
     endif
     P_COLL_element%N=0
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
     GW_NEQ_COLL_element%N=size_
   endif
   call local_alloc_and_free(GW_NEQ_COLL_element,GW_NEQ_collisions)
   if(l_collision.and.l_clean) then
     if (allocated(GW_NEQ_COLL_element)) then
       deallocate(GW_NEQ_COLL_element)
     endif
     GW_NEQ_COLL_element%N=0
   endif
 endif
 !
 contains
   !
   subroutine local_alloc_and_free(COLL,COLL_grp)
     type(COLLISIONS_group)   :: COLL_grp
     type(COLLISIONS_element) :: COLL(COLL_grp%N)
     mem_size=0
     do J=COLL_range(1),COLL_range(2)
       !
       ! Here I assign to each collision element an index corresponding to the REAL collision
       ! associated. This is because J runs on the parallel components and not on the real ones.
       !
       do K=1,COLL_grp%N
         if (COLL_grp%PAR_map(K)==J) COLL(J)%I=K
       enddo
       !
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
         COLL_grp%D=D
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
         COLL_grp%D=D
         mem_size(5)=mem_size(5)+size(COLL(J)%table)
       endif
       if (l_clean.and.(l_table.or.l_all)) then
         if (associated(COLL(J)%table)) deallocate(COLL(J)%table)
       endif
     enddo
     !
     if (l_alloc) then
       if(l_v3.or.l_all) then
         if (l_real  )  call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_v3r",  (/mem_size(1)/))
         if (l_complex) call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_v3c",  (/mem_size(2)/))
       endif
       if (l_v1.or.l_all) then
         if (l_real  )  call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_vr",   (/mem_size(3)/))
         if (l_complex) call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_vc",   (/mem_size(4)/))
       endif
       if (l_table.or.l_all)  call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_table",(/mem_size(5)/),(/LP/))
     endif
     !
     if (l_clean) then
       if (l_v3.or.l_all) then
         if (l_real   ) call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_v3r")
         if (l_complex) call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_v3c")
       endif
       if (l_v1.or.l_all) then
         if (l_real   ) call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_vr")
         if (l_complex) call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_vc")
       endif
       if (l_table.or.l_all)   call mem_est("COLLISIONS_"//trim(COLL_grp%name)//"_table")
     endif
     !
   end subroutine
   !
end subroutine
