!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine COLLISIONS_map_to_QP_table(what,COLL)
 !
 use collision_ext, ONLY:COLLISIONS_group,COLL_bands
 use QP_m,          ONLY:QP_nk,QP_nb,QP_table,QP_n_states
 use electrons,     ONLY:n_sp_pol
 use R_lattice,     ONLY:nXkibz
 !
#include<memory.h>
 !
 character(*)                            :: what
 type(COLLISIONS_group),   intent(inout) :: COLL
 !
 ! Work Space
 !
 integer :: i_coll
 !
 if (what=="fill") then
   if (allocated(QP_table)) return
   QP_n_states=COLL%N
   YAMBO_ALLOC(QP_table,(QP_n_states,3+n_sp_pol-1))
   QP_table=0
   QP_nb=COLL_bands(2)
   QP_nk=nXkibz
   do i_coll=1,COLL%N
     QP_table(i_coll,:3)=COLL%state(i_coll,:3)
     if (n_sp_pol==2) QP_table(i_coll,4)=COLL%state(i_coll,4)
   enddo
 endif
 !
 if (what=="clean") then
   YAMBO_FREE(QP_table)
   QP_nb=0
   QP_nk=0
   QP_n_states=0
 endif
 !
end subroutine COLLISIONS_map_to_QP_table
