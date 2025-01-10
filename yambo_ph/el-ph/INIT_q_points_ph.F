!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_q_points_ph( )
 !
 ! This routine reads the q-points from the e-p DB's 
 !
 use drivers,     ONLY:infile_editing
 use vec_operate, ONLY:v_is_zero
 use pars,        ONLY:SP,schlen,DP
 use it_tools,    ONLY:check_verbosity
 use R_lattice,   ONLY:nqibz,q_pt,bz_samp,q_source
 use ELPH,        ONLY:PH_qpt,elph_use_q_grid,elph_nQ,elph_grids_are_expanded
 use parser_m,    ONLY:PARSER_array
 !
#include<y_memory.h>
 !
 ! Work Space
 !
 integer       :: ic,nq_input
 type(bz_samp) :: q
 real(DP)      :: q_pt_db(3)
 logical       :: l_write_the_q_field
 !
 ! Load (eventually) the q-points from the e-p database
 !
 if (get_e_p_q_point(1,q_pt_db)==0) then
   q_source="is read from the PWscf El-Ph databases"
   do ic=1,2
     nqibz=0
     do while(get_e_p_q_point(nqibz+1,q_pt_db)==0) 
       nqibz=nqibz+1
       if (ic==2) q_pt(nqibz,:)=-real(q_pt_db,SP)
     enddo
     if (ic==1.and..not.allocated(q_pt)) then
       YAMBO_ALLOC(q_pt,(nqibz,3))
     endif
   enddo
 else if (allocated(PH_qpt).and.elph_use_q_grid.and..not.elph_grids_are_expanded) then
   q_source="is read from the El-Ph databases"
   nqibz=elph_nQ
   YAMBO_ALLOC(q_pt,(nqibz,3))
   q_pt(:,:)=-real(PH_qpt(:,:),SP)
 endif
 !
end subroutine
