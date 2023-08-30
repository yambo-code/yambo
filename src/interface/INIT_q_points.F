!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_q_points( )
 !
 ! This routine:
 !
 ! 1. reads the q-points from the input file
 ! 2. reads the q-points from the e-p DB's 
 !
 use drivers,     ONLY:infile_editing
 use vec_operate, ONLY:v_is_zero
 use pars,        ONLY:SP,schlen,DP
 use it_tools,    ONLY:check_verbosity
 use R_lattice,   ONLY:nqibz,q_pt,bz_samp,q_source
#if defined _ELPH
 use ELPH,        ONLY:PH_qpt,elph_use_q_grid,elph_nQ,elph_grids_are_expanded
#endif
 use parser_m,    ONLY:PARSER_array
 !
#include<memory.h>
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
#if defined _ELPH
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
#endif
 !
 ! Now check the input file
 !
 l_write_the_q_field=.FALSE.
 !
 if (nqibz==0) then
   call PARSER_array(mode="REAL dimensions",VAR="QptCoord",N=nq_input,M=3)
   if (nq_input==0.and.infile_editing) then
     nqibz=1
     YAMBO_ALLOC(q_pt,(nqibz,3))
     q_pt=0.
   else if (nq_input>0) then
     nqibz=nq_input
     YAMBO_ALLOC(q_pt,(nqibz,3))
     call PARSER_array(mode="read",VAR="QptCoord",N=nq_input,M=3,R_v=q_pt)
   endif
   !
   if (nqibz>0) then
     l_write_the_q_field=nqibz>1
     if (.not.l_write_the_q_field) l_write_the_q_field=nqibz==1.and..not.v_is_zero(q_pt(1,:))
   endif
   if (.not.l_write_the_q_field) l_write_the_q_field=check_verbosity("kpt")
   if (.not.l_write_the_q_field) return
   !
   call PARSER_array(mode="write",VAR="QptCoord",DESC="[KPT] [iku] Q-points coordinates (compatibility)",&
&                    N=nqibz,M=3,R_v=q_pt)
 endif
 !
 contains 
   !
   integer function get_e_p_q_point(i_q,q_in)
     !
     use D_lattice,   ONLY:alat
     use IO_m,        ONLY:io_unit,OP_RD_CL
     use IO_int,      ONLY:io_control,io_connect,io_disconnect
     !
     integer :: i_q
     real(DP):: q_in(3)
     !
     integer           :: io_err,ID
     character(schlen) :: dbph_name
     real(DP)          :: alat_DP
     !
     call io_control(ACTION=OP_RD_CL,ID=ID)
     write (dbph_name,'(a,i6.6)') 's.dbph_',iabs(i_q)
     io_err=io_connect(desc=trim(dbph_name),type=-4,ID=ID)
     get_e_p_q_point=io_err
     !
     if (io_err==0) then
       read (io_unit(ID)) 
       read (io_unit(ID)) alat_DP,q_in
       q_in=q_in/alat_DP*alat(:)
     endif
     call io_disconnect(ID)
     !
   end function
   !
end subroutine
