!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine io_RT_components_Reference(ID)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elph_scatt
 use R_lattice,      ONLY:nqbz
 use IO_m,           ONLY:read_is_on
 use IO_int,         ONLY:def_variable_bulk,io_variable_bulk
 use RT_lifetimes,   ONLY:RT_lifetime,RT_EE_REF_life,RT_EP_emit_REF_life,RT_EP_abs_REF_life,&
&                         RT_PE_REF_life,RT_EH_REF_life,q_weight_RT
 use real_time,      ONLY:G_lesser_reference,RT_nk,RT_nbands,RT_is_dynamically_dephased
#if defined _ELPH
 use ELPH,           ONLY:elph_branches
#endif
 !
#include<memory.h>
 !
 integer, intent(in)  :: ID
 !
 ! Work Space
 !
 integer              :: VAR_ID
 !
#if defined _ELPH
 !
 if (l_elph_scatt) then
   !
   if (read_is_on(ID)) then
     YAMBO_FREE(q_weight_RT)
     YAMBO_ALLOC(q_weight_RT,(nqbz,elph_branches(1):elph_branches(2),2))
   endif
   !
   call def_variable_bulk(ID,"Q_WEIGHT",3,(/nqbz,elph_branches(2)-elph_branches(1)+1,2/),SP)
   call  io_variable_bulk(ID,3,R3=q_weight_RT(:,:,:))
   !
 endif
 !
#endif
 !
 if (RT_is_dynamically_dephased) then
   !
   VAR_ID=10
   call elemental_LIFE_IO(RT_EE_REF_life)
   VAR_ID=12
   call elemental_LIFE_IO(RT_EP_emit_REF_life)
   VAR_ID=14
   call elemental_LIFE_IO(RT_EP_abs_REF_life)
   VAR_ID=16
   call elemental_LIFE_IO(RT_EH_REF_life)
   VAR_ID=18
   call elemental_LIFE_IO(RT_PE_REF_life)
   !
 endif
 !
 if (allocated(G_lesser_reference)) then
   !
   call def_variable_bulk(ID,"G_lesser_reference",4,(/2,RT_nbands,RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,4,C3=G_lesser_reference)
   !
 endif
 !
 contains
   !
   subroutine elemental_LIFE_IO(LIFE)
     type(RT_lifetime) :: LIFE
     if (LIFE%active) then
       call def_variable_bulk(ID,trim(LIFE%kind)//"_ABS",VAR_ID,(/RT_nbands,RT_nk/),SP)
       call  io_variable_bulk(ID,VAR_ID,R2=LIFE%abs)
       VAR_ID=VAR_ID+1
       call def_variable_bulk(ID,trim(LIFE%kind)//"_EMIT",VAR_ID,(/RT_nbands,RT_nk/),SP)
       call  io_variable_bulk(ID,VAR_ID,R2=LIFE%emit)
     endif
   end subroutine
   !
end subroutine io_RT_components_Reference
