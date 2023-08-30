!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module OUTPUT
 !
 use pars,  ONLY:SP,schlen,lchlen,rZERO
 !
 integer, parameter       :: N_MAX_columns=30
 integer, parameter       :: N_MAX_files  =30
 !
 type COLt
   integer            :: POS=0
   character(schlen)  :: title= " " 
   character(1)       :: KIND= " " 
   real(SP)           :: R_value=rZERO
   real(SP)           :: I_value=0
 end type
 !
 type OUTt
   logical            :: TITLES_ordered=.FALSE.
   logical            :: TITLES_dumped=.FALSE.
   integer            :: N_columns = 0
   integer            :: N_columns_ordered = 0
   character(lchlen)  :: filename = " "
   character(schlen)  :: filename_short= " "
   type(COLt)         :: COL(N_MAX_columns)
   integer            :: COL_WIDTH=0
 end type
 !
 type(OUTt):: OUT_file(N_MAX_files)
 !
 interface
   !
   subroutine OUTPUT_unit_conversion(Ivar,Rvar,DESC,KIND,PARSER_call)
     use pars,  ONLY:SP
     real(SP)    ,optional :: Rvar
     integer     ,optional :: Ivar
     character(*),optional :: DESC
     character(*),optional :: KIND
     logical     ,optional :: PARSER_call
   end subroutine
   !
   subroutine OUTPUT_driver(file,action,TITLES,I_VALUES,R_VALUES,ORDER,UNIT,KIND)
     use pars,  ONLY:SP
     character(*)            :: file
     character(*) , OPTIONAL :: action
     character(*) , OPTIONAL :: TITLES(:)
     integer      , OPTIONAL :: I_VALUES(:)
     real(SP)     , OPTIONAL :: R_VALUES(:)
     character(*) , OPTIONAL :: ORDER(:)
     character(*) , OPTIONAL :: UNIT
     character(*) , OPTIONAL :: KIND
   end subroutine
   !
 end interface
 !
 contains
   !
   subroutine OUTPUT_reset(i_f,action)
     character(*) :: action
     integer      :: ic,i_f
     if (action=="row".or.action=="reset") then
       do ic=1,N_MAX_columns
         OUT_file(i_f)%COL(ic)%POS=0
         OUT_file(i_f)%COL(ic)%title=" "
         OUT_file(i_f)%COL(ic)%KIND=" "
         OUT_file(i_f)%COL(ic)%I_value=0
         OUT_file(i_f)%COL(ic)%R_value=0.
       enddo
       OUT_file(i_f)%N_columns = 0
       OUT_file(i_f)%N_columns_ordered = 0
       OUT_file(i_f)%TITLES_ordered=.FALSE.
     endif
     if (action=="reset") then
       OUT_file(i_f)%COL_WIDTH = 0
       OUT_file(i_f)%TITLES_dumped=.FALSE.
       OUT_file(i_f)%filename = " "
       OUT_file(i_f)%filename_short= " "
     endif
   end subroutine
 !
end module
