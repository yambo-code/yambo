!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARSER_convert_INT(ivar,v_descr,force_RL)
 !
 !use units,  ONLY:Gigabyte,Megabyte,Kilobyte
 !
 implicit none
 !
 integer     ::ivar
 character(*)::v_descr
 logical     ::force_RL
 !
 if (ivar<0) return
 !
 if (index(v_descr,'RL')/=0) then
   call PARSER_close_G(ivar,'tRL')
 else if (index(v_descr,'mHa')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'tmHa')
 else if (index(v_descr,'Ha')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'tHa')
 else if (index(v_descr,'mRy')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'tmRy')
 else if (index(v_descr,'Ry')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'tRy')
 else if (index(v_descr,'eV')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'teV')
 endif
 !
end subroutine
