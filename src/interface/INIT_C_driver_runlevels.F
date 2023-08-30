!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_C_driver_runlevels(defs)
 !
 use it_tools,      ONLY:it,initdefs
 use pars,          ONLY:lchlen,schlen
 use C_driver,      ONLY:get_runlevel,code_bin,code_tool
 use stderr,        ONLY:STRING_split
 !
 implicit none
 !
 type(initdefs) :: defs
 !
 ! Work Space
 !
 integer           :: i_c,i_s
 character(lchlen) :: string,desc,bin
 character(schlen) :: bin_strings(10)
 logical           :: bin_is_ok
 !
 do i_c=0,199
   !
   string=" "
   desc=" "
   bin=" "
   !
   call get_runlevel(bin,3,i_c)
   !
   if (trim(bin)=="EMPTY") cycle
   !
   call STRING_split(bin,bin_strings)
   !
   bin_is_ok=.FALSE.
   !
   do i_s=1,10
     if (len_trim(bin_strings(i_s))==0) cycle
     if (trim(bin_strings(i_s))==trim(code_bin)) bin_is_ok=.TRUE.
     if (trim(bin_strings(i_s))==trim(code_tool)) bin_is_ok=.TRUE.
   enddo
   !
   if (.not.bin_is_ok) cycle
   !
   call get_runlevel(string,1,i_c)
   call get_runlevel(desc,2,i_c)
   !
   call it('r',defs,trim(string),'[R] '//trim(desc))
   !
 enddo
 !
end subroutine
