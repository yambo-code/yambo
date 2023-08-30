!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine MEM_library(member,shelf)
 !
 use memory,  ONLY:N_MEM_shelf,MEM_shelf,N_MEM_LIBs,MEM_LIBs
 !
 implicit none
 !
 character(*) :: member
 character(*) :: shelf
 ! 
 ! Work Space
 !
 integer           :: i_SHELF,i_LIB,i_s
 integer, external :: MEM_LIB_finder
 !
 i_SHELF=0
 do i_s=1,N_MEM_shelf
   if ( trim(MEM_shelf(i_s))==shelf ) then
     i_SHELF=i_s
     exit 
   endif
 enddo 
 i_LIB  =MEM_LIB_finder(member)
 !
 if (i_LIB==0) then
   !
   N_MEM_LIBs=N_MEM_LIBs+1
   !
   if (i_SHELF==0) then
     N_MEM_shelf=N_MEM_shelf+1
     MEM_shelf(N_MEM_shelf)="["//shelf//"]"
     i_SHELF=N_MEM_shelf
   endif
   !
   MEM_LIBs(N_MEM_LIBs)%name =member
   MEM_LIBs(N_MEM_LIBs)%shelf=i_SHELF
   !
 endif
 !
end subroutine
