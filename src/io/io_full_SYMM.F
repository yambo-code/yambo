!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
#include<dev_defs.h>
!
integer function io_full_SYMM(ID)
 !
 ! Store the full symmetries and the corresponding k-map
 ! before removing them with ypp -y 
 !
 use R_lattice,        ONLY:nkibz
 use nl_optics,        ONLY:dl_sop_eq,nsym_eq,kpts_map
 !
#include<y_memory.h>
 !
 integer, intent(in) :: ID
 !
 ! WorkSpace
 !
 io_full_SYMM =io_connect(desc="full_SYMM",type=0,ID=ID)
 if (io_full_SYMM/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
    call io_elemental(ID, VAR="DIMENSIONS",VAR_SZ=18)  
 endif
 !
 if (.not.any((/io_sec(ID,:)==2/))) goto 1
 !   
 if (read_is_on(ID)) then
    YAMBO_ALLOC(dl_sop_eq,(3,3,nsym_eq))
    YAMBO_ALLOC(kpts_map,(2,nkibz)
 endif
 !
end function io_full_SYMM
