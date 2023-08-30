!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
logical function ver_is_gt_or_eq(ID,version,revision)
 !
 use IO_m,        ONLY:io_code_version,io_code_revision
 use C_driver,    ONLY:code_version,code_revision
 !
 implicit none
 !
 integer, intent(in), optional     :: revision,version(3)
 integer, intent(in)  :: ID
 integer              :: revision2comp,version2comp(3)
 !
 ver_is_gt_or_eq=.false.
 !
 if (ID>0) version2comp =io_code_version(ID,:)
 if (ID>0) revision2comp=io_code_revision(ID,1)
 if (ID<0) version2comp =code_version
 if (ID<0) revision2comp=code_revision
 !
 if (present(version)) then
   if (version2comp(1)<version(1)) return
   if (version2comp(1)==version(1).and.version2comp(2)<version(2)) return
   if (version2comp(1)==version(1).and.version2comp(2)==version(2)&
&      .and.version2comp(3)<version(3)) return
 endif
 !
 if (present(revision)) then
   if (revision2comp<revision) return
 endif
 !
 ver_is_gt_or_eq=.true.
 !
end function
