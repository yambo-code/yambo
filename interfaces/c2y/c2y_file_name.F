!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
integer function c2y_file_name(inf,file_name_)
 !
 use pars,             ONLY:lchlen
 implicit none
 character(*)              :: inf
 character(lchlen)         :: file_name_
 !
 logical, external  :: file_exists
 !
 ! Work Space
 !
 c2y_file_name=0
 !
 file_name_=inf
 if (file_exists (file_name_) ) return
 !
 c2y_file_name=-1
 !
end function
