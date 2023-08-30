!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine MATRIX_transfer_side_crop(I,J,O)
 !
 implicit none
 integer :: I(2),J(2),O(2)
 !
 O=0
 !
 if (I(2)<J(1)) return
 if (I(1)>J(2)) return
 !
 O(1)=I(1)
 if (I(1)<=J(1)) O(1)=J(1)
 !
 O(2)=J(2)
 if (I(2)<J(2)) O(2)=I(2)
 !
end subroutine
