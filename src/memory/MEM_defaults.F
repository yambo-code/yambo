!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine MEM_defaults( )
 !
 implicit none
 !
 call MEM_library("g_vec","RL")
 call MEM_library("minus_G","RL")
 call MEM_library("g_rot","RL")
 call MEM_library("bare_qpg","RL")
 !
end subroutine
