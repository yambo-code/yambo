!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine excitons_kinematics(Xk,eh,i_s)
 !
 use electrons,     ONLY:spin
 use R_lattice,     ONLY:bz_samp
 use YPPm,          ONLY:EXCITON_kin
 use BS_solvers,    ONLY:BSS_eh_table
 !
 implicit none
 !
 type(bz_samp)          :: Xk
 type(EXCITON_kin)      :: eh
 integer                :: i_s
 ! 
 eh%ik_bz   = BSS_eh_table(i_s,1)
 eh%ik_ibz  = Xk%sstar(eh%ik_bz,1)
 eh%ik_sym  = Xk%sstar(eh%ik_bz,2)
 eh%iv      = BSS_eh_table(i_s,2)
 eh%ic      = BSS_eh_table(i_s,3)
 eh%spin    =spin(BSS_eh_table(i_s,:))
 !
end subroutine
