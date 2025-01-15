!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine DIPOLE_ALLOC_global(D,NK)
 !
 ! Here all DIPOLE observables are allocated except the S_overlap matrix
 !
 use DIPOLES,        ONLY:DIP_orbital,DIPOLE_t
 use ALLOC,          ONLY:DIPOLE_ALLOC_elemental
 use stderr,         ONLY:STRING_match
 !
 implicit none
 !
 type(DIPOLE_t),intent(inout), optional :: D
 integer                     , optional :: NK
 !
 if (.not.present(D)) then
   call DIPOLE_ALLOC_elemental('DIP_iR')
   call DIPOLE_ALLOC_elemental('DIP_v')
   call DIPOLE_ALLOC_elemental('DIP_P')
   call DIPOLE_ALLOC_elemental('DIP_orbital')
   call DIPOLE_ALLOC_elemental('DIP_spin')
   call DIPOLE_ALLOC_elemental('DIP_P_spinor')
   call DIPOLE_ALLOC_elemental('DIP_S')
   call DIPOLE_ALLOC_elemental('P_square') 
   return
 endif
 !
 call DIPOLE_ALLOC_elemental('DIP_iR',(/3,D%ib_lim(2),D%ib(2),D%ib(1),D%ib_lim(1),NK/))
 call DIPOLE_ALLOC_elemental('DIP_v' ,(/3,D%ib_lim(2),D%ib(2),D%ib(1),D%ib_lim(1),NK/))
 call DIPOLE_ALLOC_elemental('DIP_P' ,(/3,D%ib_lim(2),D%ib(2),D%ib(1),D%ib_lim(1),NK/))
 !
 if (STRING_match(D%computed,"M_orb").and.STRING_match(D%computed,"M_it")) then
   call DIPOLE_ALLOC_elemental('DIP_orbital',(/3,D%ib_lim(2),D%ib(2),D%ib(1),D%ib_lim(1),NK,2/))
 endif
 !
 if ((STRING_match(D%computed,"M_orb").or.STRING_match(D%computed,"M_CD_orb")).and.&
&     .not.STRING_match(D%computed,"M_it")) then
   call DIPOLE_ALLOC_elemental('DIP_orbital',(/3,D%ib_lim(2),D%ib(2),D%ib(1),D%ib_lim(1),NK,1/))
 endif
 !
 if (STRING_match(D%computed,"M_spin")) then
   call DIPOLE_ALLOC_elemental('DIP_spin',(/3,D%ib_lim(2),D%ib(2),D%ib(1),D%ib_lim(1),NK/))
 endif
 if (STRING_match(D%computed,"P_spinor")) then
   call DIPOLE_ALLOC_elemental('DIP_P_spinor',(/3,D%ib_lim(2),D%ib(2),D%ib(1),D%ib_lim(1),NK/))
 endif
 !
 if (STRING_match(D%computed,"P2")) then
   call DIPOLE_ALLOC_elemental('P_square',(/D%ib_lim(2),D%ib(2),D%ib(1),D%ib_lim(1),NK/))
 endif
 !
end subroutine
