!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine DIPOLE_ALLOC_elemental(what,d)
 !
 use DIPOLES,        ONLY:DIP_iR,DIP_v,DIP_P,DIP_orbital,DIP_spin,DIP_P_spinor,P_square,DIP_S
 use electrons,      ONLY:n_sp_pol,n_spinor
 use pars,           ONLY:cZERO
 !
#include <y_memory.h>
 !
 character(*)                :: what
 integer, optional           :: d(:)
 !
 if (.not.present(d)) then
   select case(what)
   case('DIP_iR')
     if (.not.allocated(DIP_iR)) return
     YAMBO_FREE(DIP_iR)
   case('DIP_P')
     if (.not.allocated(DIP_P)) return
     YAMBO_FREE(DIP_P)
   case('DIP_v')
     if (.not.allocated(DIP_v)) return
     YAMBO_FREE(DIP_v)
   case('DIP_S')
     if (.not.allocated(DIP_S)) return
     YAMBO_FREE(DIP_S)
   case('DIP_orbital')
     if (.not.allocated(DIP_orbital)) return
     YAMBO_FREE(DIP_orbital)
   case('DIP_spin')
     if (.not.allocated(DIP_spin)) return
     YAMBO_FREE(DIP_spin)
   case('DIP_P_spinor')
     if (.not.allocated(DIP_P_spinor)) return
     YAMBO_FREE(DIP_P_spinor)
   case('P_square')
     if (.not.allocated(P_square)) return
     YAMBO_FREE(P_square)
   end select
   !
   return
   !
 endif
 !
 select case(what)
   case('DIP_iR')
     if (allocated(DIP_iR)) return
     YAMBO_ALLOC(DIP_iR,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol))
     DIP_iR=cZERO
   case('DIP_P')
     if (allocated(DIP_P)) return
     YAMBO_ALLOC(DIP_P,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol))
     DIP_P=cZERO
   case('DIP_v')
     if (allocated(DIP_v)) return
     YAMBO_ALLOC(DIP_v,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol))
     DIP_v=cZERO
   case('DIP_S')
     if (allocated(DIP_S)) return
     YAMBO_ALLOC(DIP_S,(d(1),d(2),d(3),d(4),n_sp_pol))
     DIP_S=cZERO
   case('DIP_orbital')
     if (allocated(DIP_orbital)) return
     YAMBO_ALLOC(DIP_orbital,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol,d(7)))
     DIP_orbital=cZERO
   case('DIP_spin')
     if (allocated(DIP_spin)) return
     YAMBO_ALLOC(DIP_spin,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol,3))
     DIP_spin=cZERO
   case('DIP_P_spinor')
     if (allocated(DIP_P_spinor).or.n_spinor==1) return
     YAMBO_ALLOC(DIP_P_spinor,(d(1),n_spinor,n_spinor,d(2):d(3),d(4):d(5),d(6)))
     DIP_P_spinor=cZERO
   case('P_square')
     if (allocated(P_square)) return
     YAMBO_ALLOC(P_square,(d(1):d(2),d(3):d(4),d(5),n_sp_pol))
     P_square=cZERO
 end select
 !
end subroutine
