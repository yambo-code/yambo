!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine DIPOLE_dimensions(Xen,Dip,bands,q0)
 !
 use drivers,        ONLY:l_col_cut
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use DIPOLES,        ONLY:DIPOLE_t
 use X_m,            ONLY:l_X_terminator,X_terminator_Kind
 use wave_func,      ONLY:wf_ng
 !
 implicit none
 !
 integer,       intent(in)    :: bands(2)
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
 real(SP),      intent(in)    :: q0(3)
 !
 !
 ! NOTE: 1 - that in case we will enter here during a SC run to update the
 !           screened interaction all transitions must be considered in order to rotate <P>.
 !       2 - when using internal SC potentials oscilators must be rotated
 !           in the new basis. So all transitions are needed.
 !       3 - In real-time simulations P and P^2 are ALWAYS calculated => no band ordering
 !
 Dip%ng=wf_ng
 Dip%q0=q0
 Dip%ib=bands
 !
 ! Check if terminator is required
 !
 l_X_terminator = X_terminator_Kind/='none'
 !
 ! Set up band limits
 !
 if (Dip%bands_ordered.or.Dip%Energy_treshold<0._SP) then
   Dip%ib_lim(1)=maxval(Xen%nbm)
   Dip%ib_lim(2)=minval(Xen%nbf)+1
   if (l_X_terminator) Dip%ib_lim(2)=Dip%ib(1)
 else
   Dip%ib_lim(1)=Dip%ib(2)
   Dip%ib_lim(2)=Dip%ib(1)
 endif
 !
end subroutine DIPOLE_dimensions
