! 
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine electrons_dos_setup( E, dos_E , bands )
 !
 use pars,           ONLY:SP
 use YPPm,           ONLY:DOS_E_step,DOS_E_range,DOS_E_steps,DOS_broadening
 use electrons,      ONLY:levels
 !
 implicit none
 !
 type(levels),  intent(in) ::E
 integer,       intent(in) ::bands(2)
 real(SP),      intent(out)::dos_E(DOS_E_steps)
 !
 ! Work Space
 ! 
 integer               :: i_E
 !
 ! Energy Range
 !==============
 if (DOS_E_range(1)>DOS_E_range(2)) then
   DOS_E_range(1)=minval(E%E(bands(1):bands(2),:,:))-5._SP*DOS_broadening
   DOS_E_range(2)=maxval(E%E(bands(1):bands(2),:,:))+5._SP*DOS_broadening
 endif
 DOS_E_step=(DOS_E_range(2)-DOS_E_range(1))/DOS_E_steps
 !
 do i_E=1,DOS_E_steps
   dos_E(i_E)=DOS_E_range(1)+real(i_E,SP)*DOS_E_step
 enddo
 !
end subroutine
