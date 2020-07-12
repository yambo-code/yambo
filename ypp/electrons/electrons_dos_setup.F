! 
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
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
