!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
! Excitonic occupation, following PRB 99, 081109(R) 2019
!
subroutine EXC_occupation(E_QF,EXC_n_states,EXC_indexes,EXC_occ)
 !
 use pars,           ONLY:SP,cZERO,rZERO,cONE
 use electrons,      ONLY:levels
 !
#include<memory.h>
 !
 ! Work space
 !
 type(levels), intent(in)  :: E_QF
 integer,      intent(in)  :: EXC_n_states
 integer,      intent(in)  :: EXC_indexes(EXC_n_states)
 real(SP),     intent(out) :: EXC_occ(EXC_n_states)
 !
 EXC_occ=rZERO
 !
end subroutine
