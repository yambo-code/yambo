!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine ELPH_Hamiltonian()
 !
 use ELPH,          ONLY:elph_global_alloc,elph_global_free
 use IO_m,          ONLY:io_control,OP_RD,REP,RD_CL_IF_END
 use com,           ONLY:error
 use R_lattice,     ONLY:nqibz
 !
 implicit none
 !
 integer           :: ID,io_err,iq
 integer, external :: ioELPH
 !
 call section('*','Correlation: Electron-Phonon Hamiltonian')
 !
 ! ELPH DB
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
 io_err=ioELPH(ID,'gkkp')
 if (io_err/=0) call error('El-Ph database not found')
 !
 call elph_global_alloc('gkkp')
 !
 do iq=1,nqibz
   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
   io_err=ioELPH(ID,'gkkp')
 enddo
 !
 call elph_global_free( )
 !
end subroutine
