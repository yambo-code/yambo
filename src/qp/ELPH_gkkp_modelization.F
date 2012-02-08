!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine ELPH_gkkp_modelization( )
 !
 use R_lattice,           ONLY:nqbz
 use ELPH,                ONLY:ph_modes,elph_gkkp,elph_use_q_grid,elph_nDBs_used
 use IO_m,                ONLY:io_control,OP_RD,RD_CL_IF_END,REP
 use timing,              ONLY:live_timing
 !
 implicit none
 !
 integer  :: io_err,ID,iq,NQ
 integer, external::ioELPH
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
 io_err=ioELPH(ID,'gkkp')
 !
 NQ=elph_nDBs_used
 if (elph_use_q_grid) NQ=nqbz
 !
 call live_timing('Q-loop',NQ,SERIAL=.TRUE.)
 !
 do iq=1,NQ
   !
   ! I/O
   !
   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
   io_err=ioELPH(ID,'gkkp') 
   !
 enddo
 !
end subroutine
