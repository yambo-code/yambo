!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine ELPH_plot_gkkp(k,E,q)
 !
 use pars,                ONLY:SP,pi
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp
 use ELPH,                ONLY:PH_freqs_sq,elph_use_q_grid,ph_modes,GKKP,elph_nb
 use com,                 ONLY:msg
 use vec_operate,         ONLY:c2a
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_RD_CL,REP,deliver_IO_error_message,DUMP,OP_WR_CL
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 ! Work space
 !
 integer               :: ID,io_err
 integer, external     :: io_ELPH
 !
 call section('+','Plot GKKP functions')
 !
 ! Read phonon-frequencies in the ELPH-DB
 ! ======================================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1/),ID=ID)
 io_err=io_ELPH(ID,'gkkp')
 call deliver_IO_error_message(io_err,"GKKP")
 !
 if (io_err/=0) call error('El-Ph database not found')
 !
end subroutine
