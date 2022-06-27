!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AC
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
!> @brief Return phonon energy in the double-grid
!
! @param[in]       q                   momenutm sampling
! @param[in]       il                  branch index
! @param[in]       iq_fg               index in the fine grid
! @param[in]       iq_bz               index in the BZ of the course grid
!
! @param[out]      ph_E                phonon-energy
!
function get_ph_E_DbGd(il,iq_fg,iq_bz,q)
 !
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp
 use ELPH,          ONLY:PH_freqs,FAN_deltaE_treshold,PH_freqs_sq
 !
#include<memory.h>
 !
 integer,       intent(in)  :: il,iq_bz,iq_fg
 type(bz_samp), intent(in)  :: q
 !
 ! work space
 !
 integer  :: iE_fg
 real(SP) :: get_ph_E_DbGd
 !
 if(iq_fg==q%FGbz%k_range(iq_bz,1)) then
   !      
   ! the first point correspond to the point in the course grid
   !
   get_ph_E_DbGd=sqrt(abs(PH_freqs_sq(iq_bz,il)))
   !
   !  if iq==1 
   !  I read ph-energies calculated with matdyn.x 
   !  that includes the LO-TO splitting and the acustic sum rule
   !
   if(iq_bz==1) get_ph_E_DbGd=PH_freqs%FG%E(il,1,1)
   !
 else
   !      
   ! Double-grid energies
   !
   iE_fg  =q%FGbz%E_map(iq_fg)                             ! Map w(q) from BZ to IBZ
   get_ph_E_DbGd =PH_freqs%FG%E(il,iE_fg,1)                ! w(q_db) phonon energy in the D-Grid
   !
 endif
 !
end function get_ph_E_DbGd

