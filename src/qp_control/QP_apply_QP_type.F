!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
integer function QP_apply_QP_type(band_range,en,k,qp)
 !
 use QP_m,          ONLY:QP_t
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_sp_pol
 use QP_CTL_m,      ONLY:QP_ctl_t,reset_QP_ctl
 !
#include<memory.h>
 !
 type(levels)    ::en
 type(QP_t)      ::qp
 type(bz_samp)   ::k
 integer         ::band_range(2)
 !
 ! Work Space
 !
 logical         ::do_EWZ_using_Ik_IE(5) ! = ( E W Z INTERPOLATION/k INTERPOLATION/E )
 type(QP_ctl_t)  ::QP_ctl(n_sp_pol)
 integer         ::DB_corrected(en%nb,en%nk,n_sp_pol),i_spin
 !
 ! Presets
 do i_spin=1,n_sp_pol
   call reset_QP_ctl(QP_ctl(i_spin))
 enddo
 DB_corrected=0
 do_EWZ_using_Ik_IE=.false.
 do_EWZ_using_Ik_IE(1)=.true. ! Apply QP corrections to energies only
 QP_ctl(:)%db="none"
 QP_ctl(:)%short_descr="none"
 QP_ctl(:)%interp_neigh=1
 !
 ! Backup bare energies (only once)
 !
 if (.not.allocated(en%Eo)) then
   YAMBO_ALLOC(en%Eo,(en%nb,en%nk,n_sp_pol))
   en%Eo=en%E
 endif
 !
 ! Interpolation
 !
 call QP_apply_DB_interpolation(band_range,qp,en,k,do_EWZ_using_Ik_IE,DB_corrected,QP_ctl%interp_neigh)
 !
 ! Collect corrected flags and, eventually, switch off Extrapolation 
 ! 
 if (all(DB_corrected(band_range(1):band_range(2),:,:)==1)) return
 !
 ! If not all corrected FIT it
 !
 call QP_fit_DB_energies(band_range,qp,QP_ctl)
 !
end function
