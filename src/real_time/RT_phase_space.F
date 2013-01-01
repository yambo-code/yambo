!
!  Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!            http://www.yambo-code.org
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
integer function RT_phase_space(E)
 ! 
 use pars,           ONLY:zero_dfl,SP
 use drivers,        ONLY:l_ph_corr,l_carrier_dynamics 
 use electrons,      ONLY:levels,spin_occ
 use real_time,      ONLY:RT_eh_energy_max,RT_k_px
 use SC,             ONLY:SC_bands
 use R_lattice,      ONLY:nkibz
 use par_indexes_m,  ONLY:par_indexes
 use stderr,         ONLY:intc
 use com,            ONLY:warning
 use par_proc_m,     ONLY:pp_indexes_reset,pp_indexes
 !
 implicit none
 !
 type(levels), intent(in) :: E
 !
 ! Work space
 !
 integer           :: iv,ic,ib,ik,nk_todo,ik_todo,SC_bands_new(2)
 real(SP)          :: E_eh,f_eh
 logical           :: RT_b(SC_bands(2),nkibz),RT_k_point(nkibz)
 type(pp_indexes)  :: p_loc
 !
 RT_b       =.FALSE.
 RT_k_point =.FALSE.
 !
 ! Pre-allocation of CPU tables
 !
 call pp_indexes_reset(RT_k_px)
 !
 call par_indexes(RT_k_px,(/nkibz/))
 ! 
 if (RT_eh_energy_max<zero_dfl) then
   !
   RT_phase_space=nkibz
   !
   ! In the case of el-ph scattering the parallelization is already done on the
   ! q-points.
   !
   if (l_ph_corr.or.l_carrier_dynamics) RT_k_px%element_1D=.TRUE.
   !
   return
   !
 endif
 !
 do ik=1,nkibz
   !
   do iv=SC_bands(1),E%nbm
     !
     do ic=E%nbf+1,SC_bands(2)
       !
       f_eh=E%f(iv,ik,1)*(spin_occ-E%f(ic,ik,1))
       !
       if (abs(f_eh)<zero_dfl    ) cycle
       !
       E_eh=E%E(ic,ik,1)-E%E(iv,ik,1)
       !
       if (E_eh<0.              ) cycle
       if (E_eh>RT_eh_energy_max) cycle
       !
       RT_b(iv,ik) =.TRUE.
       RT_b(ic,ik) =.TRUE.
       !
     enddo
     !
   enddo
   !
   RT_k_point(ik)=any(RT_b(:,ik))
   !
 enddo
 !
 nk_todo=count(RT_k_point)
 !
 call pp_indexes_reset(p_loc)
 !
 call par_indexes(p_loc,(/nk_todo/))
 !
 ik_todo=0
 !
 do ik=1,nkibz
   !
   if (RT_k_point(ik)) then
     !
     ik_todo=ik_todo+1
     !
     ! In the case of el-ph scattering the parallelization is already done on the
     ! q-points. Nevertheless the eventually reduced range of k-points remains
     ! defined for all CPUs.
     !
     if (p_loc%element_1D(ik_todo).or.l_ph_corr.or.l_carrier_dynamics) RT_k_px%element_1D(ik)=.TRUE.
     !
   else
     !
     RT_k_px%element_1D(ik)=.FALSE.
     !
   endif
   !
 enddo
 !
 call pp_indexes_reset(p_loc)
 !
 ! Redefine SC_bands ?
 !
 SC_bands_new=SC_bands
 !
 do ib=SC_bands(1),SC_bands(2)-1
   !
   if (.not.any(RT_b(ib,:)).and.     any(RT_b(ib+1,:))) SC_bands_new(1)=ib+1
   if (     any(RT_b(ib,:)).and..not.any(RT_b(ib+1,:))) SC_bands_new(2)=ib
   !
 enddo
 !
 RT_phase_space=nk_todo
 !
 if (any((/SC_bands/=SC_bands_new/))) then
   SC_bands =SC_bands_new
   call warning('Bands reduced to '//trim(intc(SC_bands(1)))//'-'//trim(intc(SC_bands(2))))
 endif
 ! 
end function RT_phase_space
