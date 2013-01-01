!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine K_eval_scattering_amplitudes( )
 !
 ! This routine evaluates the scattering oscillators
 !
 !  <n k1_bz |exp iG.r|m k2_bz> = <n k1_ibz s1 |exp iG.r|m k2_ibz s2>  =
 !                                <n k1_ibz |exp iG.r|m k2_ibz s1^{-1} x s2 > 
 !
 use BS,            ONLY:O_n_scatt,BS_O,O_ng,Scattering_table
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 implicit none
 !
 ! Work Space
 !
 type(ggwinfo)   ::isc
 type(pp_indexes)::p_index
 integer         ::is,ib1,ib2,i_spin,i_scatt,ik1,ik2,iq
 !
 call pp_indexes_reset(p_index)
 call collision_reset(isc)
 !
 isc%ngrho=O_ng
 !
 allocate(isc%rhotw(O_ng))
 !
 BS_O=(0.,0.)
 !
 call pp_redux_wait
 !
 call par_indexes(p_index,(/O_n_scatt/))
 !
 do i_scatt=1,O_n_scatt
   !
   if (.not.p_index%element_1D(i_scatt)) cycle
   !
   ik1    =Scattering_table(i_scatt,1)
   ib1   = Scattering_table(i_scatt,2)
   ik2   = Scattering_table(i_scatt,3)
   ib2   = Scattering_table(i_scatt,4)
   is    = Scattering_table(i_scatt,5)
   i_spin= Scattering_table(i_scatt,6)
   iq    = Scattering_table(i_scatt,7)
   !
   isc%qs=(/1,iq,1/) 
   isc%is=(/ib1,ik1,1,i_spin/)
   isc%os=(/ib2,ik2,is,i_spin/)
   call scatterBamp(isc)
   BS_O(:,i_scatt)=isc%rhotw 
   !
 enddo
 !
 do i_scatt=1,O_n_scatt
   call pp_redux_wait(BS_O(:,i_scatt))
 enddo
 !
 call pp_indexes_reset(p_index)
 call collision_reset(isc)
 !
end subroutine
