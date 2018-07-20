!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine RT_occ_bands_kpts_and_En_map(En,kpt,E_bands,K_bands)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels,E_reset,n_sp_pol
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use real_time,      ONLY:RT_carriers
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: kpt
 type(levels) , intent(in)  :: En
 type(bz_samp), intent(out) :: K_bands
 type(levels) , intent(out) :: E_bands
 !
 integer        :: ik,ib,i_sp_pol,i_RT
 !
 call bz_samp_reset(K_bands)  
 !
 K_bands%units='i'
 K_bands%description='k'
 K_bands%nibz=RT_carriers%nk
 K_bands%nbz =RT_carriers%nk
 YAMBO_ALLOC(K_bands%pt,(K_bands%nibz,3))
 YAMBO_ALLOC(K_bands%weights,(K_bands%nibz))
 do ik=1,K_bands%nibz
   K_bands%pt(ik,:)   =RT_carriers%kpt(ik,:)
   K_bands%weights(ik)=RT_carriers%k_weight(ik)
 enddo
 !
 call k_build_up_BZ_tables(K_bands)
 call k_ibz2bz(K_bands,'iku',.TRUE.)
 !
 call E_reset(E_bands) 
 !
 E_bands%nb=RT_carriers%nb(2)
 E_bands%nk=RT_carriers%nk
 !
 E_bands%nbf=En%nbf
 E_bands%nbm=En%nbm
 !
 E_bands%E_Fermi  =En%E_Fermi
 E_bands%E_VBM    =En%E_VBM
 E_bands%E_CBm    =En%E_CBm
 E_bands%E_dir_gap=En%E_dir_gap
 !
 YAMBO_ALLOC(E_bands%E,(E_bands%nb,E_bands%nk,n_sp_pol))
 YAMBO_ALLOC(E_bands%f,(E_bands%nb,E_bands%nk,n_sp_pol))
 E_bands%E=0._SP
 E_bands%f=0._SP
 !
 do i_RT=1,RT_carriers%nstates
   !
   ib      =RT_carriers%table(i_RT,1)
   !ik_small_grid=RT%table(i_RT,2)
   ik      =RT_carriers%table(i_RT,3)
   i_sp_pol=RT_carriers%table(i_RT,4)
   !
   E_bands%E(ib,ik,i_sp_pol)=RT_carriers%E_bare(i_RT)
   E_bands%f(ib,ik,i_sp_pol)=RT_carriers%f_bare(i_RT)
   !
 enddo
 !
 return
 !
end subroutine RT_occ_bands_kpts_and_En_map
