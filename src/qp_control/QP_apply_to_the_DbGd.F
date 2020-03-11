!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine QP_apply_to_the_DbGd(bands,en,k)
 !
 use stderr,        ONLY:STRING_same
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use electrons,     ONLY:levels,n_sp_pol,E_reset,BZ_DbGd_kpts,DbGd_is_symmetry_expanded,&
&                        BZ_DbGd_nkpt,BZ_DbGd_table,BZ_DbGd_nbands,BZ_DbGd_tot_nk_levels,BZ_DbGd_tot_nk
 use QP_CTL_m,      ONLY:QP_ctl_ID,QP_ctl_DB_user
 use interpolate,   ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do
 use units
 !
#include<memory.h>
 !
 type(levels)    ::en
 type(bz_samp)   ::k
 integer         ::bands(2)
 !
 ! Work Space
 !
 integer         ::ik_bz,ik_r,i_spin,ib,ID,ik_r_bz,ik_r_ibz
 type(levels)    ::dE_dbgd
 type(bz_samp)   ::k_dbgd
 logical         ::NEAREST
 integer, allocatable ::DbGd_k_done(:)
 !
 call E_reset(dE_dbgd)
 call bz_samp_reset(k_dbgd)
 !
 NEAREST=.TRUE.
 if (STRING_same(trim(QP_ctl_DB_user(QP_ctl_ID)%INTERP_DbGd_mode),"BOLTZ")) NEAREST=.FALSE.
 !
 ! k_dbgd filling
 !
 k_dbgd%nibz=BZ_DbGd_tot_nk
 YAMBO_ALLOC(k_dbgd%pt,(BZ_DbGd_tot_nk,3))
 do ik_r_bz=1,BZ_DbGd_tot_nk
   k_dbgd%pt( ik_r_bz, :) = BZ_DbGd_kpts(ik_r_bz,:)
 enddo
 !
 ! Interpolation
 !
 call INTERPOLATION_driver_seed("dE_DbGd",ID,bands,k,SEED_E=en,NEAREST=NEAREST)
 call INTERPOLATION_driver_do("dE_DbGd",ID,k_dbgd,OUT_E=dE_dbgd)
 !
 YAMBO_FREE(k_dbgd%pt)
 !
 ! Interpolation 
 !
 YAMBO_ALLOC(DbGd_k_done,(BZ_DbGd_tot_nk_levels))
 DbGd_k_done=0
 !
 do ik_bz=1,k%nbz
   do ik_r=1,BZ_DbGd_nkpt(ik_bz)
     ik_r_ibz=BZ_DbGd_table(ik_bz,ik_r,1)
     if (.not.DbGd_is_symmetry_expanded) ik_r_bz =ik_r_ibz
     if (     DbGd_is_symmetry_expanded) ik_r_bz =BZ_DbGd_table(ik_bz,ik_r,2)
     if ( DbGd_k_done(ik_r_ibz) == 1) cycle
     do ib=bands(1),min(BZ_DbGd_nbands,bands(2))
       do i_spin=1,n_sp_pol
         en%E_DbGd(ib,ik_r_ibz,i_spin)=en%E_DbGd(ib,ik_r_ibz,i_spin)+ dE_dbgd%E(ib, ik_r_bz,i_spin)
       enddo
       DbGd_k_done(ik_r_ibz)=1
     enddo
   enddo
 enddo
 !
 YAMBO_FREE(DbGd_k_done)
 call E_reset(dE_dbgd)
 call bz_samp_reset(k_dbgd)
 !
end subroutine
