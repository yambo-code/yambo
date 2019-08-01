!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_k_grid(E,k,q)
 !
 !       K(BZ)                                                    K(IBZ)
 !               E_DbGd(DbGd_bz_coo(ikbz,:)=DbGd_ibz_coo(ik,:))
 !        ____                                                       ____
 !ikbz1  | 1  | DbGd_bz_coo(ikbz1,:) = DbGd_ibz_coo(ik1,:)      ik1 | 1  | RT_ibz_coo(ik1,:)     
 !       |____|                       <----------------------       |____|   
 !        ____                                                       ____
 !ikbz2  | 2  | DbGd_bz_coo(ikbz2,:) = 1                        ik2 | 2  | RT_ibz_coo(ik2,:)     
 !       |____|                                               /     |____|   
 !        ____                                               /
 !ikbz3  | 3  | DbGd_bz_coo(ikbz3,:) = DbGd_ibz_coo(ik2,:)  /            
 !       |____|                        <-------------------/        
 ! 
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elel_scatt
 use R_lattice,      ONLY:qindx_S
 use LIVE_t,         ONLY:what_is_running
 use electrons,      ONLY:levels,BZ_DbGd_nkpt,BZ_DbGd_nbands,BZ_DbGd_tot_nk_levels,DbGd_is_symmetry_expanded
 use real_time,      ONLY:DbGd_bz_coo,DbGd_ibz_coo,DbGd_EE_scatt_table,&
&                         DbGd_EE_percent,DbGd_EE_Nk_table,RT_ibz_coo,&
&                         RT_all2ibz,RT_Nk_ibz,l_RT_uses_E_DbGd,RT_bands,RT_DbGd_nk,RT_nk
 use R_lattice,      ONLY:bz_samp
 !
#include<memory.h>
 !
 type(levels), intent(in)    :: E
 type(bz_samp),intent(in)    :: k
 type(bz_samp),intent(in)    :: q
 !
 ! Work space
 !
 logical :: l_check(k%nibz,k%nibz)
 integer :: ik,ik_bz,ik_rim,iqbz,ik_m_q,ik_m_q_rim,i_loops,N_loops,Nk_rim,Nk_m_q_rim,i1,N_pts
 ! 
 l_RT_uses_E_DbGd=.FALSE.
 !
 if (.not.allocated(DbGd_ibz_coo)) then
   YAMBO_ALLOC(DbGd_ibz_coo,(k%nibz,2))
   YAMBO_ALLOC(DbGd_bz_coo,(k%nbz,2))
   YAMBO_ALLOC(RT_ibz_coo,(k%nibz,2))
   YAMBO_ALLOC(RT_Nk_ibz,(k%nibz))
 endif
 !
 ! No DbGd, trivial definitions
 !==============================
 if (.not.l_RT_uses_E_DbGd) then
   !
   RT_nk    =k%nibz
   !
   RT_Nk_ibz=1
   DbGd_EE_percent=0._SP
   YAMBO_ALLOC(RT_all2ibz,(k%nibz))
   if(l_elel_scatt) then
     YAMBO_ALLOC(DbGd_EE_scatt_table,(k%nibz,q%nbz))
     YAMBO_ALLOC(DbGd_EE_Nk_table,(k%nibz,k%nibz))
     DbGd_EE_scatt_table=.true.
     DbGd_EE_Nk_table=1
   endif
   do ik=1,k%nibz
     DbGd_ibz_coo(ik,:)=ik
     RT_ibz_coo(ik,:)=ik
     RT_all2ibz(ik)=ik
   enddo
   !
   return
   !
 endif
 !
 !
end subroutine RT_k_grid
