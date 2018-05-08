!
!        Copyright (C) 2000-2018 the YAMBO team
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
&                         RT_all2ibz,RT_Nk_ibz,l_RT_uses_E_DbGd,RT_bands,RT_DbGd_nk,RT_nk,RT_PAR_nk
 use R_lattice,      ONLY:bz_samp
 use parallel_m,     ONLY:PAR_Xk_nibz,PAR_IND_Xk_ibz,PAR_RT_k_index,PAR_Xk_ibz_index
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
 ! GPL_EXCLUDE_START
 l_RT_uses_E_DbGd=BZ_DbGd_tot_nk_levels>0.and.BZ_DbGd_nbands>=RT_bands(2).and..not.DbGd_is_symmetry_expanded
 ! GPL_EXCLUDE_END
 ! GPL_INCLUDE_START
 !l_RT_uses_E_DbGd=.FALSE.
 ! GPL_INCLUDE_END
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
   RT_PAR_nk=PAR_Xk_nibz
   RT_nk    =k%nibz
   !
   YAMBO_ALLOC(PAR_RT_k_index,(k%nibz))
   PAR_RT_k_index=PAR_Xk_ibz_index
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
 ! GPL_EXCLUDE_START
 !
 ! DbGd support
 !=============
 !
 RT_DbGd_nk=BZ_DbGd_tot_nk_levels
 !
 RT_nk    =0
 !
 do ik=1,k%nibz
   !
   ik_bz=1+sum(k%nstar(:ik-1))
   DbGd_ibz_coo(ik,:)=(/sum(BZ_DbGd_nkpt(:ik_bz-1))+1,sum(BZ_DbGd_nkpt(:ik_bz))/)
   RT_ibz_coo(ik,:)=(/RT_nk+1,RT_nk+BZ_DbGd_nkpt(ik_bz)/)
   RT_Nk_ibz(ik)=BZ_DbGd_nkpt(ik_bz)
   RT_nk=RT_nk+BZ_DbGd_nkpt(ik_bz)
   !
 enddo
 !
 do ik=1,k%nbz
   DbGd_bz_coo(ik,:)=(/sum(BZ_DbGd_nkpt(:ik-1))+1,sum(BZ_DbGd_nkpt(:ik))/)
 enddo
 !
 YAMBO_ALLOC(RT_all2ibz,(RT_nk))
 YAMBO_ALLOC(PAR_RT_k_index,(RT_nk))
 PAR_RT_k_index=0
 RT_PAR_nk=0
 do ik=1,k%nibz
   do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     RT_all2ibz(ik_rim)    =ik
   enddo
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     RT_PAR_nk=RT_PAR_nk+1
     PAR_RT_k_index(ik_rim)=RT_PAR_nk
   enddo
 enddo
 !
 if( l_elel_scatt .and. (index(what_is_running,'YPP')==0) ) then
   !
   if(.not.allocated(DbGd_EE_scatt_table)) then
     YAMBO_ALLOC(DbGd_EE_scatt_table,(RT_nk,RT_nk))
     YAMBO_ALLOC(DbGd_EE_Nk_table,(RT_nk,k%nibz))
   endif
   !
   l_check=.false.
   DbGd_EE_scatt_table=.false.
   DbGd_EE_Nk_table=0
   !
   do iqbz=1,q%nbz
     do ik=1,k%nibz
       !
       ik_m_q= k%sstar(qindx_S(ik,iqbz,1),1)
       !
       if(l_check(ik,ik_m_q)) cycle
       if(l_check(ik_m_q,ik)) cycle
       !
       Nk_rim=RT_Nk_ibz(ik)
       Nk_m_q_rim=RT_Nk_ibz(ik_m_q)
       !
       if( Nk_rim<Nk_m_q_rim ) cycle
       !
       l_check(ik,ik_m_q)=.true.
       l_check(ik_m_q,ik)=.true.
       !
       N_pts=max(1,int(real(Nk_m_q_rim)*DbGd_EE_percent/100._SP))
       N_loops=max(1, (Nk_rim*N_pts-1)/(Nk_m_q_rim) )
       !
       i_loops=1
       ik_m_q_rim=RT_ibz_coo(ik_m_q,1)-1
       !
       do while (i_loops< N_loops+1)
         do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
           !
           do i1=1,N_pts
             ik_m_q_rim=ik_m_q_rim+1
             if(ik_m_q_rim>RT_ibz_coo(ik_m_q,2)) then
               ik_m_q_rim=RT_ibz_coo(ik_m_q,1)
               i_loops=i_loops+1
             endif
             !
             DbGd_EE_scatt_table(ik_m_q_rim,ik_rim)=.true.
             DbGd_EE_Nk_table(ik_rim,ik_m_q)=DbGd_EE_Nk_table(ik_rim,ik_m_q)+1
             !
             DbGd_EE_scatt_table(ik_rim,ik_m_q_rim)=.true.
             DbGd_EE_Nk_table(ik_m_q_rim,ik)=DbGd_EE_Nk_table(ik_m_q_rim,ik)+1
             !
           enddo
           !
           if(ik_m_q_rim==RT_ibz_coo(ik_m_q,2)) i_loops=i_loops+1
           !
         enddo
         !
       enddo
       !
     enddo
   enddo
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
end subroutine RT_k_grid
