!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elel_scatt
 use R_lattice,      ONLY:qindx_S
 use C_driver,       ONLY:code_bin
 use electrons,      ONLY:levels
 use real_time,      ONLY:DbGd_EE_scatt_table,&
&                         DbGd_EE_percent,DbGd_EE_Nk_table,RT_ibz_coo,RT_ibz_dupl,&
&                         RT_all2ibz,RT_Nk_ibz,l_RT_uses_E_FineGd,RT_bands,RT_nk
 use R_lattice,      ONLY:bz_samp
 use parallel_m,     ONLY:PAR_IND_Xk_ibz
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
 integer :: ik,ik_rim,iqbz,ik_m_q,ik_m_q_rim,i_loops,N_loops,Nk_rim,Nk_m_q_rim,i1,N_pts
 ! 
 ! GPL_EXCLUDE_START
 !
 l_RT_uses_E_FineGd=k%FGibz%N>0.and.E%nb_FineGd>=RT_bands(2)
 !
 ! GPL_EXCLUDE_END
 !
 ! GPL_INCLUDE_START
 !
 !l_RT_uses_E_DbGd=.FALSE.
 !
 ! GPL_INCLUDE_END
 !
 if (.not.allocated(RT_ibz_coo)) then
   YAMBO_ALLOC(RT_ibz_coo,(k%nibz,2))
   YAMBO_ALLOC(RT_ibz_dupl,(k%nibz))
   YAMBO_ALLOC(RT_Nk_ibz,(k%nibz))
 endif
 !
 ! No DbGd, trivial definitions
 !==============================
 if (.not.l_RT_uses_E_FineGd) then
   !
   RT_nk    =k%nibz
   !
   RT_ibz_dupl=0
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
 ! RT_nk            -> k%FGibz%N
 ! RT_ibz_coo(ik,:) -> k%FGibz%k_map(ik,:)
 ! RT_all2ibz(i_fg) -> k%FGibz%k_map_m1(i_fg)
 ! RT_Nk_ibz(ik)    -> k%FGibz%N_mapped(ik)
 !
 RT_nk = 0
 do ik=1,k%nibz
   RT_ibz_coo(ik,:)=(/RT_nk+1,RT_nk+1+k%FGibz%N_mapped(ik)/)
   RT_nk=RT_nk+1+k%FGibz%N_mapped(ik)
   RT_Nk_ibz(ik)=k%FGibz%N_mapped(ik)+1
   if(ik==1) RT_ibz_dupl(ik)=k%FGibz%k_map(1,ik)-1
   if(ik> 1) RT_ibz_dupl(ik)=k%FGibz%k_map(1,ik)-k%FGibz%k_map(2,ik-1)-1
 enddo
 !
 ! Define K-weights to be used in the k-points distribution
 !
 YAMBO_ALLOC(PAR_IND_Xk_ibz%weight_1D,(k%nibz))
 PAR_IND_Xk_ibz%weight_1D=RT_Nk_ibz
 !
 YAMBO_ALLOC(RT_all2ibz,(RT_nk))
 do ik=1,k%nibz
   do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     RT_all2ibz(ik_rim)    =ik
   enddo
 enddo
 !
 if( l_elel_scatt .and. (index(code_bin,'ypp')==0) ) then
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
   do ik=1,k%nibz
     do iqbz=1,q%nbz
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
