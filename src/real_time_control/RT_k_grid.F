!
!        Copyright (C) 2000-2015 the YAMBO team
!            http://www.yambo-code.org
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
integer function RT_k_grid(E,k,q)
 !
 !       K(BZ)                                                    K(IBZ)
 !               E_RIM(RIM_bz_coo(ikbz,:)=RIM_ibz_coo(ik,:))
 !        ____                                                     ____
 !ikbz1  | 1  | RIM_bz_coo(ikbz1,:) = RIM_ibz_coo(ik1,:)      ik1 | 1  | RT_ibz_coo(ik1,:)     
 !       |____|                     <----------------------       |____|   
 !        ____                                                     ____
 !ikbz2  | 2  | RIM_bz_coo(ikbz2,:) = 1                       ik2 | 2  | RT_ibz_coo(ik2,:)     
 !       |____|                                             /     |____|   
 !        ____                                             /
 !ikbz3  | 3  | RIM_bz_coo(ikbz3,:) = RIM_ibz_coo(ik2,:)  /            
 !       |____|                      <-------------------/        
 ! 
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elel_scatt
 use R_lattice,      ONLY:qindx_S
 use LIVE_t,         ONLY:what_is_running
 use electrons,      ONLY:levels,BZ_RIM_nkpt
 use real_time,      ONLY:RIM_bz_coo,RIM_ibz_coo,RIM_EE_scatt_table,&
&                         RIM_EE_percent,RIM_EE_Nk_table,RT_ibz_coo,&
&                         RT_all2ibz,RT_Nk_ibz
 use R_lattice,      ONLY:bz_samp
 !
 implicit none
 !
 type(levels), intent(in) :: E
 type(bz_samp),intent(in) :: k
 type(bz_samp),intent(in) :: q
 !
 ! Work space
 !
 logical :: l_check(k%nibz,k%nibz)
 integer :: ik,ik_bz,ik_rim,iqbz,ik_m_q,ik_m_q_rim,i_loops,N_loops,Nk_rim,Nk_m_q_rim,i1,N_pts
 !
 ! Number of K-points
 !====================
 !
 RT_k_grid=0
 !
 if (.not.allocated(BZ_RIM_nkpt)) return
 !
 if (.not.allocated(RIM_ibz_coo)) then
   allocate(RIM_ibz_coo(k%nibz,2))
   allocate(RIM_bz_coo(k%nbz,2))
   allocate(RT_ibz_coo(k%nibz,2))
   allocate(RT_Nk_ibz(k%nibz))
 endif
 !
 do ik=1,k%nibz
   ik_bz=1+sum(k%nstar(:ik-1))
   RIM_ibz_coo(ik,:)=(/sum(BZ_RIM_nkpt(:ik_bz-1))+1,sum(BZ_RIM_nkpt(:ik_bz))/)
   RT_ibz_coo(ik,:)=(/RT_k_grid+1,RT_k_grid+BZ_RIM_nkpt(ik_bz)/)
   RT_k_grid=RT_k_grid+BZ_RIM_nkpt(ik_bz)
   RT_Nk_ibz(ik)=BZ_RIM_nkpt(ik_bz)
 enddo
 do ik=1,k%nbz
   RIM_bz_coo(ik,:)=(/sum(BZ_RIM_nkpt(:ik-1))+1,sum(BZ_RIM_nkpt(:ik))/)
 enddo
 allocate(RT_all2ibz(RT_k_grid))
 do ik=1,k%nibz
   do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     RT_all2ibz(ik_rim)=ik
   enddo
 enddo
 !
 if( l_elel_scatt .and. (index(what_is_running,'YPP')==0) ) then
   !
   if(.not.allocated(RIM_EE_scatt_table)) then
     allocate(RIM_EE_scatt_table(RT_k_grid,RT_k_grid))
     allocate(RIM_EE_Nk_table(RT_k_grid,k%nibz))
   endif
   !
   l_check=.false.
   RIM_EE_scatt_table=.false.
   RIM_EE_Nk_table=0
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
       N_pts=max(1,int(real(Nk_m_q_rim)*RIM_EE_percent/100._SP))
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
             RIM_EE_scatt_table(ik_m_q_rim,ik_rim)=.true.
             RIM_EE_Nk_table(ik_rim,ik_m_q)=RIM_EE_Nk_table(ik_rim,ik_m_q)+1
             !
             RIM_EE_scatt_table(ik_rim,ik_m_q_rim)=.true.
             RIM_EE_Nk_table(ik_m_q_rim,ik)=RIM_EE_Nk_table(ik_m_q_rim,ik)+1
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
end function RT_k_grid
