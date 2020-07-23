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
subroutine k_map_fine_to_coarse(zone,Xk,FG,FineGd_k,Xen)
 !
 use pars,                ONLY:SP
 use com,                 ONLY:msg
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,bz_fine_grid,bz_samp_FineGd_alloc
 use LIVE_t,              ONLY:live_timing
 !
#include<memory.h>
 !
 character(*)      , intent(in)    :: zone
 type(bz_fine_grid), intent(inout) :: FG
 type(bz_samp), intent(inout)      :: FineGd_k,Xk
 type(levels),  intent(inout)      :: Xen
 !
 ! Work Space
 !
 integer              :: ik_near,i_fg,i_c,N_coarse,is,ik,i_fg_k,i_fg_E,N_fine_max
 integer, allocatable :: TMP_map(:),TMP_table_mapped(:,:),N_duplicate,N_per_BZ_point(:)
 logical              :: l_BZ_FG,l_IBZ_FG,l_BZ_CG,l_IBZ_CG,l_E_FG
 !
 ! Initial mapping
 !=================
 !
 l_E_FG  =index(zone,"Energies-")>0
 l_IBZ_FG=index(zone,"IBZ-")>0
 l_BZ_FG =index(zone,"IBZ-")==0.and.index(zone,"Energies-")==0
 l_IBZ_CG=index(zone,"-IBZ")>0
 l_BZ_CG =index(zone,"-BZ")>0
 !
 if (l_BZ_FG ) FG%N = FineGd_k%nbz
 if (l_IBZ_FG.or.l_E_FG) FG%N = FineGd_k%nibz
 if (l_BZ_CG)  N_coarse = Xk%nbz
 if (l_IBZ_CG) N_coarse = Xk%nibz
 !
 N_fine_max=FG%N
 allocate(TMP_map(N_fine_max))
 !
 if (l_BZ_FG ) call k_map_fine_to_coarse_engine(zone,FineGd_k%ptbz,FineGd_k%nbz, Xk%ptbz,Xk%nbz,TMP_map)
 if (l_IBZ_FG.or.l_E_FG) call k_map_fine_to_coarse_engine(zone,FineGd_k%pt,FineGd_k%nibz,Xk%ptbz,Xk%nbz,TMP_map)
 !
 if (l_E_FG) then
   call bz_samp_FineGd_alloc(FG,N_coarse)
   call k_map_E_fine(FineGd_k,Xen,Xk,TMP_map)
   deallocate(TMP_map)
   return
 endif
 !
 allocate(N_per_BZ_point(Xk%nbz))
 do ik=1,Xk%nbz
   N_per_BZ_point(ik)=count(TMP_map(:)==ik)+count(TMP_map(:)==-ik)
 enddo
 !
 if (l_IBZ_CG) then
   ! 
   ! IBZ => BZ reduction
   !
   do i_fg=1,FG%N
     ik=Xk%sstar( iabs(TMP_map(i_fg)),1 )
     is=Xk%sstar( iabs(TMP_map(i_fg)),2 )
     if (is> 1) TMP_map(i_fg)=0
     if (is==1.and.TMP_map(i_fg)>0) TMP_map(i_fg)=ik
     if (is==1.and.TMP_map(i_fg)<0) TMP_map(i_fg)=-ik
   enddo
   !
 endif
 !
 ! Update the value of FG%N
 !
 FG%N=count(TMP_map>0)
 !
 call msg('s',':: '//zone//' Fine Grid pts per Coarse grid BZ pt :',(/minval(N_per_BZ_point),maxval(N_per_BZ_point)/))
 if (FG%N/=N_fine_max) then
   call msg('s',':: '//zone//' Fine Grid points used - read        :',(/FG%N,N_fine_max/))
 endif
 !
 ! Allocation
 !============
 FG%N=FG%N+N_coarse
 !
 call bz_samp_FineGd_alloc(FG,N_coarse)
 !
 ! Number of points in each zone 
 !===============================
 FG%N_mapped=0
 N_duplicate=0
 do i_fg=1,N_fine_max
   if (TMP_map(i_fg)==0) cycle
   ik_near=iabs(TMP_map(i_fg))
   if (TMP_map(i_fg)>0) FG%N_mapped(ik_near)=FG%N_mapped(ik_near)+1
   if (TMP_map(i_fg)<0) N_duplicate=N_duplicate+1
 enddo
 !
 call msg('s',':: '//zone//' Duplicated points                   :',N_duplicate)
 call msg('s',':: '//zone//' Blocks filling (excl. coarse grid)  :',(/minval(FG%N_mapped),maxval(FG%N_mapped)/))
 !
 ! Table(s)
 !==========
 allocate(TMP_table_mapped(N_coarse,maxval(FG%N_mapped)))
 TMP_table_mapped=0
 !
 FG%N_mapped=0
 do i_fg=1,N_fine_max
   if (TMP_map(i_fg)==0) cycle
   ik_near=iabs(TMP_map(i_fg))
   if(TMP_map(i_fg)>0) then
     FG%N_mapped(ik_near)=FG%N_mapped(ik_near)+1
     TMP_table_mapped(ik_near,FG%N_mapped(ik_near))=i_fg
   endif
 enddo
 !
 ! Reordering and transfer 
 !=========================
 call live_timing(zone//' Double Grid Reordering',FG%N)
 !
 ! Final FineGd structure. All FG%N+N_coarse points are splitted in ik-groups
 !
 ! ik: FG%k_range(ik,1) .. FG%k_range(1,2)
 !
 !  ik=FG%k_range(ik,1)                     -> COARSE grid
 !  ik=FG%k_range(ik,1)+1...FG%k_range(1,2) -> FINE grid
 !
 ! i_fg = FG%k_range(ik,1) we have
 !
 !  - k%pt(ik,:)/k%ptbz(ik,:)
 !  - E%E( <bands> ,ik, <spin> )
 !
 ! For each i_fg \in FG%k_range(ik,1)+1..FG%k_range(ik,2) we have
 !
 ! - FG%pt(i_fg,:)
 ! - FG%weights(i_fg)
 ! - E%E_FineGd( <bands> , FG%E_map(i_fg), <spin> )
 !
 i_fg=0
 !
 do ik=1,N_coarse
   !
   i_fg=i_fg+1
   FG%k_range(ik,1)=i_fg
   !
   do i_c=1,FG%N_mapped(ik)
     !
     i_fg=i_fg+1
     !
     i_fg_k=TMP_table_mapped(ik,i_c)
     if (l_BZ_FG ) FG%pt(i_fg,:)   =FineGd_k%ptbz(i_fg_k,:)
     if (l_IBZ_FG) FG%pt(i_fg,:)   =FineGd_k%pt(i_fg_k,:)
     FG%k_map(i_fg)=ik
     !
     if (l_BZ_FG ) i_fg_E=FineGd_k%sstar(i_fg_k,1)
     if (l_IBZ_FG) i_fg_E=i_fg_k
     FG%E_map(i_fg)=i_fg_E
     !
     if (l_BZ_FG ) FG%weights(i_fg) =1./real(FG%N,SP)
     if (l_IBZ_FG) FG%weights(i_fg) =Xk%weights(ik)/real(FG%N_mapped(ik),SP)
     !
     call live_timing(steps=1)
     !
   enddo
   !
   FG%k_range(ik,2)=i_fg
   !
 enddo
 !
 call live_timing()
 !
 deallocate(TMP_map,TMP_table_mapped,N_per_BZ_point)
 !
end subroutine
