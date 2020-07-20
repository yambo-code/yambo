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
subroutine k_map_fine_to_coarse(zone,Xk,Xen,FG,FineGd_k,FineGd_E,Reordering_map)
 !
 use com,                 ONLY:msg
 use electrons,           ONLY:levels,n_sp_pol
 use R_lattice,           ONLY:bz_samp,bz_fine_grid
 use LIVE_t,              ONLY:live_timing
 !
#include<memory.h>
 !
 character(*)      , intent(in)    :: zone
 type(bz_fine_grid), intent(inout) :: FG
 type(bz_samp), intent(inout)      :: FineGd_k,Xk
 type(levels),  intent(in)         :: FineGd_E
 type(levels),  intent(inout)      :: Xen
 integer,       intent(inout)      :: Reordering_map(FineGd_k%nibz)
 !
 ! Work Space
 !
 integer              :: ik_near,i_fg,i_c,N_coarse,is,ik
 integer, allocatable :: TMP_map(:),TMP_table_mapped(:,:),TMP_table_duplicate(:,:),N_duplicate(:)
 !
 ! Initial mapping
 !=================
 if (zone=="IBZ") then
   N_coarse=Xk%nibz
   FG%N    =FineGd_k%nibz
   !allocate(TMP_map(FineGd_k%nbz))
   !call k_map_fine_to_coarse_engine("BZ",FineGd_k%ptbz,FineGd_k%nbz,Xk%ptbz,Xk%nbz,TMP_map)
   allocate(TMP_map(FG%N))
   call k_map_fine_to_coarse_engine(zone,FineGd_k%pt,FineGd_k%nibz,Xk%ptbz,Xk%nbz,TMP_map)
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
 if (zone=="BZ" ) then
   N_coarse=Xk%nbz
   FG%N    =FineGd_k%nbz
   allocate(TMP_map(FG%N))
   call k_map_fine_to_coarse_engine(zone,FineGd_k%ptbz,FineGd_k%nbz,Xk%ptbz,Xk%nbz,TMP_map)
 endif
 !
 ! Number of points in each zone 
 !===============================
 allocate(FG%N_mapped(N_coarse),N_duplicate(N_coarse))
 FG%N_mapped=0
 N_duplicate=0
 do i_fg=1,FG%N
   ik_near=iabs(TMP_map(i_fg))
   if (ik_near==0) cycle
   if (TMP_map(i_fg)>0) FG%N_mapped(ik_near)=FG%N_mapped(ik_near)+1
   if (TMP_map(i_fg)<0) N_duplicate(ik_near)=N_duplicate(ik_near)+1
 enddo
 !
 FG%N_duplicate=sum(N_duplicate)
 !
 call msg('s',':: '//zone//' Duplicated points                   :',FG%N_duplicate)
 call msg('s',':: '//zone//' Blocks filling (excl. coarse grid)  :',(/minval(FG%N_mapped),maxval(FG%N_mapped)/))
 !
 ! Table(s)
 !==========
 allocate(TMP_table_mapped(N_coarse,maxval(FG%N_mapped)))
 TMP_table_mapped=0
 if(FG%N_duplicate>0) then
   allocate(TMP_table_duplicate(N_coarse,maxval(N_duplicate)))
   TMP_table_duplicate=0
 endif
 !
 FG%N_mapped=0
 N_duplicate=0
 do i_fg=1,FG%N
   ik_near=iabs(TMP_map(i_fg))
   if(TMP_map(i_fg)>0) then
     FG%N_mapped(ik_near)=FG%N_mapped(ik_near)+1
     TMP_table_mapped(ik_near,FG%N_mapped(ik_near))=i_fg
   else if(TMP_map(i_fg)<0) then
     N_duplicate(ik_near)=N_duplicate(ik_near)+1
     TMP_table_duplicate(ik_near,N_duplicate(ik_near))=i_fg
   endif
 enddo
 !
 deallocate(TMP_map)
 !
 ! Reordering and transfer 
 !=========================
 allocate(FG%pt(FG%N,3))
 allocate(FG%map(2,N_coarse))
 !
 if (zone=="IBZ") then
   allocate(Xen%E_FineGd(Xen%nb_FineGd,Xen%nk_FineGd,n_sp_pol),Xk%weights_FineGd(FG%N))
 else
   allocate(Xk%sstar_FineGd(FG%N))
 endif
 !
 call live_timing(zone//' Double Grid Reordering',FG%N)
 !
 i_fg=0
 do ik=1,N_coarse
   !
   do i_c=1,N_duplicate(ik)
     i_fg=i_fg+1
     if (zone=="IBZ") then
       FG%pt(i_fg,:)          =FineGd_k%pt(TMP_table_duplicate(ik,i_c),:)
       Xen%E_FineGd(:,i_fg,:) =FineGd_E%E(:,TMP_table_duplicate(ik,i_c),:)
       Xk%weights_FineGd(i_fg)=FineGd_k%weights(TMP_table_duplicate(ik,i_c))
       Reordering_map(TMP_table_duplicate(ik,i_c))=i_fg
     else
       FG%pt(i_fg,:)          =FineGd_k%ptbz(TMP_table_duplicate(ik,i_c),:)
       Xk%sstar_FineGd(i_fg) =Reordering_map(FineGd_k%sstar(TMP_table_duplicate(ik,i_c),1))
     endif
     call live_timing(steps=1)
   enddo
   FG%map(1,ik)=i_fg+1
   !
   do i_c=1,FG%N_mapped(ik)
     i_fg=i_fg+1
     if (zone=="IBZ") then
       FG%pt(i_fg,:)          =FineGd_k%pt(TMP_table_mapped(ik,i_c),:)
       Xen%E_FineGd(:,i_fg,:) =FineGd_E%E(:,TMP_table_mapped(ik,i_c),:)
       Xk%weights_FineGd(i_fg)=FineGd_k%weights(TMP_table_mapped(ik,i_c))
       Reordering_map(TMP_table_mapped(ik,i_c))=i_fg
     else
       FG%pt(i_fg,:)          =FineGd_k%ptbz(TMP_table_mapped(ik,i_c),:)
       Xk%sstar_FineGd(i_fg) =Reordering_map(FineGd_k%sstar(TMP_table_mapped(ik,i_c),1))
     endif
     call live_timing(steps=1)
   enddo
   FG%map(2,ik)=i_fg
   !
 enddo
 !
 call live_timing()
 !
end subroutine
