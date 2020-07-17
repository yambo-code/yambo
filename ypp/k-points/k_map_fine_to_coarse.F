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
subroutine k_map_fine_to_coarse(zone,Xk,Xen,N_coarse,k_coarse,FG,FineGd_k,FineGd_E,Reordering_map)
 !
 use pars,                ONLY:SP
 use com,                 ONLY:msg
 use vec_operate,         ONLY:k2bz
 use electrons,           ONLY:levels,n_sp_pol
 use R_lattice,           ONLY:bz_samp,k_the_nearest,rl_sop,bz_fine_grid
 use parallel_m,          ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,          ONLY:PARALLEL_index
 use LIVE_t,              ONLY:live_timing
 !
#include<memory.h>
 !
 character(*)      , intent(in)    :: zone
 integer           , intent(in)    :: N_coarse
 real(SP)          , intent(in)    :: k_coarse(N_coarse,3)
 type(bz_fine_grid), intent(inout) :: FG
 type(bz_samp), intent(inout)      :: FineGd_k,Xk
 type(levels),  intent(in)         :: FineGd_E
 type(levels),  intent(inout)      :: Xen
 integer,       intent(inout)      :: Reordering_map(FineGd_k%nibz)
 !
 ! Work Space
 !
 integer              :: ik_near,is_near,ik,i_err,i_fg,i1
 integer, allocatable :: TMP_map(:),TMP_table_mapped(:,:),TMP_table_duplicate(:,:),N_duplicate(:)
 real(SP)             :: kpt_tmp(3)
 type(PP_indexes)     :: px
 !
 ! Initial mapping
 !=================
 if (zone=="IBZ") FG%N    =FineGd_k%nibz
 if (zone=="BZ" ) FG%N    =FineGd_k%nbz
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/FG%N/))
 call PP_redux_wait
 !
 call live_timing(zone//' Double Grid Map',px%n_of_elements(myid+1))
 !
 allocate(TMP_map(FG%N))
 TMP_map=0
 !
 do i_fg=1,FG%N
   !
   if (zone=="IBZ") then
     call k2bz(FineGd_k%pt(i_fg,:))
     if (.not.px%element_1D(i_fg)) then
       FineGd_k%pt(i_fg,:)=0._SP
       cycle
     endif
     ik_near = k_the_nearest(FineGd_k%pt(i_fg,:),k_coarse(:,:),N_coarse,ROTATE_k_ref=.TRUE.,k_symm=is_near,i_err=i_err)
     kpt_tmp=FineGd_k%pt(i_fg,:)
     FineGd_k%pt(i_fg,:)=matmul(rl_sop(:,:,is_near),kpt_tmp)
   else
     if (.not.px%element_1D(i_fg)) cycle
     ik_near=k_the_nearest(FineGd_k%ptbz(i_fg,:),k_coarse(:,:),N_coarse,i_err=i_err)
   endif
   !
   if(i_err==0) TMP_map(i_fg)=-ik_near
   if(i_err/=0) TMP_map(i_fg)= ik_near
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call PP_redux_wait(TMP_map)
 if (zone=="IBZ") call PP_redux_wait(FineGd_k%pt)
 !
 ! Number of points in each zone 
 !===============================
 allocate(FG%N_mapped(N_coarse),N_duplicate(N_coarse))
 FG%N_mapped=0
 N_duplicate=0
 do i_fg=1,FG%N
   ik_near=abs(TMP_map(i_fg))
   if(TMP_map(i_fg)>0) FG%N_mapped(ik_near)=FG%N_mapped(ik_near)+1
   if(TMP_map(i_fg)<0) N_duplicate(ik_near)=N_duplicate(ik_near)+1
 enddo
 !
 FG%N_duplicate=sum(N_duplicate)
 !
 call msg('s',':: '//zone//' Duplicated points     :',FG%N_duplicate)
 call msg('s',':: '//zone//' Blocks filling range  :',(/minval(FG%N_mapped)+1,maxval(FG%N_mapped)+1/))
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
   allocate(Xen%E_FineGd(Xen%nb_FineGd,Xen%nk_FineGd,n_sp_pol))
   allocate(Xk%weights_FineGd(FG%N),Xk%weights_DbGd(N_coarse))
 else
   allocate(Xk%sstar_FineGd(FG%N))
 endif
 !
 call live_timing(zone//' Double Grid Reordering',FG%N)
 !
 i_fg=0
 do ik=1,N_coarse
   !
   do i1=1,N_duplicate(ik)
     i_fg=i_fg+1
     FG%pt(i_fg,:)          =FineGd_k%pt(TMP_table_duplicate(ik,i1),:)
     if (zone=="IBZ") then
       Xen%E_FineGd(:,i_fg,:) =FineGd_E%E(:,TMP_table_duplicate(ik,i1),:)
       Xk%weights_FineGd(i_fg)=FineGd_k%weights(TMP_table_duplicate(ik,i1))
       Reordering_map(TMP_table_duplicate(ik,i1))=i_fg
     else
       Xk%sstar_FineGd(i_fg) =Reordering_map(FineGd_k%sstar(TMP_table_duplicate(ik,i_fg),1))
     endif
     call live_timing(steps=1)
   enddo
   FG%map(1,ik)=i_fg+1
   !
   do i1=1,FG%N_mapped(ik)
     i_fg=i_fg+1
     FG%pt(i_fg,:)          =FineGd_k%pt(TMP_table_mapped(ik,i1),:)
     if (zone=="IBZ") then
       Xen%E_FineGd(:,i_fg,:) =FineGd_E%E(:,TMP_table_mapped(ik,i1),:)
       Xk%weights_FineGd(i_fg)=FineGd_k%weights(TMP_table_mapped(ik,i1))
       Reordering_map(TMP_table_mapped(ik,i1))=i_fg
     else
       Xk%sstar_FineGd(i_fg) =Reordering_map(FineGd_k%sstar(TMP_table_mapped(ik,i_fg),1))
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
