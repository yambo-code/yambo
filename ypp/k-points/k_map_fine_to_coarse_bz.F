!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine k_map_fine_to_coarse_bz(Xk,Xen,FineGd_k,FineGd_E,Reordering_map)
 !
 use pars,                ONLY:SP
 use stderr,              ONLY:intc
 use com,                 ONLY:msg
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,k_the_nearest,bz_samp_reset
 use parallel_m,          ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,          ONLY:PARALLEL_index
 use LIVE_t,              ONLY:live_timing
 !
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(in)    :: Xen
 type(bz_samp), intent(inout) :: FineGd_k
 type(levels),  intent(in)    :: FineGd_E
 integer,       intent(in)    :: Reordering_map(FineGd_k%nibz)
 !
 ! Work Space
 !
 integer              :: i1,ik_near,ikbz,ikbz_FineGd,i_err
 integer, allocatable :: TMP_map(:),TMP_table_mapped(:,:),TMP_table_duplicate(:,:),N_bz_duplicate(:)
 type(PP_indexes)     :: px
 !
 ! Initial mapping
 !=================
 Xk%nbz_FineGd = FineGd_k%nbz
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/Xk%nbz_FineGd/))
 call PP_redux_wait
 !
 call live_timing('BZ Double Grid Map',px%n_of_elements(myid+1))
 !
 allocate(TMP_map(Xk%nbz_FineGd))
 TMP_map=0
 !
 do i1=1,Xk%nbz_FineGd
   if (.not.px%element_1D(i1)) cycle
   ik_near=k_the_nearest(FineGd_k%ptbz(i1,:),Xk%ptbz(:,:),Xk%nbz,i_err=i_err)
   if(i_err==0) TMP_map(i1)=-ik_near
   if(i_err/=0) TMP_map(i1)= ik_near
   call live_timing(steps=1)
 enddo
 !
 call PP_redux_wait(TMP_map)
 !
 ! Number of points in each sphere in the BZ
 !===========================================
 allocate(Xk%nbz_mapped(Xk%nbz),N_bz_duplicate(Xk%nbz))
 Xk%nbz_mapped=0
 N_bz_duplicate=0
 do i1=1,Xk%nbz_FineGd
   ik_near=abs(TMP_map(i1))
   if(TMP_map(i1)>0) Xk%nbz_mapped(ik_near) =Xk%nbz_mapped(ik_near)+1
   if(TMP_map(i1)<0) N_bz_duplicate(ik_near)=N_bz_duplicate(ik_near)+1
 enddo
 !
 Xk%nbz_duplicate=sum(N_bz_duplicate)
 !
 call msg('s','::  BZ Blocks dupicate       :',Xk%nbz_duplicate)
 call msg('s','::  BZ Blocks filling range  :',(/minval(Xk%nbz_mapped)+1,maxval(Xk%nbz_mapped)+1/))
 !
 ! BZ table
 !==========
 allocate(TMP_table_mapped(Xk%nbz,maxval(Xk%nbz_mapped)))
 TMP_table_mapped=0
 if(Xk%nbz_duplicate>0) then
   allocate(TMP_table_duplicate(Xk%nbz,maxval(N_bz_duplicate)))
   TMP_table_duplicate=0
 endif
 !
 Xk%nbz_mapped=0
 N_bz_duplicate=0
 do i1=1,Xk%nbz_FineGd
   ik_near=abs(TMP_map(i1))
   if(TMP_map(i1)>0) then
     Xk%nbz_mapped(ik_near)=Xk%nbz_mapped(ik_near)+1
     TMP_table_mapped(ik_near,Xk%nbz_mapped(ik_near))=i1
   else if(TMP_map(i1)<0) then
     N_bz_duplicate(ik_near)=N_bz_duplicate(ik_near)+1
     TMP_table_duplicate(ik_near,N_bz_duplicate(ik_near))=i1
   endif
 enddo
 !
 deallocate(TMP_map)
 !
 ! Reordering and transfer Xk%ptbz_FineGd and Xk%sstar_FineGd
 !=============================================================
 allocate(Xk%ptbz_FineGd(Xk%nbz_FineGd,3))
 allocate(Xk%sstar_FineGd(FineGd_k%nbz))
 Xk%sstar_FineGd=FineGd_k%sstar(:,1)
 !
 allocate(Xk%bz_DbGd_map(2,Xk%nbz))
 !
 call live_timing('BZ Double Grid Reordering',Xk%nbz_FineGd)
 !
 ikbz_FineGd=0
 do ikbz=1,Xk%nbz
   !
   do i1=1,N_bz_duplicate(ikbz)
     ikbz_FineGd=ikbz_FineGd+1
     Xk%ptbz_FineGd(ikbz_FineGd,:)=FineGd_k%ptbz(TMP_table_duplicate(ikbz,i1),:)
     Xk%sstar_FineGd(ikbz_FineGd) =Reordering_map(FineGd_k%sstar(TMP_table_duplicate(ikbz,i1),1))
     call live_timing(steps=1)
   enddo
   !
   Xk%bz_DbGd_map(1,ikbz)=ikbz_FineGd+1
   do i1=1,Xk%nbz_mapped(ikbz)
     ikbz_FineGd=ikbz_FineGd+1
     Xk%ptbz_FineGd(ikbz_FineGd,:)=FineGd_k%ptbz(TMP_table_mapped(ikbz,i1),:)
     Xk%sstar_FineGd(ikbz_FineGd) =Reordering_map(FineGd_k%sstar(TMP_table_mapped(ikbz,i1),1))
     call live_timing(steps=1)
   enddo
   Xk%bz_DbGd_map(2,ikbz)=ikbz_FineGd
   !
 enddo
 !
 call live_timing()
 !
 deallocate(TMP_table_mapped,N_bz_duplicate)
 if(allocated(TMP_table_duplicate)) deallocate(TMP_table_duplicate)
 !
 ! DEBUG <
 !do ikbz=1,Xk%nbz
 !  write(1100,*) Xk%ptbz(ikbz,:)
 !  do ikbz_FineGd=Xk%bz_DbGd_map(1,ikbz),Xk%bz_DbGd_map(2,ikbz)
 !    write(1100+ikbz,*) Xk%ptbz_fineGd(ikbz_FineGd,:)
 !  enddo
 !enddo
 ! DEBUG >
 !
end subroutine
