!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine k_map_fine_to_coarse_bz(Xk,Xen,FineGd_k,FineGd_E)
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
 !
 ! Work Space
 !
 integer              :: i1,ik_near,ikbz,ikbz_FineGd
 integer, allocatable :: TMP_map_bz(:)
 type(PP_indexes)     :: px
 !
 ! Initial mapping
 !=================
 !
 Xk%nbz_FineGd = FineGd_k%nbz
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/FineGd_k%nbz/))
 call PP_redux_wait
 !
 call live_timing('BZ Double Grid Map',px%n_of_elements(myid+1))
 !
 allocate(TMP_map_bz(FineGd_k%nbz))
 TMP_map_bz=0
 !
 do i1=1,FineGd_k%nbz
   if (.not.px%element_1D(i1)) cycle
   ik_near=k_the_nearest(FineGd_k%ptbz(i1,:),Xk%ptbz(:,:),Xk%nbz)
   TMP_map_bz(i1)=ik_near
   call live_timing(steps=1)
 enddo
 !
 call PP_redux_wait(TMP_map_bz)
 !
 ! Number of points in each sphere in the BZ
 !===========================================
 allocate(Xk%nbz_mapped(Xk%nbz))
 Xk%nbz_mapped=0
 do i1=1,Xk%nbz_FineGd
   ik_near=TMP_map_bz(i1)
   Xk%nbz_mapped(ik_near)=Xk%nbz_mapped(ik_near)+1
 enddo
 !
 ! BZ table
 !==========
 allocate(Xk%bz_DbGd_map(Xk%nbz,maxval(Xk%nbz_mapped)))
 Xk%bz_DbGd_map=0
 !
 Xk%nbz_mapped=0
 do i1=1,Xk%nbz_FineGd
   ik_near=TMP_map_bz(i1)
   Xk%nbz_mapped(ik_near)=Xk%nbz_mapped(ik_near)+1
   Xk%bz_DbGd_map(ik_near,Xk%nbz_mapped(ik_near))=i1
 enddo
 !
 ! Reordering and transfer Xk%ptbz_FineGd and Xk%sstar_FineGd
 !=============================================================
 allocate(Xk%ptbz_FineGd(Xk%nbz_FineGd,3))
 allocate(Xk%sstar_FineGd(FineGd_k%nbz))
 Xk%sstar_FineGd=FineGd_k%sstar(:,1)
 !
 ikbz_FineGd=0
 do ikbz=1,Xk%nbz
   do i1=1,Xk%nbz_mapped(ikbz)
     ikbz_FineGd=ikbz_FineGd+1
     Xk%ptbz_FineGd(ikbz_FineGd,:)=FineGd_k%ptbz(Xk%bz_DbGd_map(ikbz,i1),:)
     Xk%sstar_FineGd(ikbz_FineGd) =FineGd_k%sstar(Xk%bz_DbGd_map(ikbz,i1),1)
     !
     Xk%bz_DbGd_map(ikbz,i1)=i1+sum(Xk%nbz_mapped(:ikbz-1))
   enddo
 enddo
 !
 call msg('s','::  BZ Blocks filling range  :',(/minval( Xk%nbz_mapped),maxval( Xk%nbz_mapped)/))
 !
 ! DEBUG <
 do ikbz=1,Xk%nbz
   write(1100,*) Xk%ptbz(ikbz,:)
   do i1=1,Xk%nbz_mapped(ikbz)
     write(1100+ikbz,*) Xk%ptbz_fineGd(Xk%bz_DbGd_map(ikbz,i1),:)
   enddo
 enddo
 ! DEBUG >
 !
end subroutine
