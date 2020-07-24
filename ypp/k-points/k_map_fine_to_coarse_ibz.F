!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine k_map_fine_to_coarse_ibz(Xk,Xen,FineGd_k,FineGd_E,Reordering_map)
 !
 use pars,                ONLY:SP
 use stderr,              ONLY:intc
 use com,                 ONLY:msg
 use vec_operate,         ONLY:k2bz
 ! DEBUG <
 !use vec_operate,         ONLY:c2a
 ! DEBUG >
 use electrons,           ONLY:levels,n_sp_pol
 use R_lattice,           ONLY:bz_samp,k_the_nearest,bz_samp_reset,rl_sop
 use parallel_m,          ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,          ONLY:PARALLEL_index
 use LIVE_t,              ONLY:live_timing
 !
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(inout) :: Xen
 type(bz_samp), intent(inout) :: FineGd_k
 type(levels),  intent(in)    :: FineGd_E
 integer,       intent(out)   :: Reordering_map(FineGd_k%nibz)
 !
 ! Work Space
 !
 integer              :: i1,ik_near,is_near,ikibz,ikibz_FineGd,i_err
 integer, allocatable :: TMP_map(:),TMP_table_mapped(:,:),TMP_table_duplicate(:,:),N_ibz_duplicate(:)
 real(SP)             :: kpt_tmp(3),weight_fac
 type(PP_indexes)     :: px
 !
 ! Initial mapping
 !=================
 Xk%nibz_FineGd    =FineGd_k%nibz
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/Xk%nibz_FineGd/))
 call PP_redux_wait
 !
 call live_timing('IBZ Double Grid Map',px%n_of_elements(myid+1))
 !
 allocate(TMP_map(Xk%nibz_FineGd))
 TMP_map=0
 !
 do i1=1,Xk%nibz_FineGd
   !
   call k2bz(FineGd_k%pt(i1,:))
   !
   if (.not.px%element_1D(i1)) then
     FineGd_k%pt(i1,:)=0._SP
     cycle
   endif
   !
   ik_near = k_the_nearest(FineGd_k%pt(i1,:),Xk%pt(:,:),Xk%nibz,ROTATE_k_ref=.TRUE.,k_symm=is_near,i_err=i_err)
   if(i_err==0) TMP_map(i1)=-ik_near
   if(i_err/=0) TMP_map(i1)= ik_near
   !
   ! Rotate the points in the IBZ of DbGd_k
   !
   kpt_tmp=FineGd_k%pt(i1,:)
   FineGd_k%pt(i1,:)=matmul(rl_sop(:,:,is_near),kpt_tmp)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call PP_redux_wait(TMP_map)
 call PP_redux_wait(FineGd_k%pt)
 !
 ! Number of points in each zone in the IBZ
 !=========================================
 allocate(Xk%nibz_mapped(Xk%nibz),N_ibz_duplicate(Xk%nibz))
 Xk%nibz_mapped=0
 N_ibz_duplicate=0
 do i1=1,Xk%nibz_FineGd
   ik_near=abs(TMP_map(i1))
   if(TMP_map(i1)>0) Xk%nibz_mapped(ik_near) =Xk%nibz_mapped(ik_near)+1
   if(TMP_map(i1)<0) N_ibz_duplicate(ik_near)=N_ibz_duplicate(ik_near)+1
 enddo
 !
 Xk%nibz_duplicate=sum(N_ibz_duplicate)
 !
 call msg('s',':: IBZ Blocks duplicate      :',Xk%nibz_duplicate)
 call msg('s',':: IBZ Blocks filling range  :',(/minval(Xk%nibz_mapped)+1,maxval(Xk%nibz_mapped)+1/))
 !
 ! IBZ table
 !===========
 allocate(TMP_table_mapped(Xk%nibz,maxval(Xk%nibz_mapped)))
 TMP_table_mapped=0
 if(Xk%nibz_duplicate>0) then
   allocate(TMP_table_duplicate(Xk%nibz,maxval(N_ibz_duplicate)))
   TMP_table_duplicate=0
 endif
 !
 Xk%nibz_mapped=0
 N_ibz_duplicate=0
 do i1=1,Xk%nibz_FineGd
   ik_near=abs(TMP_map(i1))
   if(TMP_map(i1)>0) then
     Xk%nibz_mapped(ik_near)=Xk%nibz_mapped(ik_near)+1
     TMP_table_mapped(ik_near,Xk%nibz_mapped(ik_near))=i1
   else if(TMP_map(i1)<0) then
     N_ibz_duplicate(ik_near)=N_ibz_duplicate(ik_near)+1
     TMP_table_duplicate(ik_near,N_ibz_duplicate(ik_near))=i1
   endif
 enddo
 !
 deallocate(TMP_map)
 !
 ! Reordering and transfer Xen%E_FineGd and Xk%k_FineGd
 !======================================================
 Xen%nb_FineGd=FineGd_E%nb
 Xen%nk_FineGd=FineGd_E%nk
 allocate(Xen%E_FineGd(Xen%nb_FineGd,Xen%nk_FineGd,n_sp_pol))
 !
 ! Skipe these for the moment
 !  real(SP),   pointer :: W_FineGd(:,:,:)        => null()
 !  real(SP),   pointer :: f_FineGd(:,:,:)        => null()
 !  real(SP),   pointer :: df_FineGd(:,:,:)       => null()
 !
 allocate(Xk%pt_FineGd(Xk%nibz_FineGd,3))
 allocate(Xk%weights_FineGd(Xk%nibz_FineGd))
 !
 call k_expand(FineGd_k)
 !
 allocate(Xk%weights_DbGd(maxval(Xk%nibz_mapped)+1,Xk%nibz))
 allocate(Xk%ibz_DbGd_map(2,Xk%nibz))
 Xk%weights_DbGd=-1
 !
 call live_timing('IBZ Double Grid Reordering',Xk%nibz_FineGd)
 !
 ikibz_FineGd=0
 do ikibz=1,Xk%nibz
   !
   do i1=1,N_ibz_duplicate(ikibz)
     ikibz_FineGd=ikibz_FineGd+1
     Xen%E_FineGd(:,ikibz_FineGd,:) =FineGd_E%E(:,TMP_table_duplicate(ikibz,i1),:)
     Xk%pt_FineGd(ikibz_FineGd,:)   =FineGd_k%pt(TMP_table_duplicate(ikibz,i1),:)
     Xk%weights_FineGd(ikibz_FineGd)=FineGd_k%weights(TMP_table_duplicate(ikibz,i1))
     Reordering_map(TMP_table_duplicate(ikibz,i1))=ikibz_FineGd
     call live_timing(steps=1)
   enddo
   !
   weight_fac=1._SP/real(Xk%nibz_mapped(ikibz)+1,SP)
   Xk%weights_DbGd(1,ikibz)=1._SP*weight_fac
   !
   Xk%ibz_DbGd_map(1,ikibz)=ikibz_FineGd+1
   do i1=1,Xk%nibz_mapped(ikibz)
     ikibz_FineGd=ikibz_FineGd+1
     Xen%E_FineGd(:,ikibz_FineGd,:) =FineGd_E%E(:,TMP_table_mapped(ikibz,i1),:)
     Xk%pt_FineGd(ikibz_FineGd,:)   =FineGd_k%pt(TMP_table_mapped(ikibz,i1),:)
     Xk%weights_FineGd(ikibz_FineGd)=FineGd_k%weights(TMP_table_mapped(ikibz,i1))
     Reordering_map(TMP_table_mapped(ikibz,i1))=ikibz_FineGd
     !
     Xk%weights_DbGd(i1+1,ikibz)=real(FineGd_k%nstar(TMP_table_mapped(ikibz,i1)),SP)/real(Xk%nstar(ikibz),SP)*weight_fac
     !
     call live_timing(steps=1)
   enddo
   Xk%ibz_DbGd_map(2,ikibz)=ikibz_FineGd
   !
 enddo
 !
 call live_timing()
 !
 deallocate(TMP_table_mapped,N_ibz_duplicate)
 if(allocated(TMP_table_duplicate)) deallocate(TMP_table_duplicate)
 !
 ! DEBUG <
 !do ikibz=1,Xk%nibz
 !  call c2a(v_in=Xk%pt(ikibz,:),v_out=kpt_tmp,mode='ki2c')
 !  write(99+ikibz,*) kpt_tmp
 !  do ikibz_FineGd=Xk%ibz_DbGd_map(1,ikibz),Xk%ibz_DbGd_map(2,ikibz)
 !    call c2a(v_in=Xk%pt_FineGd(ikibz_FineGd,:),v_out=kpt_tmp,mode='ki2c')
 !    write(1099+ikibz,*) kpt_tmp
 !  enddo
 !enddo
 ! DEBUG >
 !
end subroutine
