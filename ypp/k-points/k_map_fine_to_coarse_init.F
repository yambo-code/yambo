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
subroutine k_map_fine_to_coarse_init(Xk,Xen,FineGd_k,FineGd_is_expanded)
 !
 use pars,                ONLY:SP
 use units,               ONLY:HA2EV
 use stderr,              ONLY:intc
 use parser_m,            ONLY:parser
 use interfaces,          ONLY:io_DB1_selective_scan
 use zeros,               ONLY:k_iku_zero,k_rlu_zero,define_zeros
 use R_lattice,           ONLY:bz_samp,nkibz,bz_samp_reset,BZ_FineGd_mode,k_the_nearest
 use D_lattice,           ONLY:alat,nsym,input_GS_Tel,a,dl_sop,i_time_rev
 use com,                 ONLY:msg
 use YPPm,                ONLY:BZ_FineGd_nkpts_to_use,FineGd_DB1_paths,MAX_FineGd_DBs,&
&                              n_FineGd_DBs
 use electrons,           ONLY:levels,default_nel,n_bands,n_sp_pol,E_reset,FineGd_E_components_alloc
 use IO_m,                ONLY:REP
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)     :: Xk
 type(levels),  intent(inout)  :: Xen
 type(bz_samp), intent(out)    :: FineGd_k
 logical,       intent(out)    :: FineGd_is_expanded
 !
 ! Work Space
 !
 real(SP)     ::rand_k(BZ_FineGd_nkpts_to_use+1,3),a_save(3,3),alat_save(3),gap(2),tmp_zero(3)
 integer      ::i1,ik_ref,ib,ik,ik_near,i_fg
 logical      ::l_skip_check,l_skip_DbGd_border_points,l_I_am_border
 type(levels)          :: FineGd_E
 integer,  allocatable :: I_am_border(:)
 real(SP), allocatable :: rand_E(:,:,:)
 !
 ! BZ DB1 I/O
 !
 type(levels)         :: FineGd_io_E(MAX_FineGd_DBs)
 type(bz_samp)        :: FineGd_io_k(MAX_FineGd_DBs)
 integer              :: i_err(MAX_FineGd_DBs),nsym_save,i_time_rev_save,i_db
 real(SP)             :: dl_sop_save(3,3,48)
 !
 ! Functions
 !
 integer, external :: k_FineGd_grid_consistency_check
 logical, external :: k_map_fine_to_coarse_reduce
 !
 call parser('SkipBorderPts',l_skip_DbGd_border_points)
 !
 a_save=a
 alat_save=alat
 nsym_save=nsym
 dl_sop_save(:,:,:nsym)=dl_sop
 i_time_rev_save=i_time_rev
 FineGd_is_expanded=.FALSE.
 !
 ! DB1 reloading
 !
 i_err=0
 if (n_FineGd_DBs>0) then
   do i_db=1,n_FineGd_DBs
     i_err(i_db)=io_DB1_selective_scan("all",trim(FineGd_DB1_paths(i_db,1)),FineGd_io_E(i_db),FineGd_io_k(i_db),COM_mode=REP)
     if (i_err(i_db)/=0) call msg("s",'FineGd database NOT FOUND @'//trim(FineGd_DB1_paths(i_db,1)))
     if (i_err(i_db)==0) call msg("s",'FineGd database FOUND @'//trim(FineGd_DB1_paths(i_db,1)))
     !
     ! DB1 checks
     !
     if (i_err(i_db)==0.and.any(a/=a_save))  then
       call msg("s",':: FineGd DB1 DL structure does not fit')
       i_err(i_db)=-1
     endif
     if (i_err(i_db)==0.and.any(alat/=alat_save))  then
       call msg("s",':: FineGd DB1 alat does not fit.')
       i_err(i_db)=-1
     endif
     !
   enddo
   !
   if (any((/i_err(:n_FineGd_DBs)/=0/))) call error(" One or more FineGd databases have not been found")
   !
 endif
 !
 ! Now that I have the finer grid of Kpts I redefine the zeros for the kpts
 !
 do i_db=1,n_FineGd_DBs
   if(.not. trim(FineGd_DB1_paths(i_db,1))=="." ) then
     call define_zeros(vector_=FineGd_io_k(i_db)%pt, zero_=tmp_zero)
     k_iku_zero=min(k_iku_zero,tmp_zero)
     call define_zeros(vector_=FineGd_io_k(i_db)%pt, zero_=tmp_zero,RLU=.TRUE.)
     k_rlu_zero=min(k_rlu_zero,tmp_zero)
   endif
 enddo
 !
 call parser('SkipCheck',l_skip_check)
 !
 call msg('s',':: Electrons                 :',default_nel)
 call msg('s',':: Temperature               :',input_GS_Tel,"[eV]")
 call msg('s',':: Lattice factors           :',alat,"[a.u.]")
 do i_db=1,n_FineGd_DBs
   if (i_db==1) call msg('s',':: K points       [DB #'//trim(intc(i_db))//']    :',FineGd_io_E(i_db)%nk)
   if (i_db> 1) call msg('s','::                [DB #'//trim(intc(i_db))//']    :',FineGd_io_E(i_db)%nk)
 enddo
 !
 ! ...reset
 !
 call bz_samp_reset(FineGd_k)
 call E_reset(FineGd_E)
 !
 ! ...mark border points
 !
 if (l_skip_DbGd_border_points) then
   allocate(I_am_border( sum(FineGd_io_E(:n_FineGd_DBs)%nk) ))
   ik=0
   do i_db=1,n_FineGd_DBs
     do i1=1,FineGd_io_E(i_db)%nk
       ik_near=k_the_nearest(FineGd_io_k(i_db)%pt(i1,:),Xk%ptbz,Xk%nbz,border_point=l_I_am_border)
       ik=ik+1
       I_am_border(ik)=0
       if (l_I_am_border) I_am_border(ik)=1
     enddo
   enddo
   FineGd_E%nk=count(I_am_border==0)
   FineGd_k%nibz=count(I_am_border==0)
 else
   FineGd_E%nk=sum(FineGd_io_E(:n_FineGd_DBs)%nk)
   FineGd_k%nibz=sum(FineGd_io_E(:n_FineGd_DBs)%nk)
 endif
 !
 ! ...merge of ALL FineGd_io_E(:) and FineGd_io_k(:) into FineGd_E and FineGd_k
 !
 FineGd_E%nb=FineGd_io_E(1)%nb
 allocate(FineGd_E%E(FineGd_E%nb,FineGd_E%nk,n_sp_pol))
 allocate(FineGd_k%pt(FineGd_E%nk,3))
 ik=0
 i_fg=0
 do i_db=1,n_FineGd_DBs
   do i1=1,FineGd_io_E(i_db)%nk
     i_fg=i_fg+1
     if (l_skip_DbGd_border_points) then
       if (I_am_border(i_fg)==1) cycle
     endif
     ik=ik+1
     FineGd_E%E(:,ik,:)= FineGd_io_E(i_db)%E(:,i1,:)
     FineGd_k%pt(ik,:)= FineGd_io_k(i_db)%pt(i1,:)
   enddo
 enddo
 !
 ! ...clean after the i_db loop due [AM Jul 2020].
 do i_db=1,n_FineGd_DBs
   call E_reset(FineGd_io_E(i_db))
   call bz_samp_reset(FineGd_io_k(i_db))
 enddo
 !
 ! Use of less (only BZ_FineGd_nkpts, read from input) k-points
 !
 if (BZ_FineGd_nkpts_to_use<FineGd_E%nk.and.BZ_FineGd_nkpts_to_use>0) then
   nkibz   =BZ_FineGd_nkpts_to_use
   FineGd_E%nk=nkibz
   FineGd_k%nibz=nkibz
   allocate(rand_E(FineGd_E%nb,nkibz,n_sp_pol))
   rand_E(:,:,:)=FineGd_E%E(:,:nkibz,:)
   rand_k(:nkibz,:)=FineGd_k%pt(:nkibz,:)
   deallocate(FineGd_E%E,FineGd_k%pt)
   allocate(FineGd_E%E(FineGd_E%nb,nkibz,n_sp_pol),FineGd_k%pt(nkibz,3))
   FineGd_E%E(:,:,:)=rand_E(:,:,:)
   FineGd_k%pt(:,:)=rand_k(:nkibz,:)
   deallocate(rand_E)
 endif
 !
 gap(1)=minval(FineGd_E%E(Xen%nbf+1,:,1))-maxval(FineGd_E%E(Xen%nbf,:,1)) 
 gap(2)=10000._SP
 do i1=1,FineGd_E%nk
   if (FineGd_E%E(Xen%nbf+1,i1,1)-FineGd_E%E(Xen%nbf,i1,1)<gap(2)) then
     gap(2)=FineGd_E%E(Xen%nbf+1,i1,1)-FineGd_E%E(Xen%nbf,i1,1)
   endif
 enddo
 !
 if (BZ_FineGd_nkpts_to_use>0) call msg('s','::               [imposed]   :',BZ_FineGd_nkpts_to_use)
 call msg('s',':: Bands                     :',n_bands)
 call msg('s',':: Symmetries                :',nsym)
 if (i_time_rev==1) call msg('s',':: Time reversal included ')
 if (i_time_rev==0) call msg('s',':: Time reversal not included')
 if (Xen%nbf==Xen%nbm) then
   call msg('s',':: Gap indirect              :',gap(1)*HA2EV,"[eV]")
   call msg('s','::     direct                :',gap(2)*HA2EV,"[eV]")
 endif
 !
 ! See if there is correspondance between the random and the built-in
 ! k-points
 !
 call msg('s','Internal consistency check ...')
 ik_ref=k_FineGd_grid_consistency_check(Xk,Xen,FineGd_k,FineGd_E)
 !
 if (ik_ref>0) then
   call msg('l','passed @ Double Grid k-point #'//trim(intc(ik_ref))) 
 else 
   call msg('l','failed') 
   if (     l_skip_check) call warning(" A consistent X grid k-point was not found among the Double Grid list")
   if (.not.l_skip_check) call   error(" A consistent X grid k-point was not found among the Double Grid list")
 endif
 !
 if (nsym/=nsym_save) then
   call msg('s','Coarse grid and fine grid have different symmetries: '&
&               //trim(intc(nsym))//' [FineGd] / '//trim(intc(nsym_save))//' [CoarseGd]')
   call warning(" Grid expansion with FineGd symmetries imposed")
   !
   ! k_DbGd (IBZ->BZ)
   !
   call section('=','Double Grid K-points expansion ...')
   call k_ibz2bz(FineGd_k,'i',.true.)
   FineGd_is_expanded=k_map_fine_to_coarse_reduce(FineGd_k)
   if (FineGd_is_expanded) then
     call msg('lr','found '//trim(intc(FineGd_k%nbz))//' ('//trim(intc(FineGd_k%nibz))//') points in the BZ (IBZ)')
     YAMBO_FREE(FineGd_k%pt)
     YAMBO_ALLOC(FineGd_k%pt,(FineGd_k%nibz,3))
     FineGd_k%nibz=FineGd_k%nbz
     FineGd_k%pt=FineGd_k%ptbz
     FineGd_E%nk=FineGd_k%nibz
     BZ_FineGd_mode="expanded"
   else
     call msg('lr','found duplicates. Expansion ignored. Switching to unexpanded mode')
     BZ_FineGd_mode="unexpanded"
   endif
   !
 endif
 !
 ! Transfer to the Energy levels
 !
 Xen%FG%nb=FineGd_E%nb
 Xen%FG%nk=FineGd_E%nk
 call FineGd_E_components_alloc(Xen)
 do ib=1,FineGd_E%nb
   do ik=1,FineGd_E%nk
     if (     FineGd_is_expanded) Xen%FG%E(ib,ik,:) =FineGd_E%E(ib,FineGd_k%sstar(ik,1),:)
     if (.not.FineGd_is_expanded) Xen%FG%E(ib,ik,:) =FineGd_E%E(ib,ik,:)
   enddo
 enddo
 !
 call E_reset(FineGd_E)
 if (l_skip_DbGd_border_points) then
   call msg('sr','Removed '//trim(intc(count(I_am_border==1)))//' border points')
   deallocate(I_am_border)
 endif
 !
 ! Symmetry restoring
 !
 if (allocated(dl_sop)) then
   deallocate(dl_sop)
   nsym          =nsym_save
   i_time_rev    =i_time_rev_save
   allocate(dl_sop(3,3,nsym))
   dl_sop=dl_sop_SAVE(:,:,:nsym)
 endif
 !
end subroutine
