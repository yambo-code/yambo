!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine k_map_fine_to_coarse_init(Xk,Xen,FineGd_k,FineGd_is_expanded,PH_grid)
 !
 use pars,                ONLY:SP,pi
 use units,               ONLY:HA2EV
 use stderr,              ONLY:intc
 use parser_m,            ONLY:parser
 use interfaces,          ONLY:io_DB1_selective_scan
 use zeros,               ONLY:k_iku_zero,k_rlu_zero,define_zeros
 use R_lattice,           ONLY:bz_samp,nkibz,bz_samp_reset,BZ_FineGd_mode
 use D_lattice,           ONLY:alat,nsym,input_GS_Tel,a,dl_sop,i_time_rev,n_atoms
 use com,                 ONLY:msg
 use YPPm,                ONLY:BZ_FineGd_nkpts_to_use,FineGd_DB1_paths,MAX_FineGd_DBs,&
&                              n_FineGd_DBs
 use electrons,           ONLY:levels,default_nel,n_bands,n_sp_pol,E_reset,FineGd_E_components_alloc
 use IO_m,                ONLY:REP
#if defined _YPP_ELPH
 use YPP_ELPH,            ONLY:ph_modes_file,ph_freqs_file,ELPH_databases_IO_freqs,ELPH_databases_IO_modes
 use stderr,              ONLY:STRING_same
 use vec_operate,         ONLY:c2a
#endif
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)     :: Xk
 type(levels),  intent(inout)  :: Xen
 type(bz_samp), intent(out)    :: FineGd_k
 logical,       intent(out)    :: FineGd_is_expanded
 logical,       intent(in)     :: PH_grid
 !
 ! Work Space
 !
 real(SP)     ::rand_k(BZ_FineGd_nkpts_to_use+1,3),a_save(3,3),alat_save(3),gap(2),tmp_zero(3)
 integer      ::i1,ik_ref,ib,ik
 logical      ::l_skip_check,l_skip_DbGd_border_points
 type(levels)          :: FineGd_E
 real(SP), allocatable :: rand_E(:,:,:)
 character(1)          :: K_or_Q
 !
#if defined _YPP_ELPH
 integer            :: i_q,ph_modes
#endif
 !
 ! BZ DB1 I/O
 !
 type(levels)              :: FineGd_io_E(MAX_FineGd_DBs)
 type(bz_samp)             :: FineGd_io_k(MAX_FineGd_DBs)
 integer                   :: i_err(MAX_FineGd_DBs),nsym_save,i_time_rev_save,i_db
 real(SP)                  :: dl_sop_save(3,3,48)
 complex(SP), allocatable  :: ph_pol(:,:,:,:)
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
 if(.not.PH_grid) then
   !
   ! DB1 reloading
   !
   K_or_Q='K'
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
   call parser('SkipCheck',l_skip_check)
   !
 else ! phonon case
   !
#if defined _YPP_ELPH
   !
   K_or_Q='Q'
   !
   ph_modes=3*n_atoms
   n_FineGd_DBs=1
   !
   if(.not.STRING_same(ph_freqs_file,'none')) then
     !
     call msg('s',":: Phonon frequencies read from: "//trim(ph_freqs_file))
     call ELPH_databases_IO_freqs(ph_file=ph_freqs_file,n_q=FineGd_io_k(1)%nibz)
     !
   elseif(.not.STRING_same(ph_modes_file,'none')) then
     !      
     call msg('s',":: Phonon frequencies read from: "//trim(ph_modes_file))
     call ELPH_databases_IO_modes(ph_file=ph_modes_file,n_q=FineGd_io_k(1)%nibz)
     !
   endif
   !
   if(FineGd_io_k(1)%nibz==0) call error(' Error reading phonon frequencies')
   !   
   call msg('s',":: Number of phonon modes: ",ph_modes)
   call msg('s',":: Number of phonon q-points in the matdyn file: ",FineGd_io_k(1)%nibz)
   YAMBO_ALLOC(FineGd_io_k(1)%pt,(FineGd_io_k(1)%nibz,3))
   !
   FineGd_io_E(1)%nb=ph_modes
   FineGd_io_E(1)%nk=FineGd_io_k(1)%nibz
   !
   YAMBO_ALLOC(FineGd_io_E(1)%E,(FineGd_io_E(1)%nb,FineGd_io_E(1)%nk,1))
   !
   if(.not.STRING_same(ph_freqs_file,'none')) then
     !      
     call ELPH_databases_IO_freqs(ph_file=ph_freqs_file,n_q=FineGd_io_E(1)%nk, &
&           q_pt=FineGd_io_k(1)%pt,ph_freqs=FineGd_io_E(1)%E(:,:,1))
     !
   elseif(.not.STRING_same(ph_modes_file,'none')) then
     !       
     YAMBO_ALLOC(ph_pol,(FineGd_io_E(1)%nk,FineGd_io_E(1)%nb,n_atoms,3))
     !
     call ELPH_databases_IO_modes(ph_file=ph_modes_file,n_q=FineGd_io_E(1)%nk, &
&           q_pt=FineGd_io_k(1)%pt,ph_freqs=FineGd_io_E(1)%E(:,:,1),ph_pol=ph_pol)
     !
     YAMBO_FREE(ph_pol) ! Polarization vectors not used in the Double-grid
     !
   endif
   !
   ! q-points in matdyn output are alat units, convert to internal units
   ! ==================================
   FineGd_io_k(1)%pt=FineGd_io_k(1)%pt*(2.*pi/alat(1))    ! From QuantumEspresso Alat to CC
   !
   ! Converto to iku units
   !
   do i_q=1,FineGd_io_k(1)%nibz
     call c2a(v_in=FineGd_io_k(1)%pt(i_q,:),mode="kc2i")
   enddo
   !
#endif
   !
   l_skip_check=.TRUE.
   !
 endif
 !
 ! Now that I have the finer grid of Kpts I redefine the zeros for the kpts
 !
 do i_db=1,n_FineGd_DBs
   if(.not. trim(FineGd_DB1_paths(i_db,1))=="." .or.PH_grid) then
     call define_zeros(vector_=FineGd_io_k(i_db)%pt, zero_=tmp_zero)
     k_iku_zero=min(k_iku_zero,tmp_zero)
     call define_zeros(vector_=FineGd_io_k(i_db)%pt, zero_=tmp_zero,RLU=.TRUE.)
     k_rlu_zero=min(k_rlu_zero,tmp_zero)
   endif
 enddo
 !
 call msg('s',':: Electrons                 :',default_nel)
 call msg('s',':: Temperature               :',input_GS_Tel,"[eV]")
 call msg('s',':: Lattice factors           :',alat,"[a.u.]")
 do i_db=1,n_FineGd_DBs
   if (i_db==1) call msg('s',':: '//K_or_Q//' points       [DB #'//trim(intc(i_db))//']    :',FineGd_io_E(i_db)%nk)
   if (i_db> 1) call msg('s','::                [DB #'//trim(intc(i_db))//']    :',FineGd_io_E(i_db)%nk)
 enddo
 !
 ! ...reset
 !
 call bz_samp_reset(FineGd_k)
 call E_reset(FineGd_E)
 !
 ! ...merge databases and remove border points if required
 !
 call k_find_border_and_merge(FineGd_E,FineGd_k,n_FineGd_DBs,FineGd_io_E,FineGd_io_k,Xk,l_skip_DbGd_border_points)
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
 if(.not.PH_grid.and.Xen%nb>1) then
   gap(1)=minval(FineGd_E%E(Xen%nbf(1)+1,:,1))-maxval(FineGd_E%E(Xen%nbf(1),:,1)) 
   gap(2)=10000._SP
   do i1=1,FineGd_E%nk
     if (FineGd_E%E(Xen%nbf(1)+1,i1,1)-FineGd_E%E(Xen%nbf(1),i1,1)<gap(2)) then
       gap(2)=FineGd_E%E(Xen%nbf(1)+1,i1,1)-FineGd_E%E(Xen%nbf(1),i1,1)
     endif
   enddo
 endif
 !
 if (BZ_FineGd_nkpts_to_use>0) call msg('s','::               [imposed]   :',BZ_FineGd_nkpts_to_use)
 call msg('s',':: Bands                     :',n_bands)
 call msg('s',':: Symmetries                :',nsym)
 if (i_time_rev==1) call msg('s',':: Time reversal included ')
 if (i_time_rev==0) call msg('s',':: Time reversal not included')
 !
 if(.not.PH_grid.and.Xen%nb>1) then
   if (all(Xen%nbf==Xen%nbm)) then
     call msg('s',':: Gap indirect              :',gap(1)*HA2EV,"[eV]")
     call msg('s','::     direct                :',gap(2)*HA2EV,"[eV]")
   endif
 endif
 !
 if(.not.PH_grid) then
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
&                 //trim(intc(nsym))//' [FineGd] / '//trim(intc(nsym_save))//' [CoarseGd]')
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
