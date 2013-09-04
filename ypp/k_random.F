!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine k_random(Xk,Xen)
 !
 use pars,                ONLY:SP,DP,lchlen,schlen
 use units,               ONLY:HA2EV
 use stderr,              ONLY:set_real_printed_length,intc
 use R_lattice,           ONLY:bz_samp,nkibz,k_the_nearest,bz_samp_reset
 use parser_m,            ONLY:parser
 use D_lattice,           ONLY:alat,nsym,input_GS_Tel,a,dl_sop
 use com,                 ONLY:msg,of_open_close,error,warning
 use vec_operate,         ONLY:rlu_v_norm,v_norm,c2a,k2bz,v_is_zero
 use YPP,                 ONLY:BZ_RIM_nkpts,RIM_DB1_paths,gamma_radius,K_transform,MAX_RIM_DBs,n_RIM_DBs
 use electrons,           ONLY:levels,default_nel,n_bands,&
&                              BZ_RIM_nbands,BZ_RIM_nkpt,BZ_RIM_table,&
&                              BZ_RIM_tot_nkpts,n_spin,BZ_RIM_max_filling,&
&                              BZ_RIM_ipol_weight,n_sp_pol,E_reset
 use LIVE_t,              ONLY:live_timing
 use IO_m,                ONLY:io_control,NONE,OP_WR_CL
 !
 implicit none
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 real(SP)     ::dummy,rand_k(BZ_RIM_nkpts+1,3),a_save(3,3),alat_save(3),&
&               gap(2)
 integer      ::i1,ikbz,i2
 character(lchlen) :: k_rand_fname
 character(schlen) :: dump
 logical           :: No_Weight,l_skip_check,l_add_X_grid
 real(SP), allocatable :: rand_E(:,:,:)
 !
 ! Random Generator
 !
 character(12)      :: ch(3)
 integer            :: iseed(8),k_counter
 logical            :: impose_gamma_radius
 !
 ! BZ DB1 I/O
 !
 type(levels)      :: RIM_E(MAX_RIM_DBs)
 type(bz_samp)     :: RIM_k(MAX_RIM_DBs)
 integer           :: ID,i_err(MAX_RIM_DBs),nsym_save,nsym_RIM_DB,i_db,i_db_ref,ik_ref(2)
 integer, external :: ioDB1_selective_scan,ioE_RIM
 real(SP)          :: dl_sop_save(3,3,48)
 !
 ! Saves 2 check
 !
 a_save=a
 alat_save=alat
 nsym_save=nsym
 dl_sop_save(:,:,:nsym)=dl_sop
 !
 ! DB1 reloading
 !
 i_err=0
 if (n_RIM_DBs>0) then
   do i_db=1,n_RIM_DBs
     i_err(i_db)=ioDB1_selective_scan(RIM_E(i_db),RIM_k(i_db),trim(RIM_DB1_paths(i_db,1)),'all')
     if (i_err(i_db)/=0) call msg("s",'RIM database in '//trim(RIM_DB1_paths(i_db,1))//' NOT FOUND')
     if (i_err(i_db)==0) call msg("s",'RIM database in'//trim(RIM_DB1_paths(i_db,1))//' FOUND')
     !
     ! DB1 checks
     !
     if (i_err(i_db)==0.and.any(a/=a_save))  then
       call msg("s",':: RIM DB1 DL structure does not fit')
       i_err(i_db)=-1
     endif
     if (i_err(i_db)==0.and.any(alat/=alat_save))  then
       call msg("s",':: RIM DB1 alat does not fit.')
       i_err(i_db)=-1
     endif
     !
   enddo
   !
   if (any((/i_err(:n_RIM_DBs)/=0/))) call error("One or more RIM databases have not been found")
   !
 endif
 !
 ! Symmetry restoring
 !
 if (allocated(dl_sop)) then
   deallocate(dl_sop)
   nsym_RIM_DB=nsym
   nsym       =nsym_save
   allocate(dl_sop(3,3,nsym))
   dl_sop=dl_sop_SAVE(:,:,:nsym)
 endif
 !
 call parser('NoWeights',No_Weight)
 call parser('SkipCheck',l_skip_check)
 !
 impose_gamma_radius=gamma_radius>0.
 !
 if (n_RIM_DBs==0) then
   !
   call date_and_time(ch(1),ch(2),ch(3),iseed)
   iseed=iabs(iseed)
   ! iseed(4) must be odd
   iseed(4)=2*(iseed(4)/2)+1
   !
   call section('*',"== Random k-points generator ==")
   !
   call live_timing('Random k-points',BZ_RIM_nkpts,SERIAL=.true.)
   !
   call set_real_printed_length(f_length=20,g_length=20)
   !
   k_rand_fname="random_k_pts"
   call of_open_close(k_rand_fname,'ot')
   if(No_Weight) then
     call msg('o random',"#",(/"c1","c2","c3"/),INDENT=0,USE_TABS=.true.)
   else 
     call msg('o random',"#",(/"c1","c2","c3","wt"/),INDENT=0,USE_TABS=.true.)
   endif
   !
   k_counter=0
   !
   do while ( k_counter < BZ_RIM_nkpts )
     !
     if (impose_gamma_radius) then
       !
       ! select only point with |k| < gamma_radius [a.u.]
       !
       call build_spherical_k()
       !
     else
       !
       call build_3D_k()
       !
     endif
     !
     call K_transform(rand_k(k_counter+1,:),'iku')
     !
     if(No_Weight) then
       write (dump,'(3f12.7)') rand_k(k_counter+1,:)
     else
       write (dump,'(4f12.7)') rand_k(k_counter+1,:),1._SP/real(BZ_RIM_nkpts)     
     endif
     call msg('o random',"",trim(dump),USE_TABS=.true.)
     !
     k_counter=k_counter+1
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call of_open_close(k_rand_fname)
   call live_timing()
   call set_real_printed_length()
   !
   return
 endif 
 !
 call section('*','BZ RIM DB(s)')
 !
 call msg('s',':: Electrons             :',default_nel)
 call msg('s',':: Temperature       [ev]:',input_GS_Tel)
 call msg('s',':: Lattice factors [a.u.]:',alat)
 do i_db=1,n_RIM_DBs
   if (i_db==1) call msg('s',':: K points        [DB #'//trim(intc(i_db))//']:',RIM_E(i_db)%nk)
   if (i_db> 1) call msg('s','::                 [DB #'//trim(intc(i_db))//']:',RIM_E(i_db)%nk)
 enddo
 !
 ! Mergering of ALL RIM_E(:) => RIM_E(n_RIM_DBs+1)
 !
 i_db_ref=1
 !
 if (n_RIM_DBs>1) then
   i_db_ref=n_RIM_DBs+1
   call bz_samp_reset(RIM_k(i_db_ref))
   call E_reset(RIM_E(i_db_ref))
   RIM_E(i_db_ref)%nk=sum(RIM_E(:n_RIM_DBs)%nk)
   RIM_E(i_db_ref)%nb=RIM_E(1)%nb
   RIM_k(i_db_ref)%nibz=sum(RIM_E(:n_RIM_DBs)%nk)
   allocate(RIM_E(i_db_ref)%E(RIM_E(i_db_ref)%nb,RIM_E(i_db_ref)%nk,n_sp_pol))
   allocate(RIM_k(i_db_ref)%pt(RIM_E(i_db_ref)%nk,3))
   do i_db=1,n_RIM_DBs
     if (i_db==1) ik_ref(1)=0
     if (i_db> 1) ik_ref(1)=sum(RIM_E(1:i_db-1)%nk)
     do i1=1,RIM_E(i_db)%nk
       RIM_E(i_db_ref)%E(:,i1+ik_ref(1),:)= RIM_E(i_db)%E(:,i1,:)
       RIM_k(i_db_ref)%pt(i1+ik_ref(1),:)= RIM_k(i_db)%pt(i1,:)
     enddo
   enddo
 endif
 !
 ! Use of less (only BZ_RIM_nkpts, read from input) k-points
 !
 if (BZ_RIM_nkpts<nkibz.and.BZ_RIM_nkpts>0) then
   nkibz   =BZ_RIM_nkpts
   RIM_E(i_db_ref)%nk=nkibz
   allocate(rand_E(RIM_E(i_db_ref)%nb,nkibz,n_sp_pol))
   rand_E(:,:,:)=RIM_E(i_db_ref)%E(:,:nkibz,:)
   rand_k(:nkibz,:)=RIM_k(i_db_ref)%pt(:nkibz,:)
   deallocate(RIM_E(i_db_ref)%E,RIM_k(i_db_ref)%pt)
   allocate(RIM_E(i_db_ref)%E(RIM_E(i_db_ref)%nb,nkibz,n_sp_pol),RIM_k(i_db_ref)%pt(nkibz,3))
   RIM_E(i_db_ref)%E(:,:,:)=rand_E(:,:,:)
   RIM_k(i_db_ref)%pt(:,:)=rand_k(:,:)
   deallocate(rand_E)
 endif
 !
 gap(1)=minval(RIM_E(i_db_ref)%E(Xen%nbf+1,:,1))-maxval(RIM_E(i_db_ref)%E(Xen%nbf,:,1)) 
 gap(2)=10000.
 do i1=1,nkibz
   if (RIM_E(i_db_ref)%E(Xen%nbf+1,i1,1)-RIM_E(i_db_ref)%E(Xen%nbf,i1,1)<gap(2)) then
     gap(2)=RIM_E(i_db_ref)%E(Xen%nbf+1,i1,1)-RIM_E(i_db_ref)%E(Xen%nbf,i1,1)
   endif
 enddo
 !
 call msg('s','::              [imposed]:',BZ_RIM_nkpts)
 call msg('s',':: Bands                 :',n_bands)
 call msg('s',':: Symmetries            :',nsym_RIM_DB)
 if (Xen%nbf==Xen%nbm) then
   call msg('s',':: Gap indirect      [ev]:',gap(1)*HA2EV)
   call msg('s','::     direct        [ev]:',gap(2)*HA2EV)
 endif
 !
 ! See if there is correspondance between the random and the built-in
 ! k-points
 !
 call section('+','Internal consistency check ...')
 ik_ref=0
 i1_loop: do i1=1,Xen%nk
   do i2=1,RIM_E(i_db_ref)%nk
     if (v_is_zero(RIM_k(i_db_ref)%pt(i2,:)-Xk%pt(i1,:))) then
       ik_ref=(/i1,i2/)
       exit  i1_loop
     endif
   enddo
 enddo i1_loop
 !
 if (ik_ref(1)> 0) then
   call msg('l','passed') 
   RIM_E(i_db_ref)%E(:,:,:)=RIM_E(i_db_ref)%E(:,:,:)+Xen%E(Xen%nbm,ik_ref(1),1)-RIM_E(i_db_ref)%E(Xen%nbm,ik_ref(1),1)
 else 
   call msg('l','failed') 
   if (     l_skip_check) call warning("Internal check failed. A X grid k-point was not found among the RIM list")
   if (.not.l_skip_check) call error("Internal check failed. A X grid k-point was not found among the RIM list")
 endif
 !
 l_add_X_grid=.TRUE.
 !
 call section('+','BZ RIM K-points expansion ...')
 !
 ! Xk (IBZ->BZ)
 !
 call k_ibz2bz(Xk,'i',.true.)
 !
 ! RIM_k (IBZ->BZ)
 !
 call k_ibz2bz(RIM_k(i_db_ref),'i',.true.)
 !
 call msg('l','found '//trim(intc(RIM_k(i_db_ref)%nbz))//' points in the BZ')
 !
 BZ_RIM_nbands=RIM_E(i_db_ref)%nb
 BZ_RIM_tot_nkpts=RIM_k(i_db_ref)%nibz
 if (l_add_X_grid) BZ_RIM_tot_nkpts=BZ_RIM_tot_nkpts+Xk%nbz-1
 !
 allocate(BZ_RIM_nkpt(Xk%nbz))
 BZ_RIM_nkpt=0
 !
 call live_timing('BZ RIM Tables',RIM_k(i_db_ref)%nbz*2,SERIAL=.true.)
 do while(.not.allocated(BZ_RIM_table)) 
   if (maxval(BZ_RIM_nkpt)>0) then
     if (l_add_X_grid)      allocate(BZ_RIM_table(Xk%nbz,maxval(BZ_RIM_nkpt)+1))
     if (.not.l_add_X_grid) allocate(BZ_RIM_table(Xk%nbz,maxval(BZ_RIM_nkpt)))
     BZ_RIM_table=0
   endif
   BZ_RIM_nkpt=0
   do i1=1,RIM_k(i_db_ref)%nbz
     !
     ikbz=k_the_nearest(RIM_k(i_db_ref)%ptbz(i1,:),Xk%ptbz(:,:),Xk%nbz)
     BZ_RIM_nkpt(ikbz)=BZ_RIM_nkpt(ikbz)+1
     !
     if (allocated(BZ_RIM_table)) BZ_RIM_table(ikbz,BZ_RIM_nkpt(ikbz))=RIM_k(i_db_ref)%sstar(i1,1)
     !      
     call live_timing(steps=1)
     !
   enddo
 enddo
 call live_timing()
 !
 BZ_RIM_max_filling=maxval(BZ_RIM_nkpt)
 if (l_add_X_grid) BZ_RIM_max_filling=BZ_RIM_max_filling+1
 call msg('s',':: Blocks filling range  :',(/minval(BZ_RIM_nkpt),BZ_RIM_max_filling/))
 !
 ! RIM double grid interpolation weights, not used here, but must be defined.
 !
 allocate(BZ_RIM_ipol_weight(BZ_RIM_max_filling))
 BZ_RIM_ipol_weight = 1.0_SP
 !
 !  At the end I add to the Xen%E_RIM array the Xen%E energies
 !
 allocate(Xen%E_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_spin))
 Xen%E_RIM(:,:RIM_k(i_db_ref)%nibz,:)=RIM_E(i_db_ref)%E(:,:,:)
 !
 if (l_add_X_grid) then
   do i1=1,Xk%nbz
     if (i1==ik_ref(1)) cycle
     BZ_RIM_nkpt(i1)=BZ_RIM_nkpt(i1)+1
     BZ_RIM_table(i1,BZ_RIM_nkpt(i1))=RIM_k(i_db_ref)%nibz+i1
     Xen%E_RIM(:,RIM_k(i_db_ref)%nibz+i1,:)=Xen%E(:BZ_RIM_nbands,Xk%sstar(i1,1),:)
   enddo
 endif
 !
 ! Final I/O
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=NONE,ID=ID)
 i_err=ioE_RIM(Xen,ID)
 !
 ! CLEAN
 !
 deallocate(BZ_RIM_nkpt,BZ_RIM_table,Xen%E_RIM,BZ_RIM_ipol_weight)
 call k_ibz2bz(Xk,'d',.false.) 
 call k_ibz2bz(RIM_k(i_db_ref),'d',.false.) 
 !
 contains
   !
   subroutine build_3D_k()
     real(DP), external :: dlaran
     do i1=1,3
       rand_k(k_counter+1,i1)=(2.*dlaran(iseed(4:))-1.)
     enddo
     dummy=rlu_v_norm(rand_k(k_counter+1,:))
     call c2a(v_in=rand_k(k_counter+1,:),mode="ka2i")
     call k2bz(v_in=rand_k(k_counter+1,:))
   end subroutine
   !
   subroutine build_spherical_k()
     real(DP), external :: dlaran
     real(SP)           :: k_module
     k_module=gamma_radius+1.
     do while ( k_module > gamma_radius) 
       do i1=1,3
         rand_k(k_counter+1,i1)=gamma_radius*(2.*dlaran(iseed(4:))-1.)
       enddo
       k_module=v_norm(rand_k(k_counter+1,:))
       call c2a(v_in=rand_k(k_counter+1,:),mode="kc2a")
       dummy=rlu_v_norm(rand_k(k_counter+1,:))
       call c2a(v_in=rand_k(k_counter+1,:),mode="ka2i")
     enddo
   end subroutine
   !
end subroutine
