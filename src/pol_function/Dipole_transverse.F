!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine Dipole_transverse(Xen,Xk,X)
 !
 ! This routine returns <iR> and <P^2>.
 !
 use pars,                ONLY:SP,pi
 use drivers,             ONLY:l_real_time
 use timing,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spin, n_spinor, n_sp_pol
 use par_proc_m,          ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,       ONLY:par_indexes
 use D_lattice,           ONLY:alat,n_atomic_species,n_atoms_species
 use pseudo,              ONLY:pp_kbv_dim,pp_n_l_times_proj_max,PP_free,pp_table, &
&                              Vnl,pp_n_l_comp,kbv_spin
 use R_lattice,           ONLY:g_vec,bz_samp
 use X_m,                 ONLY:X_t,DIP_iR,Dipole_bands_ordered,P_square,DIP_P
 use IO_m,                ONLY:io_control,VERIFY,REP,OP_RD,RD_CL,RD,OP_RD_CL
 use memory_m,            ONLY:mem_est
 use wave_func,           ONLY:wf,wf_ng,wf_state, WF_load
 use wrapper,             ONLY:M_by_V
#if defined _DISTRIBUTED
 use com,                 ONLY:error
#endif
#if defined _KERR
 use KERR,           ONLY:chi_jj
#endif
 !
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 integer                      :: ik,i1,icfft,ivfft,ic,iv,iv_max,ic_min,i_pp,pp_spin
 integer                      :: is,ia,il,im,alloc_err,i_spin,E_i_spin,wf_dim
 complex(SP), allocatable     :: wf_xyz(:,:), kbv(:,:,:)
 complex(SP)                  :: rho(4)
 real(SP)                     :: Ev_m_Ec
 type(pp_indexes)             :: px
 !
 !I/O and external functions
 !
 integer                      :: ID,io_err,ioKB_err, io_Vnl_err
 integer, external            :: ioKB_PP, io_Vnl
 !
 X%Vnl_included=.false.
 !
 ! Check for the Vnl DB
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_Vnl_err=io_Vnl(X,Xen,ID)
 !
 ! Check for the KB PP DB 
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 ioKB_err=ioKB_PP(ID)
 !
 X%Vnl_included= ioKB_err==0 .or. io_Vnl_err==0
 !
 if (ioKB_err==0) then
   pp_kbv_dim=0
   do is = 1,n_atomic_species
     do ia = 1,n_atoms_species(is)
       do i_pp = 1, pp_n_l_times_proj_max
         il = pp_table(1,is,i_pp)
         pp_spin = pp_table(3,is,i_pp)
         if (pp_spin==0) cycle
         do im = 1,2*(il-1)+1
           pp_kbv_dim=pp_kbv_dim+1
         enddo
       enddo
     enddo
   enddo
   !
   allocate(kbv(wf_ng,pp_kbv_dim,7),stat=alloc_err)
   allocate(kbv_spin(pp_kbv_dim))
   !
   call mem_est("KBV",(/size(kbv)/),errors=(/alloc_err/))
 endif
 !
 ! Set up band limits
 !
 if (Dipole_bands_ordered) then
   iv_max=Xen%nbm
   ic_min=Xen%nbf+1
 else
   iv_max=X%ib(2)
   ic_min=X%ib(1)
 endif
 !
 ! Set up the parallel environment
 !
 call pp_indexes_reset(px)
#if defined _DISTRIBUTED
 call par_indexes(px,(/Xk%nibz,X%ib(2)/),(/1,ic_min/))
#else
 call par_indexes(px,(/Xk%nibz,iv_max/),(/1,X%ib(1)/))
#endif
 !
#if defined _SC
 call live_timing('P and P^2 (T):',px%n_of_elements(myid+1)*n_spin)
 wf_dim=4
#else
 call live_timing('Dipole (T):',px%n_of_elements(myid+1)*n_spin)
 wf_dim=3
#endif
 call pp_redux_wait
 !
 allocate(wf_xyz(wf_ng,wf_dim))
 !
 ! Main loop over k in IBZ
 !
 do ik=1,Xk%nibz
#if defined _MANYK
   call WF_load(0,1,X%ib,(/ik,ik/),space='G',title='-Oscillators/G space')
#endif
   !
   ! [Vnl,r]
   !
   if (ioKB_err==0) then
     if (ik< Xk%nibz) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_err=ioKB_PP(ID)
   endif
   ! 
   ! GPL_INCLUDE_START SPIN_IGNORE
   ! i_spin=1
   ! GPL_INCLUDE_END SPIN_IGNORE
   ! 
   ! GPL_EXCLUDE_START SPIN_IGNORE
   !
   do i_spin=1,n_spin
     ! 
     ! GPL_EXCLUDE_END SPIN_IGNORE
     !
     pp_spin=i_spin
     if (n_spinor==2) pp_spin=1
     if (ioKB_err==0) call Dipole_kb_pp_comp(ik,pp_spin,Xk,kbv)
     !
     E_i_spin=i_spin
     if (n_spinor==2) E_i_spin=1
     !
#if defined _DISTRIBUTED
     do ic=ic_min,X%ib(2)
       !
       if(.not.px%element_2D(ik,ic)) cycle
       !
       do iv=X%ib(1),iv_max 
#else
     do iv=X%ib(1),iv_max 
       !
       if (.not.px%element_2D(ik,iv)) cycle
       !
       do ic=ic_min,X%ib(2)
#endif
         !
         rho(:)=(0._SP,0._SP)
         !
         Ev_m_Ec=Xen%E(iv,ik,E_i_spin)-Xen%E(ic,ik,E_i_spin)
         if (associated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,E_i_spin)-Xen%Eo(ic,ik,E_i_spin)
         !
         if (any((/-Ev_m_Ec<X%ehe(1).and.X%ehe(1)>0.0_SP,-Ev_m_Ec>X%ehe(2).and.X%ehe(2)>0.0_SP/))) cycle
         !
         if (abs(Ev_m_Ec)<=1.E-5_SP.and..not.l_real_time) cycle
         !
         ivfft=wf_state(iv,ik,i_spin)
#if defined _DISTRIBUTED
         if(ivfft==0) call error(" Error in parallel wf distribution")
#endif
         !
         ! wf_xyz = -i grad fdo(iv,ik)
         !
         do i1=1,3
           wf_xyz(:,i1)=wf(:,ivfft)*(Xk%pt(ik,i1)+g_vec(:wf_ng,i1))*2.0_SP*pi/alat(i1)
         enddo
         ! 
         ! GPL_EXCLUDE_START 
         !
#if defined _SC
         wf_xyz(:wf_ng,4) = wf(1:wf_ng,ivfft) *  & 
&                          ( (Xk%pt(ik,1) + g_vec(:wf_ng,1))**2 * (2.0_SP*pi/alat(1))**2  &
&                          + (Xk%pt(ik,2) + g_vec(:wf_ng,2))**2 * (2.0_SP*pi/alat(2))**2  &
&                          + (Xk%pt(ik,3) + g_vec(:wf_ng,3))**2 * (2.0_SP*pi/alat(3))**2 )
#endif
         !
         ! GPL_EXCLUDE_END 
         !
         icfft=wf_state(ic,ik,i_spin)
#if defined _DISTRIBUTED
         if(icfft==0) call error("Error distribution wave-function")
#endif
         !
         ! WARNING !!!!!!!! The convention is to revert the order of the bands indexes
         !==================
         ! in the definition of DIP_P and DIP_iR
         !
         ! DIP_P(iv,ic)  = P_cv
         ! DIP_iR(iv,ic) = (iR)_cv 
         !
         ! Note that P is Hermitian and iR anti-hermitian.
         !
         ! rho = <v|p|c> = P_vc =
         !     = wf_xyz^* fdo(ic,ik)= - i < fdo(iv,ik)| grad |  fdo(ic,ik) > =
         !       \sum_G -i u^*_{iv ik}(G) i ( k + G ) u_{ic ik}(G)
         !     = \sum_G u^*_{iv ik}(G) ( k + G ) u_{ic ik}(G)
         !
         ! rho(4) =  \sum_G u^*_{iv ik}(G) ( k + G )^2 u_{ic ik}(G)
         !
         call M_by_V('C',wf_ng,wf_dim,(1._SP,0._SP),wf_xyz,wf_ng,wf(:,icfft),1,(0._SP,0._SP),rho,1)
         !
         if (ioKB_err==0) call Dipole_kb_sum(ivfft,icfft,rho(:3),kbv)
         if (ioKB_err==0 .and. n_spinor==2) call Dipole_kb_sum_SO(iv,ic,ik,i_spin,rho(:3),kbv)
         if(io_Vnl_err==0) rho(1:3) = rho(1:3) + Vnl(1:3,ic,iv,ik,1)*0.5
         !
         !=====
         ! <P>
         !=====
         ! P_vc=rho(:3)
#if defined _SC
         DIP_P(:,ic,iv,ik,i_spin)=DIP_P(:,ic,iv,ik,i_spin)+rho(:3)
#endif        
#if defined _KERR
         if(chi_jj) then
           if (n_spinor==2) then
             DIP_P(:,ic,iv,ik,1)=DIP_P(:,ic,iv,ik,1)+rho(:3)
           else
             DIP_P(:,ic,iv,ik,i_spin)=rho(:3)
           endif
         endif
#endif        
         !
         !======
         ! <iR>
         !======
         ! as [x,p_x]=i we get [x,H] = [x,p^2/2]= i p_x. So from rho (that is P_vc) we can extract
         !
         ! DIP_iR(c,v) = i <v|r|c> = i <v|[r,H]|c>/(Ec-Ev) = i <v|i p|c>/(Ec-Ev) =
         !             = - <v | -i grad |c> / (Ec-Ev) = <v|-i grad|c>/(Ev-Ec) = P_vc/(Ev-Ec)
         !
         rho(1:3)=rho(1:3)/Ev_m_Ec
         !
         if (abs(Ev_m_Ec)<=1.E-5_SP) rho(1:3)=(0._SP,0._SP)
         !
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
         if (n_spinor==2) then
           DIP_iR(:,ic,iv,ik,1)=DIP_iR(:,ic,iv,ik,1)+rho(:3)
         else 
           !
           ! GPL_EXCLUDE_END SPIN_IGNORE
           !
           DIP_iR(:,ic,iv,ik,i_spin)=rho(:3)
           !
           ! GPL_EXCLUDE_START SPIN_IGNORE
           !
         endif
         !
#if defined _SC
         !======
         ! <P^2>
         !======
         if( n_spinor == 2 ) then
           P_square(ic,iv,ik,1)= P_square(ic,iv,ik,1)+rho(4)
         else
           P_square(ic,iv,ik,i_spin)= rho(4)
         endif
#endif
         !
         ! GPL_EXCLUDE_END SPIN_IGNORE
         !
       enddo    ! conduction band loop
       !
       call live_timing(steps=1)
       !
     enddo      ! valence band loop
     ! 
     ! GPL_EXCLUDE_START SPIN_IGNORE
     !
   enddo        ! spin loop
   ! 
   ! GPL_EXCLUDE_END SPIN_IGNORE
   !
   ! Symmetrization 
   !================
   !
   ! To avoid numerical noise in the real-time evolution we need to impose
   ! P (iR) to be Hermitian (anti-Hermitian)
   !
   if (.not.Dipole_bands_ordered) then
     do iv=X%ib(1),X%ib(2)
       do ic=iv+1,X%ib(2)
         DIP_iR(:,ic,iv,ik,:)=-conjg(DIP_iR(:,iv,ic,ik,:))
#if defined _RT
         DIP_P(:,ic,iv,ik,:) = conjg(DIP_P(:,iv,ic,ik,:))
         P_square(ic,iv,ik,:)= conjg(P_square(iv,ic,ik,:))
#endif        
#if defined _KERR
         if(chi_jj) DIP_P(:,ic,iv,ik,:) = conjg(DIP_P(:,iv,ic,ik,:))
#endif        
       enddo
     enddo
   endif
   !
 enddo          ! k-points
 !
 call live_timing
 !
 ! CLEAN
 !
 deallocate(wf_xyz)
 call pp_indexes_reset(px)
 if (ioKB_err==0) then
   deallocate(kbv,pp_n_l_comp)
   if (allocated(pp_table)) deallocate(pp_table)
   if (allocated(kbv_spin)) deallocate(kbv_spin)
   call PP_free()
   call mem_est("KBV pp_n_l_comp pp_table")
 else if (io_Vnl_err==0) then
   if (allocated(Vnl)) deallocate(Vnl)
 endif 
 !
 do i_spin=1,n_sp_pol
   call pp_redux_wait(DIP_iR(:,:,:,:,i_spin))
#if defined _SC
   call pp_redux_wait(DIP_P(:,:,:,:,i_spin))
   call pp_redux_wait(P_square(:,:,:,i_spin))
#endif
#if defined _KERR
   if(chi_jj) call pp_redux_wait(DIP_P(:,:,:,:,i_spin))
#endif
 enddo
 !
end subroutine Dipole_transverse
