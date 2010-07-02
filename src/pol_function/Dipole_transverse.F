!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
 use pars,                ONLY:SP,pi
 use timing,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spin, n_spinor, n_sp_pol
 use par_proc_m,          ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,       ONLY:par_indexes
 use D_lattice,           ONLY:alat,n_atomic_species,n_atoms_species
 use pseudo,              ONLY:pp_kbv_dim,pp_n_l_comp,PP_free
 use R_lattice,           ONLY:g_vec,bz_samp
 use X_m,                 ONLY:X_t,DIP_iR_or_P,Dipole_bands_ordered,P_square
 use fields,              ONLY:global_gauge
 use IO_m,                ONLY:io_control,VERIFY,REP,OP_RD,RD_CL,RD
 use memory_m,            ONLY:mem_est
 use wave_func,           ONLY:wf,wf_ng,wf_state, WF_load
 use wrapper,             ONLY:M_by_V
#if defined _DISTRIBUTED
 use com,                 ONLY:error
#endif
 !
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 integer                      :: ik,i1,icfft,ivfft,ic,iv,iv_max,ic_min
 integer                      :: is,ia,il,im,alloc_err,i_spin,E_i_spin,wf_dim
 complex(SP), allocatable     :: wf_xyz(:,:), kbv(:,:,:)
 complex(SP)                  :: rho(4)
 real(SP)                     :: Ev_m_Ec
 type(pp_indexes)             :: px
 !
 !I/O and external functions
 !
 integer                      :: ID,io_err,ioKB_err
 integer, external            :: ioKB_PP
 !
 ! Check for the KB PP DB 
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 ioKB_err=ioKB_PP(ID)
 !
 if (ioKB_err==0) then
   X%Vnl_included=.true.
   pp_kbv_dim=0
   do is = 1,n_atomic_species
     do ia = 1,n_atoms_species(is)
       do il = 1, pp_n_l_comp(is)
         do im = 1,2*(il-1)+1
           pp_kbv_dim=pp_kbv_dim+1
         enddo
       enddo
     enddo
   enddo
   allocate(kbv(wf_ng,pp_kbv_dim,4),stat=alloc_err)
   call mem_est("KBV",(/size(kbv)/),errors=(/alloc_err/))
 else
   X%Vnl_included=.false.
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
 call live_timing('Dipole (T):',px%n_of_element_1D(myid+1)*n_spin)
 call pp_redux_wait
 !
 wf_dim=3
#if defined _RT
 wf_dim=4
#endif
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
     if (ioKB_err==0) call Dipole_kb_pp_comp(ik,i_spin,Xk,kbv)
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
         if (any((/-Ev_m_Ec<X%ehe(1).and.X%ehe(1)>0.0_SP,&
&                  -Ev_m_Ec>X%ehe(2).and.X%ehe(2)>0.0_SP/))) cycle
         !
         if (abs(Ev_m_Ec)<=1.E-5_SP.and.trim(global_gauge)=='length') cycle
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
#if defined _RT 
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
         ! rho = wf_xyz^* fdo(ic,ik)= - i < fdo(iv,ik)| grad |  fdo(ic,ik) > =
         !       \sum_G -i u^*_{iv ik}(G) i ( k + G ) u_{ic ik}(G)
         !     = \sum_G u^*_{iv ik}(G) ( k + G ) u_{ic ik}(G)
         !
         ! rho(4) =  \sum_G u^*_{iv ik}(G) ( k + G )^2 u_{ic ik}(G)
         !
         call M_by_V('C',wf_ng,wf_dim,(1._SP,0._SP),wf_xyz,wf_ng,wf(:,icfft),1,(0._SP,0._SP),rho,1)
         !
         if (ioKB_err==0) call Dipole_kb_sum(ivfft,icfft,rho(:3),kbv)
         !
         ! as [x,p_x]=i we get [x,H] = [x,p^2/2]= i p_x
         !
         !     DIP_iR_or_P(c,v) = i <v|r|c> = i <v|[r,H]|c>/(Ec-Ev) = i <v|i p|c>/(Ec-Ev) =
         !                   = - <v | -i grad |c> / (Ec-Ev) = <v|-i grad|c>/(Ev-Ec)
         !
         if (trim(global_gauge)=='length') rho(1:3)=rho(1:3)/Ev_m_Ec
         !
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
         if (n_spinor==2) then
           DIP_iR_or_P(:,ic,iv,ik,1)=DIP_iR_or_P(:,ic,iv,ik,1)+rho(:3)
         else 
           !
           ! GPL_EXCLUDE_END SPIN_IGNORE
           !
           DIP_iR_or_P(:,ic,iv,ik,i_spin)=rho(:3)
           !
           ! GPL_EXCLUDE_START SPIN_IGNORE
           !
         endif
         !
#if defined _RT
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
 enddo          ! k-points
 !
 call live_timing
 !
 ! CLEAN
 !
 deallocate(wf_xyz)
 call pp_indexes_reset(px)
 if (ioKB_err==0) then
   deallocate(kbv)
   call mem_est("KBV")
   call PP_free()
 endif 
 !
 do i_spin=1,n_sp_pol
   call pp_redux_wait(DIP_iR_or_P(:,:,:,:,i_spin))
#if defined _RT
   call pp_redux_wait(P_square(:,:,:,i_spin))
#endif
 enddo
 !
end subroutine Dipole_transverse
