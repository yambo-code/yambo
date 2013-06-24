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
subroutine Dipole_transverse(Xen,Xk,X)
 !
 ! This routine returns <iR> and <P^2>.
 !
 use pars,                ONLY:SP,pi
 use drivers,             ONLY:l_real_time
 use timing,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spin, n_spinor, n_sp_pol
 use parallel_m,          ONLY:PP_redux_wait,PAR_IND_CON_BANDS_X,PAR_IND_Xk_ibz,&
&                              PAR_Xk_ibz_index,PAR_IND_VAL_BANDS_X,&
&                              PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID,PAR_IND_CON_BANDS_X_ID,&
&                              PAR_IND_Xk_ibz_ID,PAR_COM_Xk_ibz_A2A
 use D_lattice,           ONLY:alat,n_atomic_species,n_atoms_species
 use pseudo,              ONLY:PP_free,pp_table,Vnl,pp_n_l_comp,PP_free,  &
&                              pp_kbv_dim,spin_m_table,pp_factor
 use R_lattice,           ONLY:g_vec,bz_samp
 use X_m,                 ONLY:X_t,DIP_iR,Dipole_bands_ordered,P_square,DIP_P,Dipole_Energy_treshold
 use IO_m,                ONLY:io_control,VERIFY,REP,OP_RD,RD_CL,RD,OP_RD_CL
 use memory_m,            ONLY:mem_est
 use wave_func,           ONLY:wf,wf_ng,wf_state
 use wrapper,             ONLY:M_by_V
 use com,                 ONLY:error,msg
#if defined _KERR
 use fields,              ONLY:global_gauge
#endif
 !
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 integer                      :: ik,i1,icfft,ivfft,ic,iv,iv_max,ic_min,i_pp
 integer                      :: i_spinor,i_spin,E_i_spin,pp_spin
 integer                      :: alloc_err,wf_dim,n_LT_steps,ik_mem
 complex(SP), allocatable     :: wf_xyz(:,:)
 complex(SP), allocatable     :: kbv(:,:,:)
 complex(SP)                  :: rho(4)
 real(SP)                     :: Ev_m_Ec
 !
 !I/O and external functions
 !
 integer                      :: ID,io_err(3)
 integer                      :: ioKB_abinit_err,ioKB_pwscf_err,io_Vnl_err
 integer, external            :: ioKB_abinit,ioKB_pwscf,io_Vnl
 !
 call Dipole_kb_init(Xen,X,io_err,ID)
 !
 ioKB_abinit_err= io_err(1)
 ioKB_pwscf_err = io_err(2)
 io_Vnl_err     = io_err(3)
 !
 if(ioKB_abinit_err==0.or.ioKB_pwscf_err==0) then
   allocate(kbv(wf_ng,pp_kbv_dim,4),stat=alloc_err)
   call mem_est("KBV",(/size(kbv)/),errors=(/alloc_err/))
 endif
 !
 ! Set up band limits
 !
 if (Dipole_bands_ordered.or.Dipole_Energy_treshold<0.) then
   iv_max=Xen%nbm
   ic_min=Xen%nbf+1
 else
   iv_max=X%ib(2)
   ic_min=X%ib(1)
 endif
 !
 if (associated(Xen%Eo)) call msg('rns','[X] Using Kohn-Sham energies for the dipoles ')
 !
 n_LT_steps=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)*&
&           PAR_IND_CON_BANDS_X(X%whoami)%n_of_elements(PAR_IND_CON_BANDS_X_ID(X%whoami)+1)*&
&           PAR_IND_VAL_BANDS_X(X%whoami)%n_of_elements(PAR_IND_VAL_BANDS_X_ID(X%whoami)+1)*n_spin
 !
 wf_dim=3
#if defined _SC
 if (allocated(DIP_P)) then
   call live_timing('P and P^2 (T):',n_LT_steps)
   wf_dim=4
 else
   call live_timing('Dipole (T):',n_LT_steps)
 endif
#else
 call live_timing('Dipole (T):',n_LT_steps)
#endif
 call PP_redux_wait
 !
 allocate(wf_xyz(wf_ng,wf_dim))
 !
 ! Main loop over k in IBZ
 !
 do ik=1,Xk%nibz
   !
   ! [Vnl,r]
   !
   if (ioKB_pwscf_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     ioKB_pwscf_err=ioKB_pwscf(ID)
   endif
   !
   if (ioKB_abinit_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     ioKB_abinit_err=ioKB_abinit(ID)
   endif
   ! 
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle 
   !
   ik_mem=PAR_Xk_ibz_index(ik)
   ! 
   do i_spin=1,n_spin
     ! 
     pp_spin=i_spin
     if (ioKB_abinit_err==0.and.n_spinor==2) pp_spin=1
     if (ioKB_pwscf_err ==0.and.n_sp_pol==2) pp_spin=1
     if (ioKB_abinit_err==0) call Dipole_kb_abinit_comp(ik,pp_spin,Xk,kbv)
     if (ioKB_pwscf_err ==0) call Dipole_kb_pwscf_comp(ik,pp_spin,Xk,kbv)
     !
     E_i_spin=i_spin
     if (n_spinor==2) E_i_spin=1
     !
     do ic=ic_min,X%ib(2)
       !
       if (.not.PAR_IND_CON_BANDS_X(X%whoami)%element_1D(ic)) cycle 
       !
       do iv=X%ib(1),iv_max 
         !
         if (.not.PAR_IND_VAL_BANDS_X(X%whoami)%element_1D(iv)) cycle 
         !
         rho(:)=(0._SP,0._SP)
         !
         Ev_m_Ec=Xen%E(iv,ik,E_i_spin)-Xen%E(ic,ik,E_i_spin)
         if (associated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,E_i_spin)-Xen%Eo(ic,ik,E_i_spin)
         !
         if (any((/-Ev_m_Ec<X%ehe(1).and.X%ehe(1)>0.0_SP,-Ev_m_Ec>X%ehe(2).and.X%ehe(2)>0.0_SP/)).or.&
&            abs(Ev_m_Ec)<=Dipole_Energy_treshold.and..not.l_real_time) then
           !
           call live_timing(steps=1)
           cycle
           !
         endif
         !
         ivfft=wf_state(iv,ik,i_spin)
         if (ivfft==0) call error(" Error in parallel wf distribution")
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
         if (allocated(DIP_P)) then
           wf_xyz(:wf_ng,4) = wf(1:wf_ng,ivfft) *  & 
&                            ( (Xk%pt(ik,1) + g_vec(:wf_ng,1))**2 * (2.0_SP*pi/alat(1))**2  &
&                            + (Xk%pt(ik,2) + g_vec(:wf_ng,2))**2 * (2.0_SP*pi/alat(2))**2  &
&                            + (Xk%pt(ik,3) + g_vec(:wf_ng,3))**2 * (2.0_SP*pi/alat(3))**2 )
         endif
#endif
         !
         ! GPL_EXCLUDE_END 
         !
         icfft=wf_state(ic,ik,i_spin)
         if (icfft==0) call error("Error distribution wave-function")
         !
         ! WARNING: 
         ! ========
         ! The convention is to revert the order of the bands
         ! indexes in the definition of DIP_P and DIP_iR :
         ! DIP_P(iv,ic)  = P_cv ; DIP_iR(iv,ic) = (iR)_cv 
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
         if (ioKB_abinit_err==0.or.ioKB_pwscf_err==0) call Dipole_kb_sum(ivfft,icfft,rho(:3),kbv)
         if (io_Vnl_err==0) then
           if(n_spinor==1) rho(1:3) = rho(1:3) + Vnl(1:3,ic,iv,ik,i_spin)*0.5
           if(n_spinor==2.and.i_spin==2) rho(1:3) = rho(1:3) + Vnl(1:3,ic,iv,ik,1)*0.5
         endif
         !
         !=====
         ! <P>
         !=====
         ! P_vc=rho(:3)
#if defined _SC
         if (allocated(DIP_P)) then
           DIP_P(:,ic,iv,ik_mem,i_spin)=DIP_P(:,ic,iv,ik_mem,i_spin)+rho(:3)
         endif
#endif        
#if defined _KERR
         if(trim(global_gauge)=='velocity') then
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
         if (abs(Ev_m_Ec)<=Dipole_Energy_treshold) rho(1:3)=(0._SP,0._SP)
         !
         if (n_spinor==2) then
           DIP_iR(:,ic,iv,ik_mem,1)=DIP_iR(:,ic,iv,ik_mem,1)+rho(:3)
         else 
           !
           DIP_iR(:,ic,iv,ik_mem,i_spin)=rho(:3)
           !
         endif
         !
#if defined _SC
         !======
         ! <P^2>
         !======
         if (allocated(DIP_P)) then
           if( n_spinor == 2 ) then
             P_square(ic,iv,ik_mem,1)= P_square(ic,iv,ik_mem,1)+rho(4)
           else
             P_square(ic,iv,ik_mem,i_spin)= rho(4)
           endif
         endif
#endif
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
       !
     enddo      ! valence band loop
     ! 
   enddo        ! spin loop
   ! 
 enddo          ! k-points
 !
 call live_timing()
 !
 ! CLEAN
 !
 deallocate(wf_xyz)
 if (ioKB_pwscf_err==0.or.ioKB_abinit_err==0) then
   deallocate(kbv)
   if (allocated(pp_n_l_comp))  deallocate(pp_n_l_comp)
   if (allocated(pp_table))     deallocate(pp_table)
   if (allocated(pp_factor))    deallocate(pp_factor)
   if (allocated(spin_m_table)) deallocate(spin_m_table)
   call PP_free()
   call mem_est("KBV") 
 else if (io_Vnl_err==0) then
   if (allocated(Vnl)) deallocate(Vnl)
 endif 
 !
 do ik=1,Xk%nibz
   !
   ik_mem=PAR_Xk_ibz_index(ik)
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle 
   ! 
   do i_spin=1,n_sp_pol
     call PP_redux_wait(DIP_iR(:,:,:,ik_mem,i_spin),COMM=PAR_COM_Xk_ibz_A2A%COMM)
#if defined _SC
    if (allocated(DIP_P)) then
      call PP_redux_wait(DIP_P(:,:,:,ik_mem,i_spin),COMM=PAR_COM_Xk_ibz_A2A%COMM)
      call PP_redux_wait(P_square(:,:,ik_mem,i_spin),COMM=PAR_COM_Xk_ibz_A2A%COMM)
    endif
#endif
#if defined _KERR
   if(trim(global_gauge)=='velocity') call pp_redux_wait(DIP_P(:,:,:,:,i_spin))
#endif
   enddo
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
         DIP_iR(:,ic,iv,ik_mem,:)=-conjg(DIP_iR(:,iv,ic,ik_mem,:))
#if defined _RT
         DIP_P(:,ic,iv,ik_mem,:) = conjg(DIP_P(:,iv,ic,ik_mem,:))
         P_square(ic,iv,ik_mem,:)= conjg(P_square(iv,ic,ik_mem,:))
#endif        
#if defined _KERR
         if(trim(global_gauge)=="velocity")DIP_P(:,ic,iv,ik,:) = conjg(DIP_P(:,iv,ic,ik,:))
#endif        
       enddo
     enddo
   endif
   !
 enddo
 !
end subroutine Dipole_transverse
