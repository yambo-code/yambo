!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CH DS
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
subroutine Dipole_shifted_grids(Xen,Xk,X)
 !
 ! Using the wfc at the k,k-q points to construct <r>
 !
 !  i q.<nk|r|mk> =  \sum_i=1,3 q(i) <u_n(k+q0(i))|u_mk>/q0(i)
 !
 use pars,                ONLY: SP,lchlen,cZERO
 use drivers,             ONLY: l_sc_run
 use com,                 ONLY: warning,msg,core_io_path,error,grid_path
 use stderr,              ONLY: intc
 use LIVE_t,              ONLY: live_timing
 use electrons,           ONLY: levels,n_spinor,n_sp_pol
 use parallel_m,          ONLY: PP_redux_wait,PP_indexes,myid,&
&                               PP_indexes_reset,PAR_COM_k_for_P
 use interfaces,          ONLY: PARALLEL_index,WF_load
 use R_lattice,           ONLY: bz_samp,nkibz,q0_shift_norm
 use X_m,                 ONLY: X_t,DIP_iR,DIP_P,use_shifted_grids_approach,&
&                               force_v_g_space_approach,num_shifted_grids
 use wrapper_omp,         ONLY: Vstar_dot_V_omp
 use memory_m,            ONLY: mem_est
 use wave_func,           ONLY: wf_ng,wf_igk,WF,wf_ncx,wf_nc_k,WAVEs
#if defined _SC           
 use SC,                  ONLY: SC_bands
#endif
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 ! Saves
 !
 integer                      :: wf_ng_save
 character(lchlen)            :: core_io_path_save
 integer                      :: nkibz_save, wf_ncx_save, wf_ncx_shifted, nkibz_shifted
 integer                      :: wf_ng_eff, wf_ng_shifted
#if defined _SC
 integer                      :: SC_bands_save(2)
#endif
 !
 ! Work Space
 !
 logical                      :: idir_not_done(3)=.true.
 integer                      :: ik,icfft(n_spinor),ivfft(n_spinor),ic,iv,iq
 integer                      :: i_spin,i_spinor,i_sp_pol,ifail,ir
 complex(SP)                  :: rho
 real(SP)                     :: Ev_m_Ec,q_mod(3)
 type(PP_indexes)             :: px
 !
 !ioDB1 shifted grids
 !
 type(levels)                 :: E_shift(3)
 type(bz_samp)                :: Xk_shift(3)
 !
 !ioWF local copies           
 !
 type(WAVEs)                  :: WF_shifted
 !
 !I/O and external functions
 !
 integer                      :: io_err(3)
 integer, external            :: io_DB1_selective_scan
 !
 ! saves...
 !
 call set_parms(grid='  local', command=' define')
 !
 if (wf_ng.lt.0.8_SP*maxval(wf_igk)) &
&   call warning('All/most WF G-vectors should be used in the case of shifted grids')
 !
 ! Read the k-point grids from second ioDB1
 !
 num_shifted_grids=2
 !
 do iq =1,num_shifted_grids
   !
   io_err(iq)=io_DB1_selective_scan(E_shift(iq),Xk_shift(iq),trim(grid_path)//'/shift_'//trim(intc(iq)),'all')
   call set_parms(grid='shifted',i_grid=iq,command=' define')
   !
   idir_not_done(iq)= (io_err(iq)/=0)
   !
 enddo
 !
 if(all(idir_not_done)) call error(' Shifted grids are not ok')
 !
 call msg('r','Shifted grids found: '//trim(intc(num_shifted_grids)))
 !
 if(any(idir_not_done)) call warning(' Shifted grids are '//trim(intc(num_shifted_grids))//'. Assuming non periodic system.')
 !
 ! Load unshifted WFs
 ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
 ! there will be other WF_load calls using the present WF distribution. 
 !
 call WF_load(WF,0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space',&
&             force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
 !
 !
 ! Load WFs {k + q_iq}
 !
 ! Main loop over cartesian directions
 !
 do iq = 1, num_shifted_grids
   !
   if(idir_not_done(iq)) cycle
   !
   ! Determine grid displacement
   !
   call DIPOLE_check_shifted_grids( Xk_shift(iq), Xk, iq, num_shifted_grids, q_mod)
   !
   core_io_path = trim(grid_path)//'/shift_'//trim(intc(iq))
   !
   call WF_load(WF_shifted,0,1,X%ib,(/1,Xk%nibz/),space='G',&
   &  title='-Oscillators/G space (shifted grid)',keep_states_to_load=.TRUE.) 
   !
   wf_ng_eff = min(wf_ng_save,wf_ng_shifted)
   !
   ir = iq
   !
   ! Set up the parallel environment 
   !
   !call PP_indexes_reset(px)
   !call PARALLEL_index(px,(/Xk%nibz,Xen%nbm/),(/1,X%ib(1)/))
   !call live_timing('Dipole @ grid '//trim(intc(iq))//':',px%n_of_elements(myid+1))
   !call PP_redux_wait(COMM=PAR_COM_k_for_P%COMM)
   !
   call live_timing('Dipole @ grid '//trim(intc(iq))//':',Xk%nibz*n_sp_pol)
   !
   do ik = 1, Xk%nibz   ! Loop over k's in IBZ [1:nkibz]
     !
     do i_sp_pol = 1,n_sp_pol
       !
       do iv = X%ib(1),Xen%nbm
         !
         !if (.not.px%element_2D(ik,iv)) cycle
         !
         do ic = Xen%nbf+1, X%ib(2)
           !
           rho=cZERO
           !
           ! No need to shift Efermi
           !
           Ev_m_Ec=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
           !
           if (any( (/ -Ev_m_Ec < X%ehe(1) .and. X%ehe(1) > 0.0_SP, &
&                      -Ev_m_Ec > X%ehe(2) .and. X%ehe(2) > 0.0_SP /))) cycle
           if (abs(Ev_m_Ec)<=1.E-5_SP) cycle
           !
           do i_spinor=1,n_spinor
             !
             i_spin=i_sp_pol+i_spinor-1
             ivfft(i_spinor)=WF_shifted%index(iv,ik,i_spin)
             icfft(i_spinor)=WF%index(ic,ik,i_spin)
             !
             if (ivfft(i_spinor)==0) call error(" Error in parallel wf distribution (Dipole iv)")
             if (icfft(i_spinor)==0) call error(" Error in parallel wf distribution (Dipole ic)")
             !
             ! rho = < v,k+q | c,k >
             !
             rho = rho + Vstar_dot_V_omp(wf_ng_eff,WF_shifted%c(1:wf_ng_eff,ivfft(i_spinor)), &
&                                                  WF%c(1:wf_ng_eff,icfft(i_spinor)))
             !
           enddo
           !
           !======
           ! <iR>
           !======
           DIP_iR(ir,ic,iv,ik,i_sp_pol)=rho/q0_shift_norm
           !
           !=====
           ! <P>
           !=====
           if(.not.force_v_g_space_approach) DIP_P(ir,ic,iv,ik,i_sp_pol)=rho/q0_shift_norm*Ev_m_Ec
           !
         enddo  ! ic
         !
       enddo    ! iv
       !
       call live_timing(steps=1)
       !
     enddo      ! i_sp_pol
     !
   enddo        ! ik
   !
   call WF_free(WF_shifted)
   !
   call live_timing()
   !
 enddo          ! iq
 !
 ! CLEAN
 !
 call WF_free(WF)
 !
 !do i_spin=1,n_sp_pol
 !  call PP_redux_wait(DIP_iR(:,:,:,:,i_spin),COMM=PAR_COM_k_for_P%COMM)
 !enddo
 !
 call DB1_reset()
 !
 if(any(idir_not_done)) call DIPOLE_x_real_space(Xen,Xk,X,idir_not_done)
 !
 contains
   !
   subroutine DB1_reset()
     !
     ! Reload g_vec, dl_sop ... from db1 using _shift types
     ! so that all type components (like occupations) are 
     ! not rewritten.
     !
     core_io_path = core_io_path_save
     io_err(1)=io_DB1_selective_scan(E_shift,Xk_shift,trim(core_io_path),'all')
     wf_ng=wf_ng_shifted
     !
     call set_parms(grid='  local', command='  reset')
     !
   end subroutine
   !
   subroutine set_parms(grid,i_grid,command)
     implicit none
     integer,          intent(in), optional :: i_grid
     character(len=7), intent(in), optional :: grid, command
     !
     if(      grid.eq.'  local'.and. command.eq.' define') then
       nkibz_save   = nkibz
       wf_ncx_save  = wf_ncx
       wf_ng_save   = wf_ng
       core_io_path_save = core_io_path
#if defined _SC       
       SC_bands_save     = SC_bands
#endif 
     else if( grid.eq.'shifted'.and. command.eq.' define') then
       nkibz_shifted  = nkibz
       wf_ncx_shifted = wf_ncx
       wf_ng_shifted  = wf_ng
     else if( grid.eq.'  local'.and. command.eq.'  reset') then
       core_io_path = core_io_path_save
       nkibz   = nkibz_save
       wf_ncx  = wf_ncx_save
       wf_ng   = wf_ng_save
#if defined _SC       
       SC_bands = SC_bands_save
#endif
     else if( grid.eq.'shifted'.and. command.eq.'  reset') then
       nkibz   = nkibz_shifted
       wf_ncx  = wf_ncx_shifted
       wf_ng   = wf_ng_shifted
     endif
     return
   end subroutine set_parms
   !
end subroutine Dipole_shifted_grids
