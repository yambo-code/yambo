!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine DIPOLE_shifted_grids(Xen,Xk,X)
 !
 ! Using the wfc at the k,k-q points to construct <r>
 !
 !  i q.<nk|r|mk> =  \sum_i=1,3 q(i) <u_n(k+q0(i))|u_mk>/q0(i)
 !
 use pars,                ONLY: SP,cZERO,lchlen,schlen
 use drivers,             ONLY: l_sc_run
 use com,                 ONLY: warning,msg,core_io_path,error,grid_paths
 use stderr,              ONLY: intc,string_split
 use LIVE_t,              ONLY: live_timing
 use electrons,           ONLY: levels,n_spinor,n_sp_pol
 use parallel_m,          ONLY: PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index
 use interfaces,          ONLY: WF_load,WF_free
 use R_lattice,           ONLY: bz_samp,nkibz,q0_shift_norm
 use X_m,                 ONLY: X_t,DIP_iR,DIP_P,l_X_terminator,&
&                               force_v_g_space_approach,num_shifted_grids,Dipole_Energy_treshold
 use wrapper,             ONLY: Vstar_dot_V
 use wave_func,           ONLY: wf_ng,wf_igk,WF,wf_ncx,WAVEs
#if defined _SC           
 use SC,                  ONLY: SC_bands
#endif
#if defined _RT
 use real_time,           ONLY: RT_bands
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
#if defined _RT
 integer                      :: RT_bands_save(2)
#endif
 !
 ! Work Space
 !
 character(schlen)            :: grid_paths_vector(3)
 logical                      :: idir_not_done(3)=.true.,&
&                                l_mat_eval_DIP(X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
 integer                      :: ik,ik_mem,icfft(n_spinor),ivfft(n_spinor),ic,iv,iq,n_LT_steps,&
&                                i_spin,i_spinor,i_sp_pol,ir,grid_order(3),ic_min
 complex(SP)                  :: rho
 real(SP)                     :: Ev_m_Ec,Ev_m_Ec_QP,q_mod(3)
 !
 !ioDB1 shifted grids
 !
 type(levels)                 :: E_shift
 type(bz_samp)                :: Xk_shift
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
 grid_paths_vector=' '
 call string_split(grid_paths,grid_paths_vector)
 !
 num_shifted_grids=0
 do iq=1,3
   if(trim(grid_paths_vector(iq))/=' ') num_shifted_grids=num_shifted_grids+1
 enddo
 !
 if(num_shifted_grids==0) call error(' No shifted grid specified in input')
 !
 ! Check grids and determine grid displacement
 !
 grid_order=0
 !
 do iq =1,num_shifted_grids
   !
   call msg('s','loading info from '//trim(grid_paths_vector(iq))//' ')
   !
   io_err(iq)=io_DB1_selective_scan(E_shift,Xk_shift,trim(grid_paths_vector(iq)),'all')
   call set_parms(grid='shifted',command=' define')
   !
   if (io_err(iq)/=0) call error(' Shifted grid not found in folder "'//trim(grid_paths_vector(iq))//'" ')
   !
   if (any( (/nkibz_save/=nkibz_shifted, wf_ncx_save/=wf_ncx_shifted/) ) ) &
&    call error(' Shifted grid in folder "'//trim(grid_paths_vector(iq))//'": parameters not consistent')
   !
   call DIPOLE_check_shifted_grids( Xk_shift, Xk, iq, num_shifted_grids, q_mod, grid_order)
   !
   idir_not_done(grid_order(iq))=.false.
   !
 enddo
 !
 call msg('r','Shifted grids found: '//trim(intc(num_shifted_grids)))
 !
 if(num_shifted_grids<3) call warning(' Shifted grids are '//trim(intc(num_shifted_grids))//'. Assuming non periodic system.')
 !
 ! Load unshifted WFs
 ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
 ! there will be other WF_load calls using the present WF distribution. 
 !
 call WF_load(WF,0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space',&
&             force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
 !
 ! Main loop over cartesian directions
 !
 call DIPOLE_build_parallelization_mask(X,l_mat_eval_DIP,n_LT_steps)
 !
 do iq = 1, num_shifted_grids
   !
   core_io_path = trim(grid_paths_vector(iq))
   !
   ! Load WFs {k + q_iq}
   !
   call WF_load(WF_shifted,0,1,X%ib,(/1,Xk%nibz/),space='G',&
   &  title='-Oscillators/G space (shifted grid)',keep_states_to_load=.TRUE.) 
   !
   wf_ng_eff = min(wf_ng_save,wf_ng_shifted)
   !
   ir = grid_order(iq)
   !
   call live_timing('Dipole @ grid '//trim(intc(iq))//':',n_LT_steps)
   !
   do i_sp_pol = 1,n_sp_pol
     !
     do ik = 1, Xk%nibz
       !
       if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
       !
       ik_mem=PAR_DIPk_ibz_index(ik)
       !
       do iv=X%ib(1),X%ib_lim(1) 
         !
         ic_min=max(iv,X%ib_lim(2))
         if (l_X_terminator) ic_min=iv
         !
         do ic=ic_min,X%ib(2)
           ! 
           if (.not.l_mat_eval_DIP(ic,iv)) cycle
           !
           rho=cZERO
           !
           if (allocated(Xen%Eo)) then 
             Ev_m_Ec   =Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
             Ev_m_Ec_QP=Xen%E (iv,ik,i_sp_pol)-Xen%E (ic,ik,i_sp_pol)
           else
             Ev_m_Ec   =Xen%E (iv,ik,i_sp_pol)-Xen%E (ic,ik,i_sp_pol)
           endif
           !
           if (abs(Ev_m_Ec)<=Dipole_Energy_treshold .or. iv==ic) then
             call live_timing(steps=1)
             cycle
           endif
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
             !
             rho = rho + Vstar_dot_V(wf_ng_eff,WF_shifted%c(1:wf_ng_eff,ivfft(i_spinor)), &
&                                                  WF%c(1:wf_ng_eff,icfft(i_spinor)))
             !
           enddo
           !
           !======
           ! <iR>
           !======
           DIP_iR(ir,ic,iv,ik_mem,i_sp_pol)=rho/q0_shift_norm
           !
           !=====
           ! <P>
           !=====
           if(.not.force_v_g_space_approach) DIP_P(ir,ic,iv,ik_mem,i_sp_pol)=rho/q0_shift_norm*Ev_m_Ec
           !
           call live_timing(steps=1)
           !
         enddo  ! ic
       enddo    ! iv
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
     !
     call set_parms(grid='  local', command='  reset')
     !
   end subroutine
   !
   subroutine set_parms(grid,command)
     implicit none
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
#if defined _RT
       RT_bands_save     = RT_bands
#endif 
     else if( grid.eq.'shifted'.and. command.eq.' define') then
       nkibz_shifted  = nkibz
       wf_ncx_shifted = wf_ncx
       wf_ng_shifted  = wf_ng
     else if( grid.eq.'  local'.and. command.eq.'  reset') then
       core_io_path = core_io_path_save
       wf_ng        = wf_ng_save
#if defined _SC       
       SC_bands = SC_bands_save
#endif
#if defined _RT
       RT_bands = RT_bands_save
#endif
     endif
     return
   end subroutine set_parms
   !
end subroutine DIPOLE_shifted_grids
