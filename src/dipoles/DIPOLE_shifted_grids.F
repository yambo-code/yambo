!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CH DS
!
subroutine DIPOLE_shifted_grids(Xen,Xk,Dip)
 !
 ! Using the wfc at the k,k-q points to construct <r>
 !
 !  i q.<nk|r|mk> =  \sum_i=1,3 q(i) <u_n(k+q0(i))|u_mk>/q0(i)
 !
 use units,               ONLY: HA2EV
 use pars,                ONLY: SP,cZERO,lchlen,schlen
 use drivers,             ONLY: l_sc_run
 use com,                 ONLY: msg,core_io_path,grid_paths
 use stderr,              ONLY: intc,STRING_split
 use LIVE_t,              ONLY: live_timing
 use electrons,           ONLY: levels,n_spinor,n_sp_pol
 use parallel_m,          ONLY: PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz_ID,&
&                               PAR_IND_VAL_BANDS_DIP,PAR_IND_VAL_BANDS_DIP_ID,&
&                               PAR_IND_CON_BANDS_DIP,PAR_IND_CON_BANDS_DIP_ID
 use interfaces,          ONLY: WF_load,WF_free,io_DB1_selective_scan
 use R_lattice,           ONLY: bz_samp,nkibz,q0_shift_norm
 use X_m,                 ONLY: l_X_terminator
 use DIPOLES,             ONLY: DIPOLE_t,DIP_iR,DIP_v,num_shifted_grids
 use vec_operate,         ONLY: degeneration_finder
 use wrapper,             ONLY: Vstar_dot_V
 use wave_func,           ONLY: wf_ng,wf_igk,WF,wf_ncx,WAVEs
#if defined _SC           
 use SC,                  ONLY: SC_bands
#endif
#if defined _RT
 use real_time,           ONLY: RT_bands
#endif
#include<dev_defs.h>
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
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
 logical                      :: idir_not_done(3)
 integer                      :: ik,ik_mem,ic_wf,iv_wf,iv_wf_sh,i_grp,ic,iv,iv_sh,iq,n_LT_steps,&
&                                i_spin,i_spinor,i_sp_pol,ir,grid_order(3),ic_min
 integer                      :: first_el(Dip%ib_lim(1)-Dip%ib(1)+1),n_of_el(Dip%ib_lim(1)-Dip%ib(1)+1),n_deg_grp
 complex(SP)                  :: rho,DIP_rotate(Dip%ib(1):Dip%ib_lim(1),Dip%ib(1):Dip%ib_lim(1)),&
&                                DIP_tmp(Dip%ib_lim(2):Dip%ib(2),Dip%ib(1):Dip%ib_lim(1))
 real(SP)                     :: Ev_m_Ec_KS,q_mod(3)
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
 !
 !
 !
 call set_parms(grid='  local', command=' define')
 !
 if (wf_ng.lt.0.8_SP*maxval(wf_igk)) &
&   call warning('All/most WF G-vectors should be used in the case of shifted grids')
 !
 grid_paths_vector=' '
 call STRING_split(grid_paths,grid_paths_vector)
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
 if(.not.Dip%force_v_g_space) Dip%Vnl_included=.true.
 !
 if ( .not.all(PAR_IND_VAL_BANDS_DIP%element_1D(Dip%ib(1):Dip%ib_lim(1))) ) &
&  call error(' DIPOLES Parallelization over v bands not accepted')
 !
 idir_not_done=.true.
 !
 do iq =1,num_shifted_grids
   !
   call msg('s','loading info from '//trim(grid_paths_vector(iq))//' ')
   !
   io_err(iq)=io_DB1_selective_scan('all',trim(grid_paths_vector(iq)),E_shift,Xk_shift)
   call set_parms(grid='shifted',command=' define')
   !
   if (io_err(iq)/=0) call error(' Shifted grid not found in folder "'//trim(grid_paths_vector(iq))//'" ')
   !
   if (nkibz_save/=nkibz_shifted) &
&    call error(' Shifted grid in folder "'//trim(grid_paths_vector(iq))//'": k-points not consistent')
   !
   if (wf_ncx_save/=wf_ncx_shifted) &
&    call warning(' Shifted grid in folder "'//trim(grid_paths_vector(iq))//'": change in number of WFs components')
   !
   call DIPOLE_check_shifted_grids( Xk_shift, Xk, iq, num_shifted_grids, q_mod, grid_order)
   !
   idir_not_done(grid_order(iq))=.false.
   !
 enddo
 !
 call msg('r','Shifted grids found '//trim(intc(num_shifted_grids)))
 !
 if(num_shifted_grids<3) call warning(' Shifted grids are '//trim(intc(num_shifted_grids))//'. Assuming non periodic system.')
 !
 n_LT_steps=n_sp_pol*PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ibz_ID+1)*&
&                    PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_IND_VAL_BANDS_DIP_ID+1)*&
&                    PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_IND_CON_BANDS_DIP_ID+1)*num_shifted_grids
 !
 call live_timing('Dipoles shifted grids   ',n_LT_steps)
 !
 do i_sp_pol = 1,n_sp_pol
   !
   do ik = 1, Xk%nibz
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     !
     ! Load WFs first at k and then at k+q_iq
     ! 
     call WF_load(WF,0,1,Dip%ib,(/ik,ik/),sp_pol_to_load=(/i_sp_pol,i_sp_pol/),space='G',&
&           title='-Oscillators/G space/Shifted',force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
     !
     do iq = 1, num_shifted_grids
       !
       ir = grid_order(iq)
       !
       core_io_path = trim(grid_paths_vector(iq))
       !
       call WF_load(WF_shifted,0,1,(/Dip%ib(1),Dip%ib_lim(1)/),(/ik,ik/),space='G',&
&      title='-Oscillators/G space (shifted grid)',keep_states_to_load=.TRUE.)
       !
       wf_ng_eff = min(wf_ng_save,wf_ng_shifted)
       !
       call degeneration_finder(Xen%E(Dip%ib(1):Dip%ib_lim(1),ik,i_sp_pol),Dip%ib_lim(1)-Dip%ib(1)+1,&
       &                           first_el,n_of_el,n_deg_grp,0.0001/HA2EV,Include_single_values=.true.)
       !
       ! Notice that here I sum over the shifted subspace, assuming that it has equal or less degeneracies than the non shifted subspace
       !
       DIP_tmp=cZERO
       DIP_rotate=cZERO
       !
       do i_grp=1,n_deg_grp
       do iv_sh=first_el(i_grp)+Dip%ib(1)-1,first_el(i_grp)+n_of_el(i_grp)-1+Dip%ib(1)-1
         !
         if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv_sh)) cycle
         !
         iv_wf_sh=WF_shifted%index(iv_sh,ik,i_sp_pol)
         !
         if (iv_wf_sh==0) call error(" Error in parallel wf_shifted distribution (Dipole iv)")
         !
         do iv=first_el(i_grp)+Dip%ib(1)-1,first_el(i_grp)+n_of_el(i_grp)-1+Dip%ib(1)-1
           !
           if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv))    cycle
           !
           iv_wf=WF%index(iv,ik,i_sp_pol)
           !
           if (iv_wf==0) call error(" Error in parallel wf distribution (Dipole iv)")
           !
           ! DIP_rotate = < v,k+q | v,k >
           !
           do i_spinor=1,n_spinor
             !
             rho = Vstar_dot_V(wf_ng_eff,DEV_VAR(WF_shifted%c)(1:wf_ng_eff,i_spinor,iv_wf_sh), &
&                                        DEV_VAR(WF%c)(1:wf_ng_eff,i_spinor,iv_wf))
             !
             if(abs(rho)>1.E-3) DIP_rotate(iv_sh,iv)=DIP_rotate(iv_sh,iv)+conjg(rho)
             !
           enddo
           !
         enddo
         !
         iv=iv_sh
         !
         ! loop over conduction
         !
         ic_min=max(iv_sh,Dip%ib_lim(2))
         if (l_X_terminator) ic_min=iv_sh
         !
         do ic=ic_min,Dip%ib(2)
           !
           if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
           !
           rho=cZERO
           !
           Ev_m_Ec_KS   =Xen%E (iv,ik,i_sp_pol)-Xen%E (ic,ik,i_sp_pol)
           if (allocated(Xen%Eo)) Ev_m_Ec_KS   =Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
           !
           if (abs(Ev_m_Ec_KS)<=Dip%Energy_treshold .or. iv==ic) then
             call live_timing(steps=1)
             cycle
           endif
           !
           ic_wf=WF%index(ic,ik,i_sp_pol)
           !
           if (ic_wf==0) call error(" Error in parallel wf distribution (Dipole ic)")
           !
           ! rho = < v,k+q | c,k >
           !
           do i_spinor=1,n_spinor
             !
             rho = rho + Vstar_dot_V(wf_ng_eff,DEV_VAR(WF_shifted%c)(1:wf_ng_eff,i_spinor,iv_wf_sh), &
&                                              DEV_VAR(WF%c)(1:wf_ng_eff,i_spinor,ic_wf))
             !
           enddo
           !
           DIP_tmp(ic,iv_sh)=rho/q0_shift_norm
           !
           call live_timing(steps=1)
           !
         enddo  ! ic
       enddo    ! iv_sh in i_grp
       enddo    ! i_grp
       !
       do i_grp=1,n_deg_grp
         do iv=first_el(i_grp)+Dip%ib(1)-1,first_el(i_grp)+n_of_el(i_grp)-1+Dip%ib(1)-1
           !
           ic_min=max(iv,Dip%ib_lim(2))
           if (l_X_terminator) ic_min=iv
           !
           ! DEBUG <
           !rho=cZERO
           !do iv_sh=first_el(i_grp)+Dip%ib(1)-1,first_el(i_grp)+n_of_el(i_grp)-1+Dip%ib(1)-1
           !  rho=rho+abs(DIP_rotate(iv_sh,iv))**2
           !enddo
           !write(*,*) i_sp_pol,ik,iq,iv,rho
           ! DEBUG >
           !
           !======
           ! <iR>
           !======
           do iv_sh=first_el(i_grp)+Dip%ib(1)-1,first_el(i_grp)+n_of_el(i_grp)-1+Dip%ib(1)-1
             DIP_iR(ir,:,iv,ik_mem,i_sp_pol)=DIP_iR(ir,:,iv,ik_mem,i_sp_pol)+DIP_tmp(:,iv_sh)*DIP_rotate(iv_sh,iv)
           enddo
           !
           !=====
           ! <v>
           !=====
           if(.not.Dip%force_v_g_space) then
             do ic=ic_min,Dip%ib(2)
               Ev_m_Ec_KS   =Xen%E (iv,ik,i_sp_pol)-Xen%E (ic,ik,i_sp_pol)
               if (allocated(Xen%Eo)) Ev_m_Ec_KS   =Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
               if (abs(Ev_m_Ec_KS)<=Dip%Energy_treshold .or. iv==ic) cycle
               DIP_v(ir,ic,iv,ik_mem,i_sp_pol)=DIP_iR(ir,ic,iv,ik_mem,i_sp_pol)*Ev_m_Ec_KS
             enddo
           endif
           !
         enddo
       enddo
       !
       call WF_free(WF_shifted)
       !
     enddo      ! iq / ir
     !
     core_io_path = trim(core_io_path_save)
     !
     call WF_free(WF)
     !
   enddo        ! ik
   !
 enddo          ! i_sp_pol
 !
 call live_timing()
 !
 ! CLEAN
 !
 call DB1_reset()
 !
 if(any(idir_not_done)) call DIPOLE_x_real_space(Xen,Xk,Dip,idir_not_done)
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
     io_err(1)=io_DB1_selective_scan('all',trim(core_io_path),E_shift,Xk_shift)
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
