!
! Copyright (C) 2000-2013 C. Hogan, A. Marini and the YAMBO team 
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
subroutine Dipole_shifted_grids(Xen,Xk,X,field_dir,use_trans_gauge)
 !
 ! Using the wfc at the k,k-q points to construct <r>
 !
 !  i q.<nk|r|mk> =  \sum_i=1,3 q(i) <u_n(k+q0(i))|u_mk>/q0(i)
 !
 use pars,                ONLY: SP,lchlen
 use com,                 ONLY: warning, core_io_path, error
 use stderr,              ONLY: intc
 use timing,              ONLY: live_timing
 use electrons,           ONLY: levels,n_spin,  n_spinor,n_sp_pol
 use par_proc_m,          ONLY: pp_redux_wait,pp_indexes,myid,&
&                               pp_indexes_reset
 use par_indexes_m,       ONLY: par_indexes
 use R_lattice,           ONLY: bz_samp,nkibz,q0_shift_norm
 use X_m,                 ONLY: X_t,DIP_iR 
 use fields,              ONLY: grid_path 
 use memory_m,            ONLY: mem_est
 use wave_func,           ONLY: wf,wf_ng,wf_igk,wf_state,WF_load,WF_free , wf_ncx
#if defined _SC           
 use SC,                  ONLY: SC_bands
#endif
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 logical,       intent(inout) :: use_trans_gauge
 real(SP),      intent(inout) :: field_dir(3)
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
 integer                      :: ik,icfft,ivfft,ic,iv, ik_grid, iq, ikpq
 integer                      :: i_spin, err, ifail, ir, num_shifted_grid
 complex(SP)                  :: rho(2)
 real(SP)                     :: Ev_m_Ec
 type(pp_indexes)             :: px
 !
 !ioDB1 shifted grids
 !
 type(levels)                 :: E_shift
 type(bz_samp)                :: Xk_shift
 !
 !ioWF local copies           
 !
 complex(SP), allocatable     :: wf_local(:,:)
 integer    , allocatable     :: wf_state_local(:,:,:)
 !
 !I/O and external functions
 !
 integer                      :: io_err
 integer, external            :: ioDB1_reload
#if defined _DOUBLE
 complex(SP)                  :: zdotc
#else 
 complex(SP)                  :: cdotc
#endif
 !
 use_trans_gauge=.true.
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
 io_err=ioDB1_reload(E_shift,Xk_shift,trim(grid_path))
 call set_parms(grid='shifted', command=' define')
 !
 if(io_err/=0) then
   call warning('Reading of shifted DB1 failed. Switch to transverse.')
   call DB1_reset()
   return
 endif
 !
 ! Determine grid displacement
 !
 ifail = 0
 call Dipole_check_shifted_grids( Xk_shift, Xk, ifail, num_shifted_grid)
 if(ifail>0) then
   call warning('Error in new grids. Switch to transverse.')
   call DB1_reset()
   return
 endif
 !
 wf_ng_eff = min(wf_ng_save,wf_ng_shifted)
 !
#if defined _MANYK
 !
 ! {k} Allocate k-dependent local copies of unshifted WF grid. 
 !
 allocate(wf_state_local( X%ib(2), nkibz_save, n_spin ), stat=err )
 allocate(wf_local( wf_ng_eff, (X%ib(2)-X%ib(1)+1)*1*n_spin ), stat=err ) 

#else
 !
 ! {k} Save local copy of unshifted WF grid. 
 !
 allocate(wf_local( size(wf,1), size(wf,2) ), stat=err )
 call mem_est('WFshifted0',(/ size(wf,1), size(wf,2) /), errors=(/err/))
 allocate(wf_state_local( size(wf_state,1), size(wf_state,2), size(wf_state,3) ), stat=err )
 call mem_est('WFstate0',(/ size(wf,1), size(wf,2) /), errors=(/err/))
 wf_local(:,:)         = wf(:,:)
 wf_state_local(:,:,:) = wf_state(:,:,:)
 !
 ! Load WFs {k} + q_iq
 !
 core_io_path = grid_path
 call WF_free()
 call WF_load(0,1,(/ X%ib(1), Xen%nbm /),&
&                 (/1,num_shifted_grid*Xk%nibz/),&
&                 space='G',title='-Oscillators/G space (shifted grid)') 
#endif
 !
 ! Main loop over cartesian directions
 !
 do iq = 1, num_shifted_grid
   ik_grid = (iq-1)*Xk%nibz + 1 ! The index of the first k point for each grid
   ir = iq
   !
   ! Set up the parallel environment 
   !
   call pp_indexes_reset(px)
   call par_indexes(px,(/Xk%nibz,Xen%nbm/),(/1,X%ib(1)/))
   call live_timing('Dipole @ grid '//trim(intc(iq))//':',px%n_of_elements(myid+1))
   call pp_redux_wait
   !
   do ik = 1, Xk%nibz   ! Loop over k's in IBZ [1:nkibz]
     !
     ikpq = ik_grid + ik - 1 ! Corresponding k in set of shifted grids
     !
#if defined _MANYK
     !
     ! Load WFs {k} 
     !
     call set_parms(grid='  local',command='  reset')
     call WF_free()
     call WF_load(wf_ng_eff,1,X%ib,(/ik,ik/),space='G',title='-Oscillators/G space')
     wf_local(:,:)         = wf(:,:)
     wf_state_local(:,1:ik,:) = wf_state(:,:,:)
     !
     ! Load WFs {k} + q_iq
     !
     call set_parms(grid=' shifted',command='  reset')
     call WF_free()
     call WF_load(wf_ng_eff,1,(/X%ib(1), Xen%nbm /),&
&                   (/ikpq,ikpq/),space='G',title='-Oscillators/G space')
#endif
     !
     do iv = X%ib(1),Xen%nbm
       if (.not.px%element_2D(ik,iv)) cycle
       do ic = Xen%nbf+1, X%ib(2)
         do i_spin = 1,n_spin
           !
           rho(i_spin)=(0.,0.)
           !
           ivfft   = wf_state(iv,ikpq,i_spin) 
           !
           ! No need to shift Efermi
           !
           if (n_sp_pol==2) then
             Ev_m_Ec=Xen%E(iv,ik,i_spin)-Xen%E(ic,ik,i_spin)
             if (associated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,i_spin)-Xen%Eo(ic,ik,i_spin)
           else
             Ev_m_Ec=Xen%E(iv,ik,1)-Xen%E(ic,ik,1)
             if (associated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,1)-Xen%Eo(ic,ik,1)
           endif
           !
           if (any( (/ -Ev_m_Ec < X%ehe(1) .and. X%ehe(1) > 0.0_SP, &
&                      -Ev_m_Ec > X%ehe(2) .and. X%ehe(2) > 0.0_SP /))) cycle
           if (abs(Ev_m_Ec)<=1.E-5_SP) cycle
           icfft = wf_state_local(ic,ik,i_spin) 
           !
           ! DIP_iR_or_P = < v,k+q | c,k >/q
           !
#if defined _DOUBLE
           rho(i_spin) = zdotc( wf_ng_eff, wf(1:wf_ng_eff,ivfft), &
&                               1, wf_local(1:wf_ng_eff,icfft),1 ) /q0_shift_norm
#else
           rho(i_spin) = cdotc( wf_ng_eff, wf(1:wf_ng_eff,ivfft), &
&                               1, wf_local(1:wf_ng_eff,icfft),1 ) /q0_shift_norm
#endif
           !
         enddo    ! spin loop
         !
         if (n_sp_pol==2) then
           DIP_iR(ir,ic,iv,ik,1)=rho(1)
           DIP_iR(ir,ic,iv,ik,2)=rho(2)
         else if (n_spinor==2) then
           DIP_iR(ir,ic,iv,ik,1)=(rho(1)+rho(2))
         else if (n_spin==1) then
           DIP_iR(ir,ic,iv,ik,1)=rho(1)
         endif
         !
       enddo  ! conduction band loop
       !
       call live_timing(steps=1)
     enddo      ! valence band loop
   enddo        ! k-points
   call live_timing
 enddo          ! iq
 !
 ! CLEAN
 !
 call WF_free()
 do i_spin=1,n_spin
   call pp_redux_wait(DIP_iR(:,:,:,:,i_spin))
 enddo
 !
 ! Switch off transverse calculation if successful 
 !
 call DB1_reset()
 use_trans_gauge = .false.
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
     io_err=ioDB1_reload(E_shift,Xk_shift,trim(core_io_path))
     wf_ng=wf_ng_shifted
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
       core_io_path = grid_path
       nkibz   = nkibz_shifted
       wf_ncx  = wf_ncx_shifted
       wf_ng   = wf_ng_shifted
     endif
     return
   end subroutine set_parms
   !
end subroutine Dipole_shifted_grids
