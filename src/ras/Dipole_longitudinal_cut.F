!
! Copyright (C) 2000-2010 C. Hogan and the YAMBO team 
!              http://www.yambo-code.org
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be   useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
 subroutine Dipole_shifted_grids_cut(Xen, Xk, X, field_dir, use_trans_gauge)
!====================================
   use pars
   use com,                     ONLY : msg, core_io_path, msg, error, warning
   use stderr,                  ONLY : intc
   use timing,                  ONLY : live_timing
   use electrons,               ONLY : levels, n_spin, n_spinor, E_reset, E_duplicate, n_sp_pol
   use par_proc_m,              ONLY : pp_redux_wait, pp_indexes, myid, &
&                                    master_cpu, pp_indexes_reset
   use par_indexes_m,           ONLY : par_indexes
   use R_lattice,               ONLY : g_vec, bz_samp, bz_samp_duplicate,bz_samp_reset, q0_def_norm, nkibz
   use D_lattice,               ONLY : DL_sop, n_atoms_species, atom_pos, Z_species
   use X_m,                     ONLY : X_alloc, X_t, grid_path
   use IO_m,                    ONLY : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP, DUMP, ver_is_gt_or_eq
   use memory_m,                ONLY : mem_est
   use wave_func,               ONLY : wf, wf_ng, wf_state, WF_load, WF_free, wf_nc_k, wf_igk, wf_ncx
   use optcut,                  ONLY : Xcut_q0grad, ng_limits, setup_optcut, pscut
   use surface_geometry,        ONLY : setup_gvecaff
   use ras_long,                ONLY : select_field_dir, grid_dir
   implicit none
   type(bz_samp),  intent(in)       :: Xk
   type(levels),   intent(in)       :: Xen
   type(X_t)                        :: X
   real(SP),          intent(inout) :: field_dir(3)
   integer                      :: nkibz_save, wf_ncx_save, wf_ncx_long, nkibz_long
   logical,        intent(inout)  :: use_trans_gauge
!  Work Space
     integer                      :: wf_ng_long, E_i_spin
  integer                      :: ikpq, ik_grid, ir, num_long_grid
  logical                      :: read_zero_grid
  complex(SP)                  :: xy1,xy2
   integer                          :: ik, i1, icfft, ivfft, ic, iv, is, i_spin, ifail, iq, err, io_err
   type(pp_indexes)                 :: px
   complex(SP)                      :: PS(3), rho(3,n_spin)
   real(SP), parameter          :: fac = 0.70710678118654752440_SP, zero = 1.0E-5
   real(SP)          :: Ev_m_Ec
   character(schlen)                :: sch 
!functions
! ioDB1 local copies
  type(levels)                 :: Xen_save
  type(bz_samp)                :: Xk_save
  real(SP), allocatable        :: f_save(:,:,:)
  character(lchlen)            :: core_io_path_save
  integer                      :: wf_ng_save, io_ID, ID
! ioDB1 shifted grids
  type(levels)                 :: E_shift
  type(bz_samp)                :: Xk_shift
! ioWF local copies           
  complex(SP), allocatable     :: wf_local(:,:)
  integer    , allocatable     :: wf_state_local(:,:,:)
  integer, external            :: ioDB1, ioDB1_reload

   call section('+',' Longitudinal (cut) gauge calculation')

   ifail = 0
   use_trans_gauge = .true.
   !
   ! Saves...
   !
   nkibz_save  = nkibz
   wf_ncx_save = wf_ncx
   wf_ng_save  = wf_ng
   core_io_path_save = core_io_path
   !
   if (wf_ng.lt.0.8_SP*maxval(wf_igk)) &
&   call warning('All/most WF G-vectors should be used in the long. gauge')

   !
   ! Read longitudinal grids from second ioDB1
   !
   io_err=ioDB1_reload(E_shift,Xk_shift,trim(grid_path))
   nkibz_long = nkibz
   wf_ncx_long = wf_ncx

   wf_ng = wf_ng_save

   if(io_err.ne.0) then
     call warning('Reading of shifted DB1 failed. Switch to transverse.')
     call DB1_reset()
     return
   endif
   !
   ! - Determine grid displacement vectors
   ! - Check compatibility of requested polarization vector
   !
   call Dipole_check_shifted_grids( Xk_shift, Xk, ifail, num_long_grid)
   call select_field_dir(ifail, field_dir, num_long_grid)

   if(ifail>0) then
     call msg('r','Error in new grids. Switch to transverse.')
     call DB1_reset()
     return
   endif

#if defined _MANYK
 !
 ! {k} Allocate k-dependent local copies of unshifted WF grid.
 !
 wf_ng_long = min(wf_ng_save,wf_ng)
 allocate(wf_state_local( X%ib(2), nkibz_save, n_spin ), stat=err )
 allocate(wf_local( wf_ng_long, (X%ib(2)-X%ib(1)+1)*1*n_spin ), stat=err ) ! care icfft

#else
 !
 ! {k} Save local copy of unshifted WF grid.
 !
 allocate(wf_local( size(wf,1), size(wf,2) ), stat=err )
 call mem_est('WFlong0',(/ size(wf,1), size(wf,2) /), errors=(/err/))
 allocate(wf_state_local( size(wf_state,1), size(wf_state,2), size(wf_state,3) ), stat=err )
 call mem_est('WFstate0',(/ size(wf,1), size(wf,2) /), errors=(/err/))
 wf_local(:,:)         = wf(:,:)
 wf_state_local(:,:,:) = wf_state(:,:,:)
#endif

   !
   ! Main loop over cartesian directions
   !
   do iq = 1,num_long_grid
     ik_grid = (iq-1)*Xk%nibz + 1 ! The index of the first k point for each grid
     ir = grid_dir(iq)

#if !defined _MANYK
   !
   ! Load WFs {k} + q_iq
   !
   core_io_path = grid_path
   call WF_free()
   call WF_load(0,1,(/ X%ib(1), Xen%nbm /),&
&                   (/ik_grid,ik_grid+Xk%nibz-1/),&
&                   space='G',title='-Oscillators/G space')
   wf_ng_long = min(wf_ng_save,wf_ng)
#endif
     !
     ! Set up the parallel environment 
     !
     call pp_indexes_reset(px)
     call par_indexes(px,(/Xk%nibz,Xen%nbm/),(/1,X%ib(1)/))
     call live_timing('Dipole (L/cut/'//trim(intc(iq))//'):',px%n_of_element_1D(myid+1))
     call pp_redux_wait

     do ik = 1, Xk%nibz   ! Loop over k's in IBZ [1:nkibz]
       ikpq = ik_grid + ik - 1 ! Corresponding k in set of shifted grids

#if defined _MANYK
     !
     ! Load WFs {k}
     !
     core_io_path = core_io_path_save
     nkibz = nkibz_save
     wf_ncx = wf_ncx_save
     wf_ng = wf_ng_save
     call WF_free()
     call WF_load(wf_ng_long,1,X%ib,(/ik,ik/),space='G',title='-Oscillators/G space')
     wf_local(:,:)         = wf(:,:)
     wf_state_local(:,1:ik,:) = wf_state(:,:,:)
     !
     ! Load WFs {k} + q_iq
     !
     core_io_path = grid_path
     nkibz = nkibz_long
     wf_ncx = wf_ncx_long
     wf_ng = wf_ng_save
     call WF_free()
     call WF_load(wf_ng_long,1,(/X%ib(1), Xen%nbm /),&
&                   (/ikpq,ikpq/),space='G',title='-Oscillators/G space')
#endif

       write(sch,'(a,i4,a,i4)') 'Loop over k oscillators: ',ik,'/',Xk%nibz
       call msg('ns',trim(sch))
       write(*,*) trim(sch)
       call setup_optcut( ik )
       do iv = X%ib(1),Xen%nbm
         if (.not.px%element_2D(ik,iv)) cycle

         do ic = Xen%nbf+1, X%ib(2)
           do i_spin = 1,n_spin
             !
             rho(:,i_spin)=(0.,0.)
             !
             E_i_spin=i_spin
             if (n_sp_pol/=2) E_i_spin=1
             !
             Ev_m_Ec=Xen%E(iv,ik,E_i_spin)-Xen%E(ic,ik,E_i_spin)
             if (associated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,E_i_spin)-Xen%Eo(ic,ik,E_i_spin)

             if (any( (/ -Ev_m_Ec < X%ehe(1) .and. X%ehe(1) > 0.0_SP, &
                         -Ev_m_Ec > X%ehe(2) .and. X%ehe(2) > 0.0_SP /))) cycle
             if (abs(Ev_m_Ec)<=1.E-5_SP) cycle

             ivfft   = wf_state(iv,ikpq,i_spin)
             icfft = wf_state_local(ic,ik,i_spin)
             !
             ! DIP_iR_or_P = < v,k+q | T(z) e^i(q.r)| c,k >/q
             !
             call PScut(PS, wf(1:wf_ng_long,ivfft), wf_local(1:wf_ng_long,icfft))
             rho(:,i_spin) = PS(:)/q0_def_norm
           enddo   ! spin loop
           if (n_sp_pol==2) then
             Xcut_q0grad(ir,ic,iv,ik,1)=rho(ir,1)
             Xcut_q0grad(ir,ic,iv,ik,2)=rho(ir,2)
           else if (n_spinor==2) then
             Xcut_q0grad(ir,ic,iv,ik,1)=(rho(ir,1)+rho(ir,2))
           else if (n_spin==1) then
             Xcut_q0grad(ir,ic,iv,ik,1)=rho(ir,1)
           endif

         enddo ! cond. bands
         call live_timing(steps=1)
       enddo   ! valence bands
     enddo     ! k-points
     call live_timing
   enddo        ! num_long_grid
   !
   ! CLEAN
   !
   call WF_free()
   do i_spin=1,n_spin
     call pp_redux_wait(Xcut_q0grad(:,:,:,:,i_spin))
   enddo
   !
   ! Switch off transverse calculation if successful
   !
   use_trans_gauge = .false.
   !
   ! Reset DB1
   !
   call DB1_reset
   return

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
   wf_ng=wf_ng_long
   !
 end subroutine

  subroutine reset_DB1
    core_io_path = core_io_path_save
    call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=REP,MODE=DUMP,ID=ID)
    if(allocated(g_vec))  deallocate(g_vec)
    if(allocated(dl_sop)) deallocate(dl_sop)
    if(allocated(wf_nc_k))  deallocate(wf_nc_k)
    if(allocated(wf_igk)) deallocate(wf_igk)
!   if (ver_is_gt_or_eq(io_ID,(/3,0,4/))) then
    if (ver_is_gt_or_eq(ID,(/3,0,4/))) then
      if(allocated(n_atoms_species))   deallocate(n_atoms_species)
      if(allocated(Z_species))     deallocate(Z_species)
      if(allocated(atom_pos)) deallocate(atom_pos)
    endif
    io_err=ioDB1(Xen,Xk,ID)
    if(io_err.ne.0) call error("Re-reading of ioDB1 failed!")
    Xen%f(:,:,:) = Xen_save%f(:,:,:)
    wf_ng = wf_ng_save
    return
  end subroutine reset_DB1

 end subroutine Dipole_shifted_grids_cut
