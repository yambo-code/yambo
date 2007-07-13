!
! Copyright (C) 2000-2005 C. Hogan and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine X_O_longitudinal_cut(Xen, Xk, X, field_dir, use_trans_gauge)
!====================================
   use pars
   use com,                     ONLY : msg, in_path, msg, error, warning
   use stderr,                  ONLY : intc
   use timing,                  ONLY : live_timing
   use electrons,               ONLY : levels, n_spin, n_spinor, E_reset, E_duplicate, n_sp_pol
   use par_proc_m,              ONLY : pp_redux_wait, pp_indexes, myid, &
&                                    master_node, pp_indexes_reset
   use par_indexes_m,           ONLY : par_indexes
   use R_lattice,               ONLY : g_vec, bz_samp, bz_samp_duplicate,bz_samp_reset, q0_def_norm
   use D_lattice,               ONLY : DL_sop, n_atoms, atom_pos, znucl
   use X_m,                     ONLY : X_alloc, X_t, long_path
   use IO_m,                    ONLY : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP, DUMP, ver_is_gt_or_eq
   use memory_m,                ONLY : mem_est
   use wave_func,               ONLY : wf, wf_ng, wf_state, wf_load, WF_free, wf_nc_k, wf_igk
   use optcut,                  ONLY : Xcut_q0grad, ng_limits, setup_optcut, pscut
   use surface_geometry,        ONLY : setup_gvecaff
   use ras_long,                ONLY : select_field_dir, grid_dir
   implicit none
   type(bz_samp),  intent(in)       :: Xk
   type(levels),   intent(in)       :: Xen
   type(X_t)                        :: X
   real(SP),          intent(inout) :: field_dir(3)
   logical,        intent(inout)  :: use_trans_gauge
!ws
     integer                      :: wf_ng_long
  integer                      :: ikpq, ik_grid, ir, num_long_grid
  logical                      :: read_zero_grid
  complex(SP)                  :: xy1,xy2
   integer                          :: ik, i1, icfft, ivfft, ic, iv, is, i_spin, ifail, iq, err, io_err
   type(pp_indexes)                 :: px
   complex(SP)                      :: PS(3), rho(n_spin,3)
   real(SP), parameter          :: fac = 0.70710678118654752440_SP, zero = 1.0E-5
   real(SP)          :: Ev_m_Ec
   character(schlen)                :: sch ! DEBUG
!functions
! ioDB1 local copies
  type(levels)                 :: Xen_save
  type(bz_samp)                :: Xk_save
  real(SP), allocatable        :: f_save(:,:,:)
  character(lchlen)            :: in_path_save
  integer                      :: wf_ng_save, io_ID, ID
! ioDB1 shifted grids
  type(levels)                 :: E_shift
  type(bz_samp)                :: Xk_shift
! ioWF local copies           
  complex(SP), allocatable     :: wf_local(:,:)
  integer    , allocatable     :: wf_state_local(:,:,:)
  integer, external            :: ioDB1

   call section('+',' Longitudinal (cut) gauge calculation')

   ifail = 0
   use_trans_gauge = .true.
   ! 
   ! Save local copy of ioDB1 (Enk, wf_k)
   !
   ! The following lines are needed to reload DB1 correctly
   !
   in_path_save = in_path
   wf_ng_save = wf_ng
   call E_reset(Xen_save)
   ! Note (1) : E_reset resets Efermi: not read from DB1
   ! Note (2) : Memory estimate is wrong!
   call E_duplicate(Xen,Xen_save)
   call bz_samp_duplicate(Xk, Xk_save) 
   !
   ! Read longitudinal grids from second ioDB1
   !
   deallocate(g_vec,dl_sop,wf_nc_k,wf_igk)
   call mem_est("g_vec dl_sop wf_nc_k wf_igk")
   in_path = long_path
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=REP,MODE=DUMP,ID=io_ID)
   if (ver_is_gt_or_eq(io_ID,(/3,0,4/))) then
     deallocate(n_atoms,znucl,atom_pos)
     call mem_est("n_atoms atom_pos znucl")
   endif
   io_err = ioDB1( E_shift, Xk_shift, io_ID)  ! E_shift is not   used!
   wf_ng = wf_ng_save ! beca  use wf_ng is overwritten in init.F

   if(io_err.ne.0) then
     call msg('r','Reading of shifted DB1 failed.')
     call reset_DB1
     return
   endif
   !
   ! - Determine grid displacement vectors
   ! - Check compatibility of requested polarization vector
   !
   call check_shifted_grids( Xk_shift, Xk, ifail, num_long_grid)
   call select_field_dir(ifail, field_dir, num_long_grid)

!  call check_shifted_grids( Xk_shift, Xk_save, ifail, num_long_grid)
!  call select_field_dir(ifail, field_dir)
   if(ifail>0) then
     call msg('r','Error in new grids.')
     call reset_DB1
     return
   endif
   !
   ! {k} Save local copy of unshifted WF grid. 
   ! Note: Theres no need to save the occupied states! Can I use a pointer?
   !
   allocate(wf_local( size(wf,1), size(wf,2) ), stat=err )
   call mem_est('WFlong0',(/ size(wf,1), size(wf,2) /), errors=(/err/))
   allocate(wf_state_local( size(wf_state,1), size(wf_state,2), size(wf_state,3) ), stat=err )
   call mem_est('WFstate0',(/ size(wf,1), size(wf,2) /), errors=(/err/))
   wf_local(:,:)         = wf(:,:)
   wf_state_local(:,:,:) = wf_state(:,:,:)
   !
   ! Main loop over cartesian directions
   !
   do iq = 1,num_long_grid
     ik_grid = (iq-1)*Xk_save%nibz + 1 ! The index of the first k point for each grid
     ir = grid_dir(iq)
     !
     ! Load WFs {k} + q_iq
     !
     in_path = long_path
     call WF_free()
     call wf_load(0,1, (/ X%ib(1), Xen_save%nbm /),(/ik_grid,ik_grid+Xk_save%nibz-1/),space='G',title='-Oscillators/G space')
     wf_ng_long = min(wf_ng_save,wf_ng)
     !
     ! Set up the parallel environment 
     !
     call pp_indexes_reset(px)
     call par_indexes(px,(/Xk_save%nibz,Xen_save%nbm/),(/1,X%ib(1)/))
     call live_timing('Optical strengths (L/cut/'//trim(intc(iq))//'):',px%stps(myid+1))
     call pp_redux_wait

     do ik = 1, Xk_save%nibz   ! Loop over k's in IBZ [1:nkibz]
!--> DEBUG
       write(sch,'(a,i4,a,i4)') 'Loop over k oscillators: ',ik,'/',Xk_save%nibz
       call msg('ns',trim(sch))
       write(*,*) trim(sch)
!<-- DEBUG
       ikpq = ik_grid + ik - 1 ! Corresponding k in set of shifted grids
       call setup_optcut( ik )
       do iv = X%ib(1),Xen_save%nbm
         if (.not.px%i2p(ik,iv)) cycle

         do ic = Xen_save%nbf+1, X%ib(2)
           do i_spin = 1,n_spin

             ivfft   = wf_state(iv,ikpq,i_spin)

             if (n_sp_pol==2) then
               Ev_m_Ec=Xen_save%E(iv,ik,i_spin)-Xen_save%E(ic,ik,i_spin)
               if (associated(Xen_save%Eo)) Ev_m_Ec=Xen_save%Eo(iv,ik,i_spin)-Xen_save%Eo(ic,ik,i_spin)
             else 
               Ev_m_Ec=Xen_save%E(iv,ik,1)-Xen_save%E(ic,ik,1)
               if (associated(Xen_save%Eo)) Ev_m_Ec=Xen_save%Eo(iv,ik,1)-Xen_save%Eo(ic,ik,1)
             endif

             if (any( (/ -Ev_m_Ec < X%ehe(1) .and. X%ehe(1) > 0.0_SP, &
                         -Ev_m_Ec > X%ehe(2) .and. X%ehe(2) > 0.0_SP /))) cycle
             if (abs(Ev_m_Ec)<=1.E-5_SP) cycle
             icfft = wf_state_local(ic,ik,i_spin)
             !
             ! X_q0grad = < v,k+q | T(z) e^i(q.r)| c,k >/q
             !
             call PScut(PS, wf(1:wf_ng_long,ivfft), wf_local(1:wf_ng_long,icfft))
             rho(i_spin,:) = PS(:)/q0_def_norm
           enddo   ! spin loop
           if (n_sp_pol==2) then
             Xcut_q0grad(ir,ic,iv,ik,1)=rho(1,ir)
             Xcut_q0grad(ir,ic,iv,ik,2)=rho(2,ir)
           else if (n_spinor==2) then
             Xcut_q0grad(ir,ic,iv,ik,1)=(rho(1,ir)+rho(2,ir))
           else if (n_spin==1) then
             Xcut_q0grad(ir,ic,iv,ik,1)=rho(1,ir)
           endif

!          Xcut_q0grad(ir,ic,iv,ik,i_spin) = PS(ir)/q0mod
         enddo ! cond. bands
         !
         ! Correct for spinor case
         !
!        if(n_spinor.eq.2) then
!          do ic = Xen%nbf+1, X%ib(2)
!            spinor_avg(:) =  ( Xcut_q0grad(:,ic,iv,ik,1) + Xcut_q0grad(:,ic,iv,ik,2) )*fac
!            Xcut_q0grad(:,ic,iv,ik,1) = spinor_avg(:)
!            Xcut_q0grad(:,ic,iv,ik,2) = spinor_avg(:)
!          enddo
!        endif
         call live_timing(steps=1)
       enddo   ! valence bands
     enddo     ! k-points
     call live_timing
   enddo        ! num_long_grid
!  call end_optcut
   !
   ! Switch off transverse calculation if successful
   !
   use_trans_gauge = .false.
   !
   ! Reset DB1
   !
   call reset_DB1
   return

 contains

  subroutine reset_DB1
    in_path = in_path_save
    call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=REP,MODE=DUMP,ID=ID)
    if(allocated(g_vec))  deallocate(g_vec)
    if(allocated(dl_sop)) deallocate(dl_sop)
    if(allocated(wf_nc_k))  deallocate(wf_nc_k)
    if(allocated(wf_igk)) deallocate(wf_igk)
!   if (ver_is_gt_or_eq(io_ID,(/3,0,4/))) then
    if (ver_is_gt_or_eq(ID,(/3,0,4/))) then
      if(allocated(n_atoms))   deallocate(n_atoms)
      if(allocated(znucl))     deallocate(znucl)
      if(allocated(atom_pos)) deallocate(atom_pos)
    endif
    io_err=ioDB1(Xen,Xk,ID)
    if(io_err.ne.0) call error("Re-reading of ioDB1 failed!")
    Xen%f(:,:,:) = Xen_save%f(:,:,:)
    wf_ng = wf_ng_save
    return
  end subroutine reset_DB1

 end subroutine X_O_longitudinal_cut
