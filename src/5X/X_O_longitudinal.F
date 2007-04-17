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
subroutine X_O_longitudinal(Xen,Xk,X,field_dir,use_trans_gauge)
 !
 ! In the longitudinal gauge we can use the wfc 
 ! at the k,k-q points to construct
 !
 !  i q.<nk|r|mk> =  \sum_i=1,3 q(i) <u_n(k+q0(i))|u_mk>/q0(i)
 !
 use pars,                only : SP,pi, lchlen
 use com,                 only : warning, inpath, msg, error
 use timing,              only : live_timing
 use electrons,           only : levels,n_spin, E_duplicate, E_reset, n_spinor,n_sp_pol
 use par_proc_m,          only : pp_redux_wait,pp_indexes,myid,master_node,pp_indexes_reset
 use par_indexes_m,       only : par_indexes
 use D_lattice,           only : DL_sop, n_atoms,znucl,atom_pos
 use R_lattice,           only : g_vec,bz_samp, bz_samp_duplicate, q0_defmod, bz_samp_reset
 use X_m,                 only : X_alloc,X_t,X_q0grad, q0mod, long_path, num_long_grid
 use IO_m,                only : io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP,OP_RD,&
&                                RD_CL,RD,DUMP,NONE, ver_is_gt_or_eq
 use memory_m,            only : mem_est
 use wave_func,           only : wf,wf_ng,wf_state,wf_load,WF_free, wf_igk, wf_nc_k
#if defined PJ_RAS
 use ras_long,            only : grid_dir, select_field_dir
#endif
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 logical,       intent(inout) :: use_trans_gauge
 real(SP),      intent(inout) :: field_dir(3)
 !
 ! Work Space
 !
 integer                      :: ik,icfft,ivfft,ic,iv, ik_grid, iq, ikpq, &
&                                i_spin, err, ifail, wf_ng_long, ir
 complex(SP), allocatable     :: wf_k(:)
 complex(SP)                  :: rho(2)
 real(SP)                     :: Ev_m_Ec
 real(SP), parameter          :: fac = 0.70710678118654752440_SP
 type(pp_indexes)             :: px
 logical                      :: lfail
 !
 !ioDB1 local copies
 !
 type(levels)                 :: Xen_save
 type(bz_samp)                :: Xk_save
 real(SP), allocatable        :: f_save(:,:,:)
 character(lchlen)            :: inpath_save
 integer                      :: wf_ng_save
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
 integer                      :: ID,io_err,io_ID
 integer, external            :: ioDB1
#if defined _DOUBLE
 complex(SP)                  :: zdotc
#else 
 complex(SP)                  :: cdotc
#endif
 !
 call section('+',' Longitudinal gauge calculation')
 ifail = 0
 use_trans_gauge=.true.
 !
 ! Save local copy of ioDB1 (Enk, wf_k)
 !
 inpath_save = inpath
 wf_ng_save = wf_ng
 call E_reset(Xen_save) 
 call E_duplicate(Xen,Xen_save)   
 call bz_samp_duplicate(Xk, Xk_save) 
 !
 ! Read longitudinal k-point grids from second ioDB1
 !
 deallocate(g_vec,dl_sop,wf_nc_k,wf_igk)
 call mem_est("g_vec dl_sop wf_nc_k wf_igk")
 inpath = long_path
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=REP,MODE=DUMP,ID=io_ID)
 if (ver_is_gt_or_eq(io_ID,(/3,0,4/))) then
   deallocate(n_atoms,znucl,atom_pos)
   call mem_est("n_atoms atom_pos znucl")
 endif
 io_err = ioDB1( E_shift, Xk_shift, io_ID)  ! E_shift is not used!
 wf_ng = wf_ng_save ! because wf_ng is overwritten in init.F

 if(io_err.ne.0) then
   call msg('r','Reading of shifted DB1 failed.')
   call reset_DB1
   return
 endif
 !
 ! Determine grid displacement (q0mod)
 !
 call check_shifted_grids( Xk_shift, Xk_save, ifail )
#if defined PJ_RAS
 call select_field_dir(ifail, field_dir)
#endif
 if(ifail>0) then
   call msg('r','Error in new grids.')
   call reset_DB1
   return
 endif
 !
 ! {k} Save local copy of unshifted WF grid. 
 !
 allocate(wf_local( size(wf,1), size(wf,2) ), stat=err )
 call mem_est('WFlong0',(/ size(wf,1), size(wf,2) /), errors=(/err/))
 allocate(wf_state_local( size(wf_state,1), size(wf_state,2), size(wf_state,3) ), stat=err )
 call mem_est('WFstate0',(/ size(wf,1), size(wf,2) /), errors=(/err/))
 wf_local(:,:)         = wf(:,:)
 wf_state_local(:,:,:) = wf_state(:,:,:)

 allocate(wf_k(wf_ng))
 !
 ! Main loop over cartesian directions
 !
 do iq = 1, num_long_grid
   ik_grid = (iq-1)*Xk_save%nibz + 1 ! The index of the first k point for each grid
   ir = iq
#if defined PJ_RAS
   ir = grid_dir(iq)
#endif
   !
   ! Load WFs {k} + q_iq
   !
   inpath = long_path
   call WF_free()
   call wf_load(0,1,(/ X%ib(1), Xen_save%nbm /),(/ik_grid,ik_grid+Xk_save%nibz-1/),space='G',title='-Oscillators/G space') 
   wf_ng_long = min(wf_ng_save,wf_ng)
   !
   ! Set up the parallel environment 
   !
   call pp_indexes_reset(px)
   call par_indexes(px,(/Xk_save%nibz,Xen_save%nbm/),(/1,X%ib(1)/))
   call live_timing('Optical strengths (L):',px%stps(myid+1))
   call pp_redux_wait

   do ik = 1, Xk_save%nibz   ! Loop over k's in IBZ [1:nkibz]
     ikpq = ik_grid + ik - 1 ! Corresponding k in set of shifted grids
     do iv = X%ib(1),Xen_save%nbm
       if (.not.px%i2p(ik,iv)) cycle

       do ic = Xen_save%nbf+1, X%ib(2)
         do i_spin = 1,n_spin

           ivfft   = wf_state(iv,ikpq,i_spin) 
           !
           ! No need to shift Efermi
           !
           if (n_sp_pol==2) then
             Ev_m_Ec=Xen_save%E(iv,ik,i_spin)-Xen_save%E(ic,ik,i_spin)
             if (associated(Xen_save%Eo)) Ev_m_Ec=Xen_save%Eo(iv,ik,i_spin)-Xen_save%Eo(ic,ik,i_spin)
           else
             Ev_m_Ec=Xen_save%E(iv,ik,1)-Xen_save%E(ic,ik,1)
             if (associated(Xen_save%Eo)) Ev_m_Ec=Xen_save%Eo(iv,ik,1)-Xen_save%Eo(ic,ik,1)
           endif

           if (any( (/ -Ev_m_Ec < X%ehe(1) .and. X%ehe(1) > 0.0_SP, &
&                      -Ev_m_Ec > X%ehe(2) .and. X%ehe(2) > 0.0_SP /))) cycle
           if (abs(Ev_m_Ec)<=1.E-5_SP) cycle
           icfft = wf_state_local(ic,ik,i_spin) 
           !
           ! X_q0grad = < v,k+q | c,k >/q
           !
#if defined _DOUBLE
           rho(i_spin) = zdotc( wf_ng_long, wf(1:wf_ng_long,ivfft), &
&                               1, wf_local(1:wf_ng_long,icfft),1 ) /q0mod
#else
           rho(i_spin) = cdotc( wf_ng_long, wf(1:wf_ng_long,ivfft), &
&                               1, wf_local(1:wf_ng_long,icfft),1 ) /q0mod
#endif
           !
         enddo    ! spin loop
         !
         if (n_sp_pol==2) then
           X_q0grad(ir,ic,iv,ik,1)=rho(1)
           X_q0grad(ir,ic,iv,ik,2)=rho(2)
         else if (n_spinor==2) then
           X_q0grad(ir,ic,iv,ik,1)=(rho(1)+rho(2))
         else if (n_spin==1) then
           X_q0grad(ir,ic,iv,ik,1)=rho(1)
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
 deallocate(wf_k)
 call WF_free()
 do i_spin=1,n_spin
   call pp_redux_wait(X_q0grad(:,:,:,:,i_spin))
 enddo
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
   inpath = inpath_save
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=REP,MODE=DUMP,ID=ID)
   if(allocated(g_vec))  deallocate(g_vec)
   if(allocated(dl_sop)) deallocate(dl_sop)
   if(allocated(wf_nc_k))  deallocate(wf_nc_k)
   if(allocated(wf_igk)) deallocate(wf_igk)
   if (ver_is_gt_or_eq(io_ID,(/3,0,4/))) then
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
end subroutine X_O_longitudinal

subroutine check_shifted_grids( Xk_shift, Xk_save, ifail)
  use pars,            only : SP
  use stderr,          only : intc
  use com,             only : msg
  use vec_operate,     only : v_is_zero, v_norm, c2a, normalize_v
  use X_m,             only : q_long, q0mod, num_long_grid ! Remove q0mod from here
  use R_lattice,       only : bz_samp
  implicit none
  !
  ! Find number of shifted grids
  !
  type(bz_samp), intent(in)                :: Xk_shift, Xk_save
  integer, intent(inout) :: ifail
  !
  real(SP)         :: q_init(3), q_mod(3), q0mod_temp, qtemp(3), ca(3,3)
  real(SP)                 :: a(3), b(3), c(3), x(3), y(3), z(3)
  integer          :: i_grid_k, iq, ik

  if(ifail>0) return

  if(mod(Xk_shift%nibz,Xk_save%nibz)/=0) then
    call msg('r','Bad number of k-points in shifted DBs:'//intc(Xk_save%nibz)//intc(Xk_shift%nibz))
    ifail = ifail + 1
    return
  endif

  num_long_grid = Xk_shift%nibz / Xk_save%nibz
  call msg('nr','Longitudinal grids found: '//intc(num_long_grid))

#if defined PJ_RAS
  if(.not.(num_long_grid.ge.1.and.num_long_grid.le.4)) then
    call msg('r','Bad number of shifted grids (1/2/3).')
    ifail = ifail + 1
    return
  endif
#else
  if(num_long_grid.ne.3) then
    call msg('r','Must have 3 shifted grids!')
    ifail = ifail + 1
    return
  endif
#endif
  !
  ! Find the displacement vector for each grid
  !
  ! q_long(:,i)   is the shift vector of grid(i) in [iku]
  ! q_mod(i) is the modulus of the grid shift vector (i) in [a.u.]
  !
  q_mod(1:3) = 0.0_SP
  do iq = 1, num_long_grid
    i_grid_k = (iq-1)*Xk_save%nibz
    q_init(:) = Xk_shift%pt(i_grid_k + 1,:) - Xk_save%pt(1,:) ! units of [iku]
    !
    ! Check grid is uniformly shifted
    !
    do ik = 2, Xk_save%nibz
      if(.not.v_is_zero( Xk_shift%pt(i_grid_k+ik,:) - &
&                         Xk_save%pt(ik,:) - q_init(:) )) then
        call msg('r','Long. gauge shifts are not uniform for all {k}.')
        ifail = ifail + 1
        return
      endif
    enddo
    q_long(:,iq) = q_init(:)
    call c2a(v_in=q_long(:,iq), v_out=qtemp, mode="ki2c")
    call msg('r','Grid '//trim(intc(iq))//' has shift vector [c.c.]: ',qtemp )
    q_mod(iq) = v_norm(qtemp)
    call msg('r','            and modulus [a.u.]: ',q_mod(iq))
  enddo
  !
  ! Check the lengths are compatible (not abolutely required)
  !
  q0mod = q_mod(1)  ! a.u.
  q_mod(1:num_long_grid) = q_mod(1:num_long_grid) - q0mod
  if(.not.all( abs(q_mod).lt. 1.0e-5_SP )) then
    call msg('r','Long. gauge shifts are not uniform for all {k}.')
    ifail = ifail + 1
  endif

  return
end subroutine check_shifted_grids

