!
! Copyright (C) 2000-2005 C. Hogan, A. Marini and the SELF team 
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
 use pars,                ONLY : SP,lchlen
 use com,                 ONLY : warning, in_path, msg, error
 use stderr,              ONLY : intc
 use timing,              ONLY : live_timing
 use electrons,           ONLY : levels,n_spin,  n_spinor,n_sp_pol
 use par_proc_m,          ONLY : pp_redux_wait,pp_indexes,myid,master_node,&
&                                pp_indexes_reset
 use par_indexes_m,       ONLY : par_indexes
 use R_lattice,           ONLY : bz_samp,q0_def_norm
 use X_m,                 ONLY : X_alloc,X_t,X_q0grad, long_path 
 use IO_m,                ONLY : io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP,OP_RD,&
&                                RD_CL,RD,DUMP,NONE
 use memory_m,            ONLY : mem_est
 use wave_func,           ONLY : wf,wf_ng,wf_igk,wf_state,wf_load,WF_free 
#if defined PJ_RAS
 use ras_long,            ONLY : grid_dir, select_field_dir
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
 integer           ::wf_ng_save
 character(lchlen) ::in_path_save
 !
 ! Work Space
 !
 integer                      :: ik,icfft,ivfft,ic,iv, ik_grid, iq, ikpq, &
&                                i_spin, err, ifail, wf_ng_long, ir,num_long_grid
 complex(SP), allocatable     :: wf_k(:)
 complex(SP)                  :: rho(2)
 real(SP)                     :: Ev_m_Ec
 real(SP), parameter          :: fac = 0.70710678118654752440_SP
 type(pp_indexes)             :: px
 logical                      :: lfail
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
 wf_ng_save=wf_ng
 in_path_save = in_path
 !
 if (wf_ng/=maxval(wf_igk)) &
&   call warning('All WF G-vectors should be used in the long. gauge')
 !
 ! Read longitudinal k-point grids from second ioDB1
 !
 io_err=ioDB1_reload(E_shift,Xk_shift,trim(long_path))
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
 call check_shifted_grids( Xk_shift, Xk, ifail ,num_long_grid)
#if defined PJ_RAS
 call select_field_dir(ifail, field_dir)
#endif
 if(ifail>0) then
   call warning('Error in new grids. Switch to transverse.')
   call DB1_reset()
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
 !
 allocate(wf_k(wf_ng))
 !
 ! Main loop over cartesian directions
 !
 do iq = 1, num_long_grid
   ik_grid = (iq-1)*Xk%nibz + 1 ! The index of the first k point for each grid
   ir = iq
#if defined PJ_RAS
   ir = grid_dir(iq)
#endif
   !
   ! Load WFs {k} + q_iq
   !
   in_path = long_path
   call WF_free()
   call wf_load(0,1,(/ X%ib(1), Xen%nbm /),&
&                   (/ik_grid,ik_grid+Xk%nibz-1/),&
&                   space='G',title='-Oscillators/G space') 
   wf_ng_long = min(wf_ng_save,wf_ng)
   !
   ! Set up the parallel environment 
   !
   call pp_indexes_reset(px)
   call par_indexes(px,(/Xk%nibz,Xen%nbm/),(/1,X%ib(1)/))
   call live_timing('Optical strengths @ grid '//trim(intc(iq))//':',px%stps(myid+1))
   call pp_redux_wait
   !
   do ik = 1, Xk%nibz   ! Loop over k's in IBZ [1:nkibz]
     ikpq = ik_grid + ik - 1 ! Corresponding k in set of shifted grids
     do iv = X%ib(1),Xen%nbm
       if (.not.px%i2p(ik,iv)) cycle
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
           ! X_q0grad = < v,k+q | c,k >/q
           !
#if defined _DOUBLE
           rho(i_spin) = zdotc( wf_ng_long, wf(1:wf_ng_long,ivfft), &
&                               1, wf_local(1:wf_ng_long,icfft),1 ) /q0_def_norm
#else
           rho(i_spin) = cdotc( wf_ng_long, wf(1:wf_ng_long,ivfft), &
&                               1, wf_local(1:wf_ng_long,icfft),1 ) /q0_def_norm
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
   in_path = in_path_save
   io_err=ioDB1_reload(E_shift,Xk_shift,trim(in_path))
   wf_ng=wf_ng_long
   !
 end subroutine
 !
end subroutine X_O_longitudinal

subroutine check_shifted_grids( Xk_shift, Xk, ifail,num_long_grid)
 !
 use pars,            ONLY : SP
 use stderr,          ONLY : intc
 use com,             ONLY : msg,warning
 use vec_operate,     ONLY : v_is_zero, v_norm, c2a, normalize_v
 use R_lattice,       ONLY : bz_samp,q0_def_norm
 implicit none
 !
 ! Find number of shifted grids
 !
 type(bz_samp), intent(in)    :: Xk_shift, Xk
 integer,       intent(inout) :: ifail,num_long_grid
 !
 real(SP)         :: q_init(3), q_mod(3), qtemp(3), ca(3,3)
 real(SP)         :: a(3), b(3), c(3), x(3), y(3), z(3), q_long(3,3)
 integer          :: i_grid_k, iq, ik
 !
 if(ifail>0) return
 !
 if(mod(Xk_shift%nibz,Xk%nibz)/=0) then
   call msg('r',&
&       'Bad number of k-points in shifted DBs:'//intc(Xk%nibz)//intc(Xk_shift%nibz))
   ifail = ifail + 1
   return
 endif
 !
 num_long_grid = Xk_shift%nibz / Xk%nibz
 call msg('r','Longitudinal grids found: '//intc(num_long_grid))
  !
#if defined PJ_RAS
 if(.not.(num_long_grid.ge.1.and.num_long_grid.le.4)) then
   call warning('Bad number of shifted grids (1/2/3).')
   ifail = ifail + 1
   return
 endif
#else
 if(num_long_grid.ne.3) then
   call warning('Must have 3 shifted grids!')
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
   !
   i_grid_k = (iq-1)*Xk%nibz
   q_init(:) = Xk_shift%pt(i_grid_k + 1,:) - Xk%pt(1,:) ! units of [iku]
   !
   ! Check grid is uniformly shifted
   !
   do ik = 2, Xk%nibz
     if(.not.v_is_zero( Xk_shift%pt(i_grid_k+ik,:) - &
&                        Xk%pt(ik,:) - q_init(:) )) then
       call warning('Long. gauge shifts are not uniform for all {k}.')
       ifail = ifail + 1
       return
     endif
   enddo
   q_long(:,iq) = q_init(:)
   call c2a(v_in=q_long(:,iq), v_out=qtemp, mode="ki2c")
   call msg('r','Grid '//trim(intc(iq))//' has shift vector [c.c.]: ',qtemp )
   q_mod(iq) = v_norm(qtemp)
   call msg('r','            and modulus [a.u.]: ',q_mod(iq))
   if (iq==num_long_grid) call msg('r','')
 enddo
 !
 ! Check the lengths are compatible (not abolutely required)
 !
 q0_def_norm = q_mod(1)  ! a.u.
 q_mod(1:num_long_grid) = q_mod(1:num_long_grid) - q0_def_norm
 if(.not.all( abs(q_mod).lt. 1.0e-5_SP )) then
   call msg('r','Long. gauge shifts are not uniform for all {k}.')
   ifail = ifail + 1
 endif
 !
 end subroutine check_shifted_grids
