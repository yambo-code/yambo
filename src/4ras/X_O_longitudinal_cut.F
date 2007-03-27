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
   use com,                     ONLY : msg, inpath, msg, error, warning
    use stderr,                 only : intc
   use timing,                  ONLY : live_timing
   use electrons,               ONLY : levels, n_spin, n_spinor, E_reset, E_duplicate
   use par_proc_m,              ONLY : pp_redux_wait, pp_indexes, myid, &
&                                    master_node, pp_indexes_reset
   use par_indexes_m,           ONLY : par_indexes
   use R_lattice,               ONLY : g_vec, bz_samp, bz_samp_duplicate,bz_samp_reset, q0_defmod
   use D_lattice,               ONLY : DL_sop, n_atoms, atom_pos, znucl
   use X_m,                     ONLY : X_alloc, X_t, long_path, q0mod, num_long_grid
   use IO_m,                    ONLY : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP, DUMP, ver_is_gt_or_eq
   use memory_m,                ONLY : mem_est
   use wave_func,               ONLY : wf, wf_ng, wf_state, wf_load, WF_free, wf_nc_k, wf_igk
   use optcut,                  ONLY : Xcut_q0grad, ng_limits, setup_optcut, pscut
   use surface_geometry,        ONLY : setup_gvecaff
   implicit none
   type(bz_samp),  intent(in)       :: Xk
   type(levels),   intent(in)       :: Xen
   type(X_t)                        :: X
   real(SP),          intent(inout) :: field_dir(3)
   logical,        intent(inout)  :: use_trans_gauge
!ws
     integer                      :: wf_ng_long
  integer                      :: grid_offset, ikpq, ik_grid
  logical                      :: read_zero_grid
  complex(SP)                  :: xy1,xy2
   integer                          :: ik, i1, icfft, ivfft, ic, iv, is, i_spin, ifail, iq, err, io_err
   type(pp_indexes)                 :: px
   complex(SP)                      :: PS(3), spinor_avg(3)
   real(SP), parameter          :: fac = 0.70710678118654752440_SP, zero = 1.0E-5
   real(SP)          :: Ev_m_Ec
   character(schlen)                :: sch ! DEBUG
!functions
! ioDB1 local copies
  type(levels)                 :: Xen_save
  type(bz_samp)                :: Xk_save
  real(SP), allocatable        :: f_save(:,:,:)
  character(lchlen)            :: inpath_save
  integer                      :: wf_ng_save, io_ID, ID
! ioDB1 shifted grids
  type(levels)                 :: E_shift
  type(bz_samp)                :: Xk_shift
! ioWF local copies           
  complex(SP), allocatable     :: wf_local(:,:)
  integer    , allocatable     :: wf_state_local(:,:,:)
  integer, external            :: ioDB1
#if defined _DOUBLE
  complex(SP)                  :: zdotc
#else 
  complex(SP)                  :: cdotc
#endif

   call section('+',' Longitudinal gauge calculation')

   ifail = 0
   use_trans_gauge = .true.
   ! 
   ! Save local copy of ioDB1 (Enk, wf_k)
   !
   ! The following lines are needed to reload DB1 correctly
   !
   inpath_save = inpath
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
   inpath = long_path
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
   ! - Set up new reference frame (presently disabled)
   ! - Check compatibility of requested polarization vector (disabled)
   !
   call long_grid_setup(ifail)
 ! call rotate_oscillator_frame(ifail)
 ! call select_field_dir(ifail)
   if(ifail>0) then
     call msg('r','Error in new grids.')
     call reset_DB1
     return
   endif
   !
   ! Read the unshifted WFs again if they are included.
   ! Note: this is inadvisable, since the unshifted WFs are already loaded 
   !       in X_O_strengths_cut
   !
   if(read_zero_grid) then
     inpath = long_path
     call WF_free()
     call wf_load(0,1,X%ib,(/1,Xk_save%nibz/),space='G',title='-Oscillators/G space')
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
  
!  call setup_gvecaff
!  call nG_limits( Xk%nibz )
!  do i1 = 1,3
!   kg(i1,:) = 1.0_SP
!  enddo
   !
   ! Main loop over cartesian directions
   !

! # Have to set iq to the CARTESIAN index for PS(iq), for less grids

   do iq = 1,num_long_grid
     ik_grid = (iq-1)*Xk_save%nibz + grid_offset + 1 ! The index of the first k point for each grid
     !
     ! Load WFs {k} + q_iq
     !
     inpath = long_path
     call WF_free()
     call wf_load(0,1,X%ib,(/ik_grid,ik_grid+Xk_save%nibz-1/),space='G',title='-Oscillators/G space')
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

         do i_spin = 1,n_spin

           ivfft   = wf_state(iv,ikpq,i_spin)

           do ic = Xen_save%nbf+1, X%ib(2)
             Ev_m_Ec = Xen_save%E(iv,ik,i_spin) - Xen_save%E(ic,ik,i_spin) ! No need to shift Efermi
             if (associated(Xen_save%Eo)) Ev_m_Ec = Xen_save%Eo(iv,ik,i_spin) - Xen_save%Eo(ic,ik,i_spin)
             if (any( (/ -Ev_m_Ec < X%ehe(1) .and. X%ehe(1) > 0.0_SP, &
                         -Ev_m_Ec > X%ehe(2) .and. X%ehe(2) > 0.0_SP /))) cycle
             if (abs(Ev_m_Ec)<=1.E-5_SP) cycle
             icfft = wf_state_local(ic,ik,i_spin)
             !
             ! X_q0grad = < v,k+q | T(z) e^i(q.r)| c,k >/q
             !
!            call PScut(kg, PS, wf(1:wf_ng_long,ivfft), wf_local(1:wf_ng_long,icfft), use_trans_gauge )
             call PScut(PS, wf(1:wf_ng_long,ivfft), wf_local(1:wf_ng_long,icfft))

             Xcut_q0grad(iq,ic,iv,ik,i_spin) = PS(iq)/q0mod
           enddo ! cond. bands
         enddo   ! spinors
         !
         ! Correct for spinor case
         !
         if(n_spinor.eq.2) then
           do ic = Xen%nbf+1, X%ib(2)
             spinor_avg(:) =  ( Xcut_q0grad(:,ic,iv,ik,1) + Xcut_q0grad(:,ic,iv,ik,2) )*fac
             Xcut_q0grad(:,ic,iv,ik,1) = spinor_avg(:)
             Xcut_q0grad(:,ic,iv,ik,2) = spinor_avg(:)
           enddo
         endif
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
    inpath = inpath_save
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

  subroutine long_grid_setup(ifail)
    use stderr,          only : intc
    use vec_operate,     only : v_is_zero, v_norm, c2a
    use X_m,             only : q_long, q0mod ! Remove q0mod from here
    implicit none
    !
    ! Find number of shifted grids
    !
    real(SP)         :: q_init(3), q_mod(3), q0mod_temp, qtemp(3)
    integer          :: i_grid_k
    integer, intent(inout) :: ifail

    if(ifail>0) return

    if(mod(Xk_shift%nibz,Xk_save%nibz)/=0) then
      call msg('r','Bad number of k-points in shifted DBs:'//intc(Xk_save%nibz)//intc(Xk_shift%nibz))
      ifail = ifail + 1
      return
    endif

    num_long_grid = Xk_shift%nibz / Xk_save%nibz
    if(.not.(num_long_grid.ge.1.and.num_long_grid.le.4)) then
      call msg('r','Bad number of shifted grids (1/2/3).')
      ifail = ifail + 1
      return
    endif
    !
    ! Simple test if original grid is present.
    !
    grid_offset = 0
    read_zero_grid = .false.
    q_init(:) = Xk_shift%pt(1,:) - Xk_save%pt(1,:) ! units of [iku]
    if(v_is_zero( q_init )) then
      call msg('r','First grid is unshifted grid.')
      read_zero_grid = .true.
      grid_offset = Xk_save%nibz
      num_long_grid = num_long_grid - 1
    endif
    !
    ! Find the displacement vector for each grid
    !
    ! q_long(:,i)   is the shift vector of grid(i) in [iku]
    ! q_mod(i) is the modulus of the grid shift vector (i) in [a.u.] 
    !
    q_mod(1:3) = 0.0_SP
    do iq = 1, num_long_grid
      i_grid_k = (iq-1)*Xk_save%nibz + grid_offset
      q_init(:) = Xk_shift%pt(i_grid_k + 1,:) - Xk_save%pt(1,:) ! units of [iku]
      !
      ! Check grid is uniformly shifted
      !
      do ik = 2, Xk_save%nibz
        if(.not.v_is_zero( Xk_shift%pt(i_grid_k+ik,:) - &
&                           Xk_save%pt(ik,:) - q_init(:) )) then
          call msg('r','Long. gauge shifts are not uniform for all {k}.')
          ifail = ifail + 1
          return
        endif
      enddo
      q_long(:,iq) = q_init(:)
      call c2a(v_in=q_long(:,iq), v_out=qtemp, mode="ki2c")
      q_mod(iq) = v_norm(qtemp)
      call msg('r','Grid'//intc(iq)//' has shift [a.u.]:',q_mod(iq))
    enddo
    !
    ! Check the lengths are compatible 
    !
    q0mod = q_mod(1)  ! a.u.
    q_mod(1:num_long_grid) = q_mod(1:num_long_grid) - q0mod
    if(.not.all( abs(q_mod).lt.zero )) then
      call msg('r','Note: Long. gauge shifts are not uniform for all {k}.')
    endif

    call msg('nr','Longitudinal grids found: '//intc(num_long_grid))

    return
  end subroutine long_grid_setup


   subroutine rotate_oscillator_frame(ifail)
     !
     ! Construct the rotated frame (x,y,z) -> (a,b,c) 
     ! and the rotation matrix R_long: v' = R_long . v
     !
     use com,             only : warning
     use pars,            only : SP
     use vec_operate,     only : cross_product, v_is_zero, normalize_v
     use X_m,             only : q_long, R_long
    implicit none
     integer, intent(inout) :: ifail
     real(SP)                 :: a(3), b(3), c(3)
     integer                  :: iq, i1
     logical                  :: bfound = .false.
     if(ifail>0) return
     
     !
     ! Make the new basis (unit) vectors
     !
     select case (num_long_grid)
    case(3)
      a(:) = normalize_v( q_long(:,1) )
      b(:) = normalize_v( q_long(:,2) )
      c(:) = normalize_v( q_long(:,3) )
    case(2)
      a(:) = normalize_v( q_long(:,1) )
      b(:) = normalize_v( q_long(:,2) )
      c = cross_product(a,b) 
    case(1)
      a(:) = normalize_v( q_long(:,1) )
      !
      ! Try to use normal Cartesian axes where possible
      !
      b(:) = 0.0_SP
      do i1 = 1,3 
        if(abs(a(i1)).lt.zero) then
          b(i1) = 1.0_SP
          bfound = .true.
          exit
        endif
      enddo
      !
      ! Otherwise contruct an orthogonal vector
      !
      if(.not.bfound) then
        b(1:2) = a(1:2)
        b(3) = -(a(1)**2 + a(2)**2)/a(3)
        b(:) = normalize_v( b )
      endif
      c = cross_product(a,b) 
    end select
    !
    ! Check the orthonormality of the new basis vectors
    !
    if(.not.all( (/ v_is_zero(cross_product(a,b) - c), & 
&                   v_is_zero(cross_product(b,c) - a), &
&                   v_is_zero(cross_product(c,a) - b) /) )) then
      call msg('r','Basis vectors of new frame:',a(1:3))
      call msg('r','                           ',b(1:3))
      call msg('r','                           ',c(1:3))
      call msg('r','Shifted grids yield a non-orthonormal basis.')
      ifail = ifail + 1
      return
    endif 
    !
    ! The rotation matrix is simply formed by the row vectors
    ! of the new frame wrt the old frame
    !
    R_long(1,1:3) = a(1:3)
    R_long(2,1:3) = b(1:3)
    R_long(3,1:3) = c(1:3)
    call msg('r','Rotation matrix of oscillators:',R_long(1,1:3))
    call msg('r','                               ',R_long(2,1:3))
    call msg('r','                               ',R_long(3,1:3))

    return
  end subroutine rotate_oscillator_frame

  subroutine select_field_dir(ifail)
    !
    ! Select and check the requested field direction
    ! Note that field_dir has length q0_defmod ALWAYS
    !
    use pars,            only : SP
    use X_m,             only : R_long, q_long
    use vec_operate,     only : v_is_zero, c2a, normalize_v, cross_product, v_norm
    implicit none
    integer, intent(inout) :: ifail
    real(SP)               :: qtemp(3), field_tmp(3), field_dir_norm(3)

    if(ifail>0) return
    ! Here we need vectors normalized for comparisons
    field_dir_norm(:) = normalize_v(field_dir)

    select case (num_long_grid)
    case(3) 
      !
      ! 3 grids defined, just rotate field vector to new plane
      !
      field_tmp(:) = matmul(R_long,field_dir)
      call msg('r','Calculated field direction: [a.u., Cartesian frame] ',field_dir_norm)
      
    case(2) 
      !
      ! 2 grids defined, check field vector in the plane and rotate
      !
      if( abs(dot_product(R_long(3,:),field_dir_norm)).gt.zero ) then
        call msg('r','Field direction does not lie in plane allowed by grids.')
        ifail = ifail + 1
        return ! Don't overwrite field_dir
      endif 
      
      field_tmp(:) = matmul(R_long,field_dir)
      call msg('r','Calculated field direction: [a.u., Cartesian frame] ',field_dir_norm)
      
    case(1)
      !
      ! 1 grid defined, set field vector to the only possible value 
      !
      ! Convert q_long(:,1) from [iku] to [cc]
      !
      call c2a(v_in=q_long(:,1), v_out=qtemp, mode="ki2c") ! 
      qtemp(:) = normalize_v(qtemp)
      field_tmp(:) = matmul(R_long, qtemp) * q0_defmod 
      call msg('r','Calculated field direction: [a.u., Cartesian frame] ',qtemp)
      !
      ! Just check that the new field direction is now (trivially) along x'
      !
      if( .not.v_is_zero(field_tmp(:)-(/q0_defmod, 0.0_SP, 0.0_SP/) ) ) then
        call error('Problem with rotated field vector')
      endif
      !
      ! Warn if direction of field is different from that in input (q= -q)
      !
      if( abs(v_norm(cross_product(R_long(1,:),field_dir_norm))).gt.zero ) then
          call warning(' Overwriting requested field direction with grid defined value')
      endif

    end select 
    !
    ! Only overwrite field_dir if above is successful, so transverse calculation
    ! will still run
    !
    field_dir(:) = field_tmp(:) 
    !
    ! Report the new field direction
    !
    field_dir_norm(:) = normalize_v(field_dir)
    call msg('r','Calculated field direction: [a.u. oscillator frame] ',field_dir_norm)
    return
  end subroutine select_field_dir

 end subroutine X_O_longitudinal_cut
