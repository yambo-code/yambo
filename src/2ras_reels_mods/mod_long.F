
module ras_long
  use pars,            only : SP
  integer :: grid_dir(3)
  real(SP):: q_long(3,3)
contains

    subroutine select_field_dir(ifail, field_dir, num_long_grid)
    !
    ! Select and check the requested field direction
    ! Note that field_dir has length q0_def_norm ALWAYS
    !
    use pars,            only : SP
    use vec_operate,     only : v_is_zero, c2a, normalize_v, cross_product, v_norm
    use D_lattice,       only : nsym, dl_sop
    use R_lattice,       only : q0_def_norm
        use stderr,          only : intc
  use com,                 only : msg, warning
    implicit none
    integer, intent(inout) :: ifail
    integer, intent(in) :: num_long_grid
    real(SP), intent(inout) :: field_dir(3)
    real(SP)               :: field_tmp(3), field_dir_norm(3), ca(3,3), field_dir_rot(3)
    real(SP)         :: q_long_norm(3,3), qtemp(3)
    real(SP)          ::  zero = 1.0E-5
    character(len=1), parameter, dimension (3) :: cx = (/ 'x','y','z' /)
    integer                :: ix,iq, itmp(1), is

    call msg('r','Check consistency of q-vector with longitudinal grids:')
!   write(*,*)
!   write(*,*) "Select field long"
!   write(*,*)
    ca=reshape((/1.,0.,0.,0.,1.,0.,0.,0.,1./),(/3,3/))

    if(ifail>0) return
    !
    ! Check the shift vectors are allowed
    !
    do iq = 1,num_long_grid
      call c2a(v_in=q_long(:,iq), v_out=qtemp, mode="ki2c")
      q_long_norm(:,iq) = normalize_v(qtemp)
    enddo

    field_tmp = field_dir
    select case (num_long_grid)
    case(3)
      if(any(abs(q_long_norm-ca).gt.zero)) then
        call msg('r','Long. gauge grids must be in standard cartesian frame.')
        ifail = ifail + 1; return
      endif
      grid_dir(:) = (/ 1,2,3 /)
    case(2)
      q_long_norm(:,3) = cross_product( q_long_norm(:,1), q_long_norm(:,2) )
      do iq = 1,2
        itmp =  maxloc( q_long_norm(:,iq) )
        ix = itmp(1)
        grid_dir(iq) = ix
        if(any(abs(q_long_norm(:,iq)-ca(:,ix)).gt.zero)) then
          call msg('r','Long. gauge grids must be standard cartesian axes.')
          ifail = ifail + 1; return
        endif
        call msg('r','Grid '//intc(iq)//' is shifted on '//cx(ix)//' axis.' )
      enddo
      itmp = maxloc( q_long_norm(:,3) )
      grid_dir(3) = itmp(1)
!     write(*,*) " grid_dir(3) = ", grid_dir(3)
      !
      ! Check that field_dir never leaves the plane of the two shifted grids
      !
      do is=1,nsym
        field_dir_rot = matmul( dl_sop(:,:,is), field_dir ) / q0_def_norm
!       write(*,*) " field_dir_rot ",is,"=",  field_dir_rot

        if(abs(field_dir_rot(grid_dir(3))).gt.zero) then
          call msg('r','Symmetries do not allow this q-vector with these grids.')
          ifail = ifail + 1; return
        endif
      enddo
    case(1)
      call msg('r','Field direction specified from grid: [c.c.] ',q_long_norm(:,1))
      field_tmp(:) = (/q0_def_norm, 0.0_SP, 0.0_SP/)
      if( dot_product(field_dir, q_long_norm(:,1)).gt.zero ) then
        call warning(' Overwriting requested field direction with grid defined value')
      endif
      grid_dir(:) = (/ 1,2,3 /) ! Why?
    end select
    !
    ! Only overwrite field_dir if above is successful, so transverse calculation
    ! will still run
    !
    if(ifail.eq.0) call msg('r','Consistency checks passed.')
    field_dir(:) = field_tmp(:)
    return
 end subroutine select_field_dir

end module ras_long
