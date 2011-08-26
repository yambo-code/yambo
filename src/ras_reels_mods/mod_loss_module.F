module loss_module
  use pars,                 ONLY : SP
  implicit none
  real(SP), allocatable         :: loss(:,:)   ! defined on nwtot, never written

contains

subroutine alloc_loss( ndum, nw )
  implicit none
  integer,          intent(in)    :: ndum, nw
  allocate(loss(ndum,nw))
! Probably not allowed on XD1
  return
end subroutine alloc_loss

subroutine dealloc_loss
  implicit none
  if(allocated(loss)) deallocate(loss)
  return
end subroutine dealloc_loss

subroutine write_loss( hw, nw, ipol )
  !
  ! NEver used!
  !
  use com,                    only : msg, of_open_close
  use pars,                   only : SP, schlen
  use units,                  only : HA2EV
  implicit none
  integer,          intent(in)    :: nw, ipol
  real(SP),         intent(in)    :: hw(nw)
! ws
  real(SP)                        :: od(8)
  integer                         :: iw
  character(schlen)               :: of_name

  if(nw.ne.size(loss,2)) then
     write(*,*) "Error with size of loss, nw",size(loss,2),nw
     return
  endif

  write (of_name,'(a,a)') 'reels','-rpa'
  call of_open_close(of_name,'ot')

  do iw = 1, nw
    od = (/ hw(iw)*HA2EV, &
&           loss(1,iw), loss(2,iw), loss(3,iw), &
&           loss(4,iw), loss(5,iw), loss(5,iw), &
&           loss(5,iw) /)
    call msg('o reels','',od, INDENT=0, USE_TABS=.true.)
  enddo
  call of_open_close(of_name)

  return
end subroutine write_loss

end module loss_module
