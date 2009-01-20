subroutine write_eps_tensor( q0v, eps_i, hw, nw, npol, epstype ) 
  use pars,                  only : SP, schlen
  use units,                 only : HARTREE
  use com,                   only : msg, of_open_close
  use vec_operate,           only : v_norm
  implicit none
  integer,          intent(in)    :: nw, npol
  complex(SP),      intent(in)    :: eps_i(nw, npol) 
  real(SP),         intent(in)    :: q0v(3,3), hw(nw)
  character(len=*), intent(in)    :: epstype
! ws
  real(SP)                        :: od(7)
  integer                         :: iw, ipol
  character(10)                   :: headings(7)
  character(1)                    :: qname(3)
  character(schlen)               :: lch, of_name
  real(SP)                        :: q0(3)

  qname(1) = 'X'    ; qname(2) = 'Y'    ; qname(3) = 'Z'
  headings(1:7) = (/'E/ev[1] ','eX/Im[2]','eX/Re[3]','eY/Im[4]','eY/Re[5]','eZ/Im[6]','eZ/Re[7]'/)

  write (of_name,'(a4,a,a)') 'eps_',trim(epstype),'-rpa'
  call of_open_close(of_name,'ot')

  do ipol = 1, npol
    q0(:) = q0v(:,ipol)
    write (lch,'(1x,5a,3f10.5)') 'Eps_',trim(epstype),' @ q || ',qname(ipol),' : ', q0/v_norm(q0)
    call msg('o eps','#',trim(lch),INDENT=0)
  enddo

  call msg('o eps','#','',INDENT=0)
  if(npol.eq.2) call msg('o eps','#',headings(1:5),INDENT=0,USE_TABS=.true.)
  if(npol.eq.3) call msg('o eps','#',headings(1:7),INDENT=0,USE_TABS=.true.)
  call msg('o eps','#','',INDENT=0)

  call of_open_close(of_name,'oa')
  do iw = 1, nw

    od(1:5) = (/ hw(iw) *HARTREE, &
&         aimag( eps_i(iw,1)), real( eps_i(iw, 1) ), &
&         aimag( eps_i(iw,2)), real( eps_i(iw, 2) ) /)
    if(npol.eq.3) od(6:7) = (/ aimag( eps_i(iw, npol)), real( eps_i(iw, npol) ) /)
    if(npol.eq.2) call msg('o eps','',od(1:5))
    if(npol.eq.3) call msg('o eps','',od)

  enddo
  call of_open_close(of_name)

  return
end subroutine write_eps_tensor
