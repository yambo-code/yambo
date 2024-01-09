!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine Check_symmetries(field_dir,field_kind)
  !
  use pars,          ONLY:SP
  use vec_operate,   ONLY:v_norm
  use matrix_operate,ONLY:m3det
  use com,           ONLY:msg
  use D_lattice,     ONLY:nsym,dl_sop
  use stderr,        ONLY:intc
  implicit none
  !
  real(SP),     intent(in)  :: field_dir(3)
  character(*), intent(in)  :: field_kind
  !
  ! Work space
  !
  integer  :: is,nsym_wrong,i_wrong
  real(SP) :: v(3)
  !
  if(v_norm(field_dir)<1.E-5) return
  !
  nsym_wrong=0
  !
  select case(field_kind)
  case("magnetic")
    do is=1,nsym
      v(:) = matmul( dl_sop(:,:,is), field_dir )
      i_wrong=0
      if( all( abs(v(:)-field_dir(:))<1E-5 )) i_wrong=+1
      if( all( abs(v(:)+field_dir(:))<1E-5 )) i_wrong=-1
      if(i_wrong==0) then
        nsym_wrong=nsym_wrong+1
      else
        if( m3det(dl_sop(:,:,is))/i_wrong-1>1E-5 ) nsym_wrong=nsym_wrong+1
      endif
    enddo
  case("electric")
    do is=1,nsym
      v(:) = matmul( dl_sop(:,:,is), field_dir )
      i_wrong=0
      if( v_norm(v(:)-field_dir(:))>1.E-5) nsym_wrong=nsym_wrong+1
    enddo
  case default
    call error(" wrong field kind")  
  end select
  !
  if(nsym_wrong/=0) then
    call msg('s','Detected '//intc(nsym_wrong)//' wrong symmetries (out of '//intc(nsym)//' )')
    call error(' Use ypp -y to repair the symmetry group and restart Yambo ')
  endif
  !
end subroutine Check_symmetries
