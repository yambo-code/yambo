!
! Copyright (C) 2000-2008 C. Attaccalite & A. Marini and the YAMBO team
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
subroutine SC_check_symmetries(field_dir)
  !
  use pars,          ONLY:SP,lchlen
  use vec_operate,   ONLY:v_norm
  use com,           ONLY:msg,error
  use R_lattice,     ONLY:rl_sop
  use D_lattice,     ONLY:nsym
  use stderr,        ONLY:intc
  implicit none
  !
  real(SP), intent(in)  :: field_dir(3)
  !
  ! Work space
  !
  integer  :: is,nsym_wrong
  real(SP) :: v(3)
  !
  if(v_norm(field_dir)<1.E-5) return
  !
  nsym_wrong=0
  !
  do is=1,nsym
    !      
    v(:) = matmul( rl_sop(:,:,is), field_dir )
    !
    if( v_norm(v(:)-field_dir(:))>1.E-5) then
      nsym_wrong=nsym_wrong+1
    endif
    !
  enddo
  !
  if(nsym_wrong/=0) then
    call msg('s','Detected '//intc(nsym_wrong)//' wrong symmetries (out of '//intc(nsym)//' )')
    call error(' Use ypp_rt -n to remove fix the symmetry group and restart Yambo ')
    stop '  '
  endif
  !
end subroutine SC_check_symmetries
