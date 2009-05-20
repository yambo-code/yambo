!
! Copyright (C) 2000-2008 C. Attaccalite and the YAMBO team
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
subroutine check_symm(field_dir)
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
  character(lchlen)       :: dump_ch
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
    !
    if( v_norm(v(:)-field_dir(:))>1.E-5) then
        call msg('s',' ')
        call msg('s',' * * * SYMMETRY NOT COMPATIBLE * * * ')
        write(dump_ch,'(a,3f12.6,a)') '|',rl_sop(:,1,is),'|'
        call msg('s',dump_ch)      
        write(dump_ch,'(a,3f12.6,a)') '|',rl_sop(:,2,is),'|'
        call msg('s',dump_ch)      
        write(dump_ch,'(a,3f12.6,a)') '|',rl_sop(:,3,is),'|'
        call msg('s',dump_ch)      
        nsym_wrong=nsym_wrong+1
    endif
    !
  enddo
  !
  if(nsym_wrong/=0) then
    call msg('s',' Number of not compatible symm '//intc(nsym_wrong)//' on '//intc(nsym))
    call error(' Use ypp -n to remove these symmetries ')
    stop '  '
  endif
  !
end subroutine check_symm
