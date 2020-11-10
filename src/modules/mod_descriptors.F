!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
module descriptors
 !
 use pars,       ONLY:SP,DP,lchlen,schlen
 !
 integer, parameter :: n_MAX_descs=100
 !
 type IO_desc
   !
   integer           :: n=0
   integer           :: size(n_MAX_descs)
   logical           :: check(n_MAX_descs)
   character(2)      :: ops(10,n_MAX_descs)
   character(schlen) :: str(n_MAX_descs)      ! Main variable body
   character(schlen) :: term(n_MAX_descs)     ! Terminator string. E.G. o/o, eV, fs....
   character(1)      :: kind(n_MAX_descs)     ! "i"=integer, "r"=real, "d"=double, "c"=complex, "s"=string, "l"=logical
   !
   integer           :: ival(10,n_MAX_descs)
   real(SP)          :: rval(10,n_MAX_descs)
   !
   real(DP)          :: dval(n_MAX_descs)
   complex(SP)       :: cval(n_MAX_descs)
   !
   character(schlen) :: sval(n_MAX_descs)
   logical           :: lval(n_MAX_descs)
   !
 end type
 !
 contains
   !
   subroutine IO_desc_add(desc,kind,str,SZ,term,check,ops,I,R,D,C,S,L)
     type(IO_desc),    intent(inout)  :: desc
     character(1)                     :: kind
     character(*)                     :: str
     character(*),          optional  :: term,S
     character(2),          optional  :: ops(:)
     integer,               optional  :: SZ,I(:)
     real(SP),              optional  :: R(:)
     real(DP),              optional  :: D
     complex(SP),           optional  :: C
     logical,               optional  :: L,check
     !
     ! Work Space
     integer                          :: l_size
     !
     desc%n=desc%n+1
     desc%str(desc%n)=str
     desc%size(desc%n)=1
     if (present(SZ)) then
       desc%size(desc%n)=SZ
     endif
     desc%kind(desc%n)=kind
     !
     desc%term(desc%n)=" "
     if (present(term)) then
       desc%term(desc%n)=trim(term)
     endif
     if (present(S)) then
       desc%sval(desc%n)=trim(S)
     endif
     if (present(ops)) then
       l_size=size(ops)
       desc%ops(:l_size,desc%n)=ops
     endif
     if (present(I)) then
       l_size=size(I)
       desc%ival(:l_size,desc%n)=I
     endif
     if (present(R)) then
       l_size=size(R)
       desc%rval(:l_size,desc%n)=R
     endif
     if (present(D)) then
       desc%dval(desc%n)=D
     endif
     if (present(C)) then
       desc%cval(desc%n)=C
     endif
     if (present(L)) then
       desc%lval(desc%n)=L
     endif
     if (present(check)) then
       desc%check(desc%n)=check
     endif
     !
   end subroutine
   !
   subroutine IO_desc_element_duplicate(desc_in,desc_out,i_d_in,i_d_out)
     type(IO_desc),    intent(in)    :: desc_in
     type(IO_desc),    intent(inout) :: desc_out
     integer,          intent(in)    :: i_d_in,i_d_out
     desc_out%size(i_d_out)   = desc_in%size(i_d_in)
     desc_out%term(i_d_out)   = desc_in%term(i_d_in)
     desc_out%str (i_d_out)   = desc_in%str(i_d_in)
     desc_out%kind(i_d_out)   = desc_in%kind(i_d_in)
     desc_out%check(i_d_out)  = desc_in%check(i_d_in)
     desc_out%ops (:,i_d_out) = desc_in%ops(:,i_d_in)
     desc_out%ival(:,i_d_out) = desc_in%ival(:,i_d_in)
     desc_out%rval(:,i_d_out) = desc_in%rval(:,i_d_in)
     desc_out%dval(i_d_out)   = desc_in%dval(i_d_in)
     desc_out%cval(i_d_out)   = desc_in%cval(i_d_in)
     desc_out%sval(i_d_out)   = desc_in%sval(i_d_in)
     desc_out%lval(i_d_out)   = desc_in%lval(i_d_in)
   end subroutine
   !
   subroutine IO_desc_duplicate(desc_in,desc_out,offset)
     type(IO_desc),    intent(in)  :: desc_in
     type(IO_desc),    intent(out) :: desc_out
     integer,optional, intent(in)  :: offset
     integer  :: i_d,off_
     off_=0
     if (present(offset)) then
       off_=offset
     endif
     desc_out%n    = desc_in%n + off_
     do i_d=1,desc_in%n
       call IO_desc_element_duplicate(desc_in,desc_out,i_d+off_,i_d)
     enddo
   end subroutine
   !
   subroutine IO_desc_append(desc_in,desc_out)
     type(IO_desc),    intent(in)    :: desc_in
     type(IO_desc),    intent(inout) :: desc_out
     integer  :: i_d
     do i_d=1,desc_in%n
       call IO_desc_element_duplicate(desc_in,desc_out,i_d,desc_out%n+i_d)
     enddo
     desc_out%n = desc_out%n+desc_in%n 
   end subroutine
   !
   subroutine IO_desc_reset(desc)
     type(IO_desc), intent(out) :: desc
     desc%n    = 0
     desc%size = 0
     desc%term = ""
     desc%str  = ""
     desc%kind = ""
     desc%check= .false.
     desc%ops  = ""
     desc%ival = 0
     desc%rval = 0._SP
     desc%dval = 0._DP
     desc%cval = (0._SP,0._SP)
     desc%sval = " "
     desc%lval = .false.
   end subroutine
   !
end module descriptors
