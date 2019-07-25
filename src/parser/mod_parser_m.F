!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module parser_m
 !
 use pars
 !
 interface parser
   module procedure lqparser,chparser,chqparser,chmparser,i1qparser,iqparser,imqparser,&
&                   r1qparser,rqparser,rmqparser,c1qparser
 end interface
 !
 contains
   !
   subroutine chparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in)  :: name
     character(*), intent(out) :: res
     character(lchlen) :: def
     def=trim(res)
     if (iparse_isdef(cstr(trim(name)))==0) return
     res=' '
     call iparse_string(cstr(name),cstr(def),res)
     if (trim(res)=='"') res=' '
   end subroutine
   !
   subroutine chqparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in)  :: name
     character(*), intent(out) :: res(:)
     integer :: i1,dim(1) !ws
     dim=shape(res)
     if (iparse_isdef(cstr(trim(name)))==0) return
     do i1=1,dim(1)
       call iparse_block_string(cstr(trim(name)),0,i1-1,res(i1))
       if (trim(res(i1))=='"') res=' '
     enddo
   end subroutine
   !
   subroutine chmparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in)  :: name
     character(*), intent(out) :: res(:,:)
     integer :: i1,i2,dim(2) !ws
     dim=shape(res)
     if (iparse_isdef(cstr(trim(name)))==0) return
     do i1=1,dim(1)
       do i2=1,dim(2)
         call iparse_block_string(cstr(trim(name)),i1-1,i2-1,res(i1,i2))
       enddo
     enddo
   end subroutine
   !
   subroutine i1qparser(name,res)
     use stderr, ONLY : cstr
     use parser_lib
     implicit none
     character(*), intent(in) :: name
     integer, intent(out)     :: res
     integer :: def           !ws
     def=res
     call iparse_int(cstr(trim(name)),def,res)
     call ps_convert(0._SP,res,name)
   end subroutine
   !
   subroutine iqparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in) :: name
     integer, intent(out)  :: res(:)
     integer :: i1,dim(1) !ws
     dim=shape(res)
     if (iparse_isdef(cstr(trim(name)))==0) return
     do i1=1,dim(1)
       call iparse_block_int(cstr(trim(name)),0,i1-1,res(i1))
       call ps_convert(0._SP,res(i1),name)
     enddo
   end subroutine
   !
   subroutine imqparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in) :: name
     integer, intent(out)  :: res(:,:)
     integer :: i1,i2,dim(2) !ws
     dim=shape(res)
     if (iparse_isdef(cstr(trim(name)))==0) return
     do i1=1,dim(1)
       do i2=1,dim(2)
         call iparse_block_int(cstr(trim(name)),i1-1,i2-1,res(i1,i2))
         call ps_convert(0._SP,res(i1,i2),name)
       enddo
     enddo
   end subroutine
   !
   subroutine r1qparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in) :: name
     real(SP), intent(out) :: res
     real(DP) :: dres,ddef  ! ws
     ddef=res
     call iparse_double(cstr(trim(name)),ddef,dres)
     res=dres
     call ps_convert(res,-1,name)
   end subroutine
   !
   subroutine rqparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in) :: name
     real(SP), intent(out) :: res(:)
     integer :: i1,dim(1) !ws
     real(DP) :: dres    !ws
     dim=shape(res)
     if (iparse_isdef(cstr(trim(name)))==0) return
     do i1=1,dim(1)
       dres=res(i1)
       call iparse_block_double(cstr(trim(name)),0,i1-1,dres)
       res(i1)=dres
       call ps_convert(res(i1),-1,name)
     enddo
   end subroutine
   !
   subroutine rmqparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in) :: name
     real(SP), intent(out) :: res(:,:)
     integer :: i1,i2,dim(2) !ws
     real(DP) :: dres      !ws
     dim=shape(res)
     if (iparse_isdef(cstr(trim(name)))==0) return
     do i1=1,dim(1)
       do i2=1,dim(2)
         dres=res(i1,i2)
         call iparse_block_double(cstr(trim(name)),i1-1,i2-1,dres)
         res(i1,i2)=dres
         call ps_convert(res(i1,i2),-1,name)
       enddo
     enddo
   end subroutine
   !
   subroutine lqparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     character(*), intent(in) :: name
     logical,      intent(out):: res
     res=(iparse_isdef(cstr(trim(name)))==1)
   end subroutine
   !
   subroutine c1qparser(name,res)
     use parser_lib
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in) :: name
     complex(SP), intent(out):: res
     complex(DP) :: dres,ddef  ! ws
     real(SP) :: r_i,r_r
     ddef=res
     call iparse_complex(cstr(trim(name)),ddef,dres)
     r_r=real(dres);r_i=aimag(dres)
     call ps_convert(r_r,-1,name)
     call ps_convert(r_i,-1,name)
     res=r_r+(0._SP,1._SP)*r_i
   end subroutine
   !
end module parser_m
