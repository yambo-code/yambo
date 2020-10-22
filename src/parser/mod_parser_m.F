!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use pars         ,ONLY: SP,lchlen,DP
 use PARSER_lib   ,ONLY: iparse_isdef
 use OUTPUT_simple,ONLY: OUTPUT_unit_conversion
 !
 interface parser
   module procedure lqparser,chparser,chqparser,chmparser,i1qparser,iqparser,imqparser,&
&                   r1qparser,rqparser,rmqparser,c1qparser
 end interface
 !
 interface 
   subroutine PARSER_array(mode,VAR,N,M,DESC,CH_v,R_v,I_v,Kn)
     use pars,  ONLY:SP,lchlen
     implicit none
     character(*)                ::mode
     integer                     ::N,M
     character(*)                ::VAR
     character(*)     , OPTIONAL ::DESC
     character(*)     , OPTIONAL ::CH_v(N,M)
     integer,           OPTIONAL ::I_v(N,M)
     real(SP),          OPTIONAL ::R_v(N,M)
     character(1),      OPTIONAL ::Kn(M)
   end subroutine
 end interface
 !
 contains
   !
   subroutine chparser(name,res)
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
     implicit none
     character(*), intent(in) :: name
     integer, intent(out)     :: res
     integer :: def           !ws
     def=res
     call iparse_int(cstr(trim(name)),def,res)
     call OUTPUT_unit_conversion(iVAR=res,DESC=name,PARSER_call=.TRUE.)
   end subroutine
   !
   subroutine iqparser(name,res)
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in) :: name
     integer, intent(out)  :: res(:)
     integer :: i1,dim(1) !ws
     dim=shape(res)
     if (iparse_isdef(cstr(trim(name)))==0) return
     do i1=1,dim(1)
       call iparse_block_int(cstr(trim(name)),0,i1-1,res(i1))
       call OUTPUT_unit_conversion(iVAR=res(i1),DESC=name,PARSER_call=.TRUE.)
     enddo
   end subroutine
   !
   subroutine imqparser(name,res)
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
         call OUTPUT_unit_conversion(iVAR=res(i1,i2),DESC=name,PARSER_call=.TRUE.)
       enddo
     enddo
   end subroutine
   !
   subroutine r1qparser(name,res)
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in) :: name
     real(SP), intent(out) :: res
     real(DP) :: dres,ddef  ! ws
     ddef=res
     call iparse_double(cstr(trim(name)),ddef,dres)
     res=dres
     call OUTPUT_unit_conversion(rVAR=res,DESC=name,PARSER_call=.TRUE.)
   end subroutine
   !
   subroutine rqparser(name,res)
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
       call OUTPUT_unit_conversion(rVAR=res(i1),DESC=name,PARSER_call=.TRUE.)
     enddo
   end subroutine
   !
   subroutine rmqparser(name,res)
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
         call OUTPUT_unit_conversion(rVAR=res(i1,i2),DESC=name,PARSER_call=.TRUE.)
       enddo
     enddo
   end subroutine
   !
   subroutine lqparser(name,res)
     use stderr, ONLY : cstr
     character(*), intent(in) :: name
     logical,      intent(out):: res
     res=(iparse_isdef(cstr(trim(name)))==1)
   end subroutine
   !
   subroutine c1qparser(name,res)
     use stderr, ONLY : cstr
     implicit none
     character(*), intent(in):: name
     complex(SP), intent(out):: res
     complex(DP) :: dres,ddef  ! ws
     real(SP) :: r_i,r_r
     ddef=res
     call iparse_complex(cstr(trim(name)),ddef,dres)
     r_r=real(dres)
     r_i=aimag(dres)
     call OUTPUT_unit_conversion(rVAR=r_r,DESC=name,PARSER_call=.TRUE.)
     call OUTPUT_unit_conversion(rVAR=r_i,DESC=name,PARSER_call=.TRUE.)
     res=r_r+(0._SP,1._SP)*r_i
   end subroutine
   !
end module parser_m
