!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): HM, DS
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
subroutine io_BS_PAR_block(iq,i_block,ID,mode)
 !
 ! Write one block of the BSE matrix to disk
 ! There is a single file for the full BSE matrix with maximum four variables
 ! for each of the four blocks that can be present (resonant, anti-resonant, coupling...)
 ! For each block only the upper triangular part is written
 ! Each block has a status variables indicating if the matrix element was already calculated
 !
 use pars,          ONLY:IP,IPL,SP,schlen
 use BS,            ONLY:BS_blk,BSK_IO_mode,MAX_BSK_LIN_size,n_BS_blks_min
 use timing_m,      ONLY:timing
 use IO_m,          ONLY:io_unit,netcdf_call,nf90_sync,&
&                        write_is_on,read_is_on,io_BS_K,YAMBO_CMPLX_NO_FILL
 use IO_int,        ONLY:io_variable_bulk
 !
 implicit none
 !
 integer,      intent(in) :: ID
 integer,      intent(in) :: iq,i_block
 character(*), intent(in) :: mode
 !
 ! Work Space
 !
 character(schlen)   :: BSK_IO_local
 logical             :: lwrite(3)
 integer             :: x1,x2,VAR_IDs(2),stat,ID_now(2)
 integer             :: n2,i1,i2,idx1,idx2,imax,i_rep(2),ilast
 integer(IPL)        :: idx
 !
 complex(SP), allocatable :: TMP_IO_VAR(:)
 !
 if( (.not.io_BS_K) .and. trim(mode)=="full") return
 !
 call timing('io_BS',OPR='start')
 !
 ! Get the type of block that we want to write 
 !
 select case(BS_blk(i_block)%mode)
 case("R")
   VAR_IDs=(/1,2/)
 case("C")
   VAR_IDs=(/3,4/)
 case("A")
   VAR_IDs=(/5,6/)
 end select
 !
 ! Get lower bound coordinates in 2D matrix
 x1=BS_blk(i_block)%coordinate(1)
 x2=BS_blk(i_block)%coordinate(2)
 !
 BSK_IO_local=trim(BSK_IO_mode)
 if(index(mode,"compressed")/=0) BSK_IO_local=trim(mode)
 !
 select case(trim(BSK_IO_local))
 case("1D_linear","compressed_head","compressed_mat")
   !
   do i2=1,BS_blk(i_block)%size(2)
     !
     if(trim(mode)=="compressed_mat") then
       idx = BS_blk(i_block)%idx_compr(i2)
       imax= count(BS_blk(i_block)%done(:,i2)=="t")
     else
       ! tmp coordinate in 2D matrix, runs from x2 to x2 + SIZE -1
       n2  = (x2+i2-1)
       !
       ! coordinate in linearized matrix n2*(n2-1)/2 counting all the elements of the
       ! triangular matrix as follow
       ! 1  2  4  7 11
       ! x  3  5  8 12
       ! x  x  6  9 
       ! x  x  x 10 
       idx = int(n2,kind=IPL)*int(n2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
       !
       ! number of rows from the first in the block to the diagonal of the matrix
       ! I'm going to write elements from idx to idx+imax-1
       imax=min(BS_blk(i_block)%size(1),n2-x1+1)
       !
     endif
     !
     if(imax<1) cycle
     !
     ! if idx > MAX_BSK_LIN_size switch to the next variable
     i_rep(1)=1+int((idx-int(1,IPL))/int(MAX_BSK_LIN_size,IPL),IP)
     ! if idx+imax  > MAX_BSK_LIN_size switch to the next variable
     i_rep(2)=1+int((idx+int(imax-2,IPL))/int(MAX_BSK_LIN_size,IPL),IP)
     ! I need both the previous two because a block can be written
     ! part on linear variable, part on the next
     !
     idx1= int(idx-int(MAX_BSK_LIN_size,IPL)*int(i_rep(1)-1,IPL),IP)
     !
     if(trim(mode)=="compressed_mat") then
       allocate(TMP_IO_VAR(imax))
       if(write_is_on(ID)) then
         idx=0
         do i1=1,BS_blk(i_block)%size(1)
           if( BS_blk(i_block)%done(i1,i2)/="t") cycle
           idx=idx+1
           TMP_IO_VAR(idx)=BS_blk(i_block)%mat(i1,i2)
         enddo
       endif
     endif
     !
     ! Write column by column in 1D
     !
     lwrite(1)=trim(mode)=="compressed_head"
     lwrite(2)=trim(mode)=="full"
     lwrite(3)=trim(mode)=="compressed_mat"
     !
     if(idx1+int(imax,IPL)-1<=MAX_BSK_LIN_size) then
       ! Block is all in one variable
       ID_now(1)=VAR_IDs(2)+(i_rep(1)-1)*8
       ID_now(2)=VAR_IDs(1)+(i_rep(1)-1)*8
       if(lwrite(1)) call io_variable_bulk(ID,ID_now(1), CH1=BS_blk(i_block)%done(:imax,i2), IPOS=[  idx1] )
       if(lwrite(2)) call io_variable_bulk(ID,ID_now(2),  C1=BS_blk(i_block)%mat(:imax,i2),  IPOS=[1,idx1] )
       if(lwrite(3)) call io_variable_bulk(ID,ID_now(2),  C1=TMP_IO_VAR,                     IPOS=[1,idx1] )
     else
       ! Block is split
       ilast=MAX_BSK_LIN_size-idx1+1
       if(lwrite(1)) then
         ID_now(1)=VAR_IDs(2)+(i_rep(1)-1)*8
         ID_now(2)=VAR_IDs(2)+(i_rep(2)-1)*8
         call io_variable_bulk(ID,ID_now(1), CH1=BS_blk(i_block)%done(:ilast,i2),       IPOS=[  idx1] )
         call io_variable_bulk(ID,ID_now(2), CH1=BS_blk(i_block)%done(ilast+1:imax,i2), IPOS=[     1] )
       endif
       if(lwrite(2)) then
         ID_now(1)=VAR_IDs(1)+(i_rep(1)-1)*8
         ID_now(2)=VAR_IDs(1)+(i_rep(2)-1)*8
         call io_variable_bulk(ID,ID_now(1),  C1=BS_blk(i_block)%mat(:ilast,i2),        IPOS=[1,idx1] )
         call io_variable_bulk(ID,ID_now(2),  C1=BS_blk(i_block)%mat(ilast+1:imax,i2),  IPOS=[1,   1] )
       endif
       if(lwrite(3)) then
         ID_now(1)=VAR_IDs(1)+(i_rep(1)-1)*8
         ID_now(2)=VAR_IDs(1)+(i_rep(2)-1)*8
         call io_variable_bulk(ID,ID_now(1),  C1=TMP_IO_VAR(:ilast),                    IPOS=[1,idx1] )
         call io_variable_bulk(ID,ID_now(2),  C1=TMP_IO_VAR(ilast+1:imax),              IPOS=[1,   1] )
       endif
     endif
     !
     if(trim(mode)=="full".and.read_is_on(ID)) then
       do i1=1,imax
         if(BS_blk(i_block)%mat(i1,i2)==YAMBO_CMPLX_NO_FILL) cycle
         BS_blk(i_block)%done(i1,i2)='t'
       enddo
     endif
     !
     if(trim(mode)=="compressed_mat") then
       if(read_is_on(ID)) then
         idx=0
         do i1=1,BS_blk(i_block)%size(1)
           if( BS_blk(i_block)%done(i1,i2)/="t") cycle
           idx=idx+1
           BS_blk(i_block)%mat(i1,i2)=TMP_IO_VAR(idx)
         enddo
       endif
       deallocate(TMP_IO_VAR)
     endif
     !
   enddo
   !
   if(trim(mode)=="compressed_mat") deallocate(BS_blk(i_block)%idx_compr)
   !
 case("2D_standard")
   ! 
   ! Write one block at a time in 2D
   !
   call io_variable_bulk(ID, VAR_IDs(1),  C2=BS_blk(i_block)%mat , IPOS=(/1,x1,x2/) )
   if(read_is_on(ID)) then
     do i2=1,BS_blk(i_block)%size(2)
       do i1=1,BS_blk(i_block)%size(1)
         if(BS_blk(i_block)%mat(i1,i2)==YAMBO_CMPLX_NO_FILL) cycle
         BS_blk(i_block)%done(i1,i2)='t'
       enddo
     enddo
   endif
   !
 end select
 !
 if(write_is_on(ID).and.i_block<=n_BS_blks_min) then
   stat = nf90_sync(io_unit(ID))
   call netcdf_call(stat,ID)
 endif
 !   
 call timing('io_BS',OPR='stop')
 !
end subroutine