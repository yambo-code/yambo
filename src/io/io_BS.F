!
!        Copyright (C) 2000-2016 the YAMBO team
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
integer function io_BS(iq,X,ID)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use X_m,           ONLY:X_t
 use stderr,        ONLY:intc
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use parallel_m,    ONLY:myid
 use BS,            ONLY:BS_blk,n_BS_blks,BS_K_has_been_calculated_loaded
 use BS_solvers,    ONLY:BSS_description,BSS_n_descs,variables_BS
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_elemental,io_bulk,frag_BS_K,&
&                        read_is_on,write_is_on,io_header,io_mode,VERIFY,&
&                        io_fragment,IO_NOT_ALLOWED,io_BS_K,manage_OP_IF_START_and_CL_IF_END
 implicit none
 type(X_t):: X 
 integer  :: iq,ID
 !
 ! Work Space
 !
 integer             ::i_block,i1
 character(schlen)   ::ch,db_name
 character(lchlen)   ::local_description(100)
 real(SP),allocatable::BS_mat_disk(:,:,:)
 !
 if (.not.io_BS_K.and.maxval(io_sec(ID,:))>1) then
   io_BS=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 db_name='BS_Q'//trim(intc(iq))//'_CPU_'//trim(intc(myid))
 !
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (io_BS/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   if (io_mode(ID)==VERIFY.or.write_is_on(ID)) then
     io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,XC_KIND="K_WF force Xs",&
&                    PARALLEL_STRUCTURE="Response_T_space",CUTOFF=.true.,FRAG=frag_BS_K)
   else
     io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,&
&                       XC_KIND="K_WF force Xs",CUTOFF=.true.,FRAG=frag_BS_K)
   endif
   !
   if (io_BS/=0) then
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     goto 1
   endif
   !
   io_BS=variables_BS(ID,local_description,X=X)
   !
   if (io_BS/=0) then
     !
     ! reset the descriptions...
     !
     BSS_n_descs=0
     !
     goto 1
     !
   endif
   !
   ! Descriptions transfer
   !
   if (BSS_n_descs==0) then
     do i1=1,100
       if (len_trim(local_description(i1))>0) then
         BSS_n_descs=BSS_n_descs+1
         BSS_description(BSS_n_descs)=local_description(i1)
       endif
     enddo
   endif
   !
 endif
 !
 i_block=maxval(io_sec(ID,:))-1
 !
 if (i_block==0) goto 1
 !
 call manage_OP_IF_START_and_CL_IF_END(ID,i_block,n_BS_blks)
 !
 call io_fragment(ID,i_fragment=i_block)
 !
 allocate(BS_mat_disk(BS_blk(i_block)%size(1),BS_blk(i_block)%size(2),2))
 !
 if (BS_blk(i_block)%mode=="R") ch="BSE_RESONANT_BLOCK_"//trim(intc(i_block))
 if (BS_blk(i_block)%mode=="A") ch="BSE_ANTIRESONANT_BLOCK_"//trim(intc(i_block))
 !
 call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/BS_blk(i_block)%size(1),BS_blk(i_block)%size(2),2/))
 !
 if (write_is_on(ID)) call mat_c2r(BS_blk(i_block)%mat,BS_mat_disk)
 !
 call io_bulk(ID,R3=BS_mat_disk)
 !
 if (read_is_on(ID)) call mat_r2c(BS_mat_disk,BS_blk(i_block)%mat)
 !
 deallocate(BS_mat_disk)
 !
1 continue
 !
 ! If the database is not ok but the BS_identifier has been read I may have problems
 ! with the inversion database (like dimensions errors).
 !
 if (io_BS==0) BS_K_has_been_calculated_loaded=.TRUE.
 if (io_BS<0 ) BS_K_has_been_calculated_loaded=.FALSE.
 !
 call io_disconnect(ID)
 !
end function
