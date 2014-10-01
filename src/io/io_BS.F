!
!        Copyright (C) 2000-2014 the YAMBO team
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
 use R_lattice,     ONLY:nXkibz
 use stderr,        ONLY:intc
 use matrix_operate,ONLY:mat_c2r,mat_r2c,UP_matrix_index
 use parallel_m,    ONLY:myid
 use BS,            ONLY:BS_mat,BS_K_dim,BS_K_coupling,&
&                        BS_nT_at_k,BS_res_mode,&
&                        BS_DB_is_fragmented,BS_blk,n_BS_blks,&
&                        BS_cpl_mat,BSS_description,BSS_n_descs,variables_BS
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_elemental,io_bulk,io_restart_point,&
&                        read_is_on,write_is_on,io_header,io_mode,&
&                        VERIFY,io_fragmented,Fragmented_IO,&
&                        io_fragment,IO_NOT_ALLOWED,io_BS_K,RD_CL,io_action,RD_CL_IF_END
 implicit none
 type(X_t):: X 
 integer  :: iq,ID
 !
 ! Work Space
 !
 integer             ::ik12(2),i_block,i1,BS_k_and_row_restart(2)
 character(schlen)   ::ch,db_name
 character(lchlen)   ::local_description(100)
 real(SP),allocatable::BS_mat_disk(:,:,:)
 logical             ::db_is_large,load_DB_once_for_all
 !
 if (.not.io_BS_K) then
   io_BS_K=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 db_name='BS_Q'//trim(intc(iq))//'_CPU_'//trim(intc(myid))
 !
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (io_BS/=0) goto 1
 !
 ! If the DB size > 1.9Gb  I automatically switch on the fragmentation.
 ! To do so both dbs_fragment (overall fragmentation) and 
 ! io_fragmented(ID) (fragmentation for this DB) must be set true.
 ! This is because io_fragmented(ID) is defined in reading but not
 ! in writing where it keep the default (dbs_fragment) value.
 !
 ! Update [27/6/07]: NETCDF performance increase drammatically
 ! when using fragmentation. So I lowered the treshold to enable 
 ! fragmentation to matrices >= 6000x6000
 !
 if (write_is_on(ID)) then
   db_is_large=BS_K_dim>6000
   if (BS_K_coupling) db_is_large=BS_K_dim>3000
   if (db_is_large) then
     Fragmented_IO=.true.
     io_fragmented(ID)=.true.
   endif
 endif
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   if (io_mode(ID)==VERIFY.or.write_is_on(ID)) then
     io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,XC_KIND="K_WF force Xs",&
&                    PARALLEL_STRUCTURE="Response_T_space",CUTOFF=.true.)
   else
     io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,XC_KIND="K_WF force Xs",CUTOFF=.true.)
   endif
   !
   if (io_BS/=0) then
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     goto 1
   endif
   !
   BS_DB_is_fragmented=io_fragmented(ID)
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
 if (io_action(ID)==RD_CL_IF_END.and.i_block==n_BS_blks) io_action(ID)=RD_CL
 !
 ! Fragmentation
 !
 ! io_fragmented is read in the header. While accessing the fragments
 ! io_fragmented is not defined and must be overwritten with BS_DB_is_fragmented
 !
 io_fragmented(ID)=BS_DB_is_fragmented
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
1 call io_disconnect(ID)
 !
end function
