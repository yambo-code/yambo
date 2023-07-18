!
!        Copyright (C) 2000-2022 the YAMBO team
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
! be useful, but WITHOUT ANY WARRANTY; without even the ,BSS_desc
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
! Analogously to io_DIPOLES, we write the io files of BS with the
! information of the q-vector
! Just to use what is done:
! BS_Q_##_
!
integer function io_BS(iq,X,ID)
 !
 use pars,          ONLY:SP,schlen
 use X_m,           ONLY:X_t
 use stderr,        ONLY:intc
 use parallel_m,    ONLY:myid
 use iso_c_binding, ONLY:C_LOC,C_F_POINTER
 use BS,            ONLY:BS_blk,BS_K_has_been_calculated_loaded,BS_K_coupling,&
&                        l_BS_ares_from_res,BS_IO_dim,n_BS_blks
 use BS_solvers,    ONLY:BSS_desc,variables_BS
 use IO_m,          ONLY:io_sec,frag_BS_K,io_BS_K,write_is_on,io_mode,VERIFY,IO_NOT_ALLOWED
 use IO_int,        ONLY:io_connect,io_disconnect,io_elemental,&
&                        def_variable_bulk,io_variable_bulk,io_header,io_fragment
 use descriptors,   ONLY:IO_desc,IO_desc_reset,IO_desc_duplicate
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 type(X_t):: X 
 integer  :: iq,ID
 !
 ! Work Space
 !
 complex(SP),pointer ::tmp_link(:)
 integer             ::i_block,j_block,n1,n2,block_pos,VAR_ID,ID_frag
 character(schlen)   ::ch,db_name,PARALLEL_STRUCTURE
 type(IO_desc)       ::local_desc
 !
 if (.not.io_BS_K.and.maxval(io_sec(ID,:))>1) then
   io_BS=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 call timing('io_BS',OPR='start')
 !
 db_name='BS_Q'//trim(intc(iq))//'_CPU_'//trim(intc(myid))
 !
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (io_BS/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   PARALLEL_STRUCTURE="NONE"
   if (io_mode(ID)==VERIFY.or.write_is_on(ID)) PARALLEL_STRUCTURE="Response_T_space"
   !
   io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,XC_KIND="K_WF force Xs",&
&                    PARALLEL_STRUCTURE=trim(PARALLEL_STRUCTURE),CUTOFF=.true.,FRAG=frag_BS_K)
   !
   if (io_BS/=0) then
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     goto 1
   endif
   !
   io_BS=variables_BS(ID,iq,local_desc,.TRUE.,X=X)
   !
   if (io_BS/=0) then
     !
     ! reset the descriptions...
     !
     call IO_desc_reset(BSS_desc)
     !
     goto 1
     !
   endif
   !
   ! Descriptions transfer
   !
   call IO_desc_duplicate(local_desc,BSS_desc)
   !
 endif
 !
 i_block=maxval(io_sec(ID,:))-1
 !
 if (i_block==0.and..not.frag_BS_K) then
   call def_variable_bulk(ID,"BSE_RESONANT",1,(/2,BS_IO_dim(1,1)/),SP)
   if (BS_K_coupling)           call def_variable_bulk(ID,"BSE_COUPLING",2,(/2,BS_IO_dim(1,2)/),SP)
   if (.not.l_BS_ares_from_res) call def_variable_bulk(ID,"BSE_ANTI-RESONANT",3,(/2,BS_IO_dim(1,3)/),SP)
   goto 1
 endif
 !
 if (i_block==n_BS_blks+1.and..not.frag_BS_K) goto 1
 !
 call io_fragment(ID,ID_frag,i_fragment=i_block)
 !
 if (BS_blk(i_block)%mode=="R") ch="BSE_RESONANT"
 if (BS_blk(i_block)%mode=="C") ch="BSE_COUPLING"
 if (BS_blk(i_block)%mode=="A") ch="BSE_ANTI-RESONANT"
 !
 if(frag_BS_K) then
   ch=trim(ch)//"_BLOCK_"//trim(intc(i_block))
   call def_variable_bulk(ID_frag,trim(ch),1,(/2,BS_blk(i_block)%size(1),BS_blk(i_block)%size(2)/),SP)
   call io_variable_bulk(ID_frag,1,C2=BS_blk(i_block)%mat)
 else
   if (BS_blk(i_block)%mode=="R") VAR_ID=1
   if (BS_blk(i_block)%mode=="C") VAR_ID=2
   if (BS_blk(i_block)%mode=="A") VAR_ID=3
   n1=BS_blk(i_block)%size(1)
   n2=BS_blk(i_block)%size(2)
   block_pos=1
   do j_block=1,i_block-1
     if (BS_blk(j_block)%mode/=BS_blk(i_block)%mode) cycle
     block_pos=block_pos+BS_blk(j_block)%size(1)*BS_blk(j_block)%size(2)
   enddo
   call C_F_POINTER(C_LOC(BS_blk(i_block)%mat),tmp_link,(/n1*n2/))
   call io_variable_bulk(ID_frag,VAR_ID,C1=tmp_link,IPOS=(/1,block_pos/))
   nullify(tmp_link)
 endif
 !
 call io_fragment_disconnect(ID,ID_frag)
 !
1 continue
 !
 ! If the database is not ok but the BS_identifier has been read I may have problems
 ! with the inversion database (like dimensions errors).
 !
 if (io_BS==0) BS_K_has_been_calculated_loaded=.TRUE.
 if (io_BS< 0) BS_K_has_been_calculated_loaded=.FALSE.
 !
 call io_disconnect(ID)
 !
 call timing('io_BS',OPR='stop')
 !
end function
