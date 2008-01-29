!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
integer function ioBS(iq,X,ID)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use X_m,           ONLY:X_t
 use R_lattice,     ONLY:nXkibz
 use stderr,        ONLY:intc
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use BS,            ONLY:BS_mat,BS_K_dim,BS_K_coupling,&
&                        BS_blk_dim,BS_blk_coord,BS_res_mode,cumulative_index,BS_blocks,&
&                        BS_DB_is_fragmented,&
&                        BS_cpl_mat,BSS_description,BSS_n_descs,variables_BS
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_elemental,io_bulk,io_restart,&
&                        read_is_on,write_is_on,io_check_restart,io_header,io_mode,&
&                        VERIFY,io_fragment,io_fragmented,dbs_fragment,&
&                        ver_is_gt_or_eq,&
&                        io_save_to_try_again,io_restore_to_try_again
 implicit none
 type(X_t):: X 
 integer  :: iq,ID
 !
 ! Work Space
 !
 integer             ::ik12(2),i_pointer,i1
 character(schlen)   ::ch,db_name
 character(lchlen)   ::local_description(100)
 real(SP),allocatable::BS_mat_disk(:,:,:)
 logical             ::db_is_large
 !
 db_name='BS_Q'//trim(intc(iq))
 !
 call io_save_to_try_again(ID)
 ioBS=io_connect(desc=trim(db_name),type=2,ID=ID)
 !
 ! The BS DB is searched in the SAVE folder when 
 ! it is not found in the JOBNAME folder
 !
 if (read_is_on(ID).and.ioBS/=0) then
   call io_restore_to_try_again(ID)
   ioBS=io_connect(desc=trim(db_name),type=1,ID=ID)
 endif 
 if (ioBS/=0) goto 1
 !
 call io_check_restart(trim(db_name),ID)
 !
 ! If the DB size > 1.9Gb  I automatically switch on the fragmentation.
 ! To do so both dbs_fragment (overall fragmentation) and 
 ! io_fragmented(ID) (fragmentation for this DB) must be set true.
 ! This is because io_fragmented(ID) is defined in reading but not
 ! in writing where it keep the default (dbs_fragment) value.
 !
 ! Note that io_connect supports the ENABLE_LARGE_FILE logical when
 ! using NETCDF support.
 !
 ! Update [27/6/07]: NETCDF performance increase drammatically
 ! when using fragmentation. So I lowered the treshold to enable 
 ! fragmentation to matrices >= 6000x6000
 !
 if (write_is_on(ID)) then
   db_is_large=BS_K_dim>6000
   if (BS_K_coupling) db_is_large=BS_K_dim>3000
   if (db_is_large) then
     dbs_fragment=.true.
     io_fragmented(ID)=.true.
   endif
 endif
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ioBS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.)
   if (ioBS/=0) goto 1
   !
   ! v 3.0.5: Inclusion of %f_xc 
   !
   if (.not.ver_is_gt_or_eq(ID,(/3,0,5/))) then
     ioBS=-1
     goto 1
   endif
   !
   BS_DB_is_fragmented=io_fragmented(ID)
   !
   ! MODE
   !
   if (.not.ver_is_gt_or_eq(ID,(/3,0,12/))) then
     call io_elemental(ID,VAR="MODE",VAR_SZ=1,CH0="",MENU=0)
     call io_elemental(ID,CH0=BS_res_mode)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
   ioBS=variables_BS(ID,local_description,'BSK',X=X)
   !
   if (ioBS/=0) then
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
 ! Section 1 is used in VERIFY mode.
 ! Now that the menu is closed I can return with ioBS/=0 if
 ! there is a restart point
 !
 ! Cerfull. As io_restart is the restart point it is +1 of the
 ! one written in the RESTART directory
 !
 if (io_restart(ID)>0.and.io_mode(ID)==VERIFY) then
   ioBS=io_restart(ID)-1
   BS_blk_coord=BS_blocks(ioBS)
 endif
 !
 i_pointer=maxval(io_sec(ID,:))
 ik12=BS_blocks(i_pointer)
 if (i_pointer==1) goto 1
 !
 ! Fragmentation
 !
 ! io_fragmented is read in the header. While accessing the fragments
 ! io_fragmented is not defined and must be overwritten with BS_DB_is_fragmented
 !
 io_fragmented(ID)=BS_DB_is_fragmented
 call io_fragment(ID,i_pointer=ik12(1),j_pointer=ik12(2))
 !
 allocate(BS_mat_disk(BS_blk_dim(ik12(1)),BS_blk_dim(ik12(2)),2))
 !
 ! Resonant I/O
 !
 write (ch,'(a,i4.4,a,i4.4)') "BS_RES_KERNEL_BLK_K1_",ik12(1),"_K2_",ik12(2)
 call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/BS_blk_dim(ik12(1)),BS_blk_dim(ik12(2)),2/))
 if (write_is_on(ID)) call mat_c2r(BS_mat(:,:),BS_mat_disk)
 call io_bulk(ID,R3=BS_mat_disk)
 if (read_is_on(ID)) call mat_r2c(BS_mat_disk,&
&   BS_mat(BS_blk_coord(1)+1:BS_blk_coord(1)+BS_blk_dim(ik12(1)),&
&          BS_blk_coord(2)+1:BS_blk_coord(2)+BS_blk_dim(ik12(2))))
 !
 ! Coupling I/O
 !
 if (BS_K_coupling) then
   write (ch,'(a,i4.4,a,i4.4)') "BS_CPL_KERNEL_BLK_K1_",ik12(1),"_K2_",ik12(2)
   call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/BS_blk_dim(ik12(1)),BS_blk_dim(ik12(2)),2/))
   !
   if (write_is_on(ID)) call mat_c2r(BS_cpl_mat,BS_mat_disk)
   !
   call io_bulk(ID,R3=BS_mat_disk)
   !
   if (read_is_on(ID)) then
     if (BS_DB_is_fragmented)      call mat_r2c(BS_mat_disk,BS_cpl_mat)
     if (.not.BS_DB_is_fragmented) call mat_r2c(BS_mat_disk,&
&     BS_mat(BS_blk_coord(1)+1:BS_blk_coord(1)+BS_blk_dim(ik12(1)),&
&            BS_K_dim+BS_blk_coord(2)+1:BS_K_dim+BS_blk_coord(2)+BS_blk_dim(ik12(2))))
   endif
   !
 end if
 !
 deallocate(BS_mat_disk)
 !
 call io_check_restart(trim(db_name),ID,now=i_pointer,todo=cumulative_index(1,nXkibz))
 ioBS=io_restart(ID)
 !
1 call io_disconnect(ID)
 !
end function
