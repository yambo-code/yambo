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
 use pars,          ONLY:SP,schlen,HARTREE
 use X_m,           ONLY:X_t
 use R_lattice,     ONLY:nXkibz,RIM_ng,RIM_n_rand_pts,cut_geometry
 use stderr,        ONLY:intc
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use BS,            ONLY:BS_mat,BS_identifier,BS_K_dim,BS_bands,&
&                        BS_K_coupling,BS_res_K_exchange,BS_res_K_corr,&
&                        BS_n_g_exch,BS_n_g_W,BS_eh_en,BS_eh_win,BS_W_is_diagonal,&
&                        BS_blk_dim,BS_blk_coord,BS_res_mode,cumulative_index,BS_blocks,&
&                        BS_DB_is_fragmented,BS_K_is_ALDA,BS_cpl_K_exchange,&
&                        BS_cpl_K_corr,BS_cpl_mat
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_elemental,io_status,io_bulk,io_restart,&
&                        read_is_on,write_is_on,io_check_restart,io_header,io_mode,&
&                        VERIFY,io_fragment,io_fragmented,dbs_fragment,&
&                        ver_is_gt_or_eq
 use TDDFT,         ONLY:FXC_n_g_corr
 implicit none
 type(X_t):: X 
 integer  :: iq,ID
 !
 ! Work Space
 !
 integer             ::ik12(2),i_pointer,var_size
 character(schlen)   ::ch,db_name
 real(SP),allocatable::BS_mat_disk(:,:,:)
 logical             ::res_corr_disk,res_exch_disk,db_is_large,cpl_disk,&
&                      cpl_corr_disk,cpl_exch_disk
 !
 db_name='BS_Q'//trim(intc(iq))
 !
 ioBS=io_connect(desc=trim(db_name),type=2,ID=ID)
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
 ! Update [27/6/07]: NETCDF performance increase darammatically
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
   call io_elemental(ID,VAR="MODE",VAR_SZ=1,CH0="",MENU=0)
   call io_elemental(ID,CH0=BS_res_mode)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! PARS1
   !
   var_size=29
   if (ver_is_gt_or_eq(ID,(/3,0,9/))) var_size=30
   !
   call io_elemental(ID,VAR="PARS1",VAR_SZ=var_size,MENU=0)
   call io_elemental(ID,&
&       VAR=" BSK|Identifier              :",I0=BS_identifier)
   call io_elemental(ID,&
&       VAR="    |Dimension               :",I0=BS_K_dim,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR="    |Bands                   :",I1=BS_bands,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,DB_L0=res_exch_disk,&
&       VAR="    |Exchange           [res]:",L0=BS_res_K_exchange,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,DB_L0=res_corr_disk,&
&       VAR="    |Correlation        [res]:",L0=BS_res_K_corr,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,DB_L0=cpl_disk,&
&       VAR="    |Kernel`s coupling       :",L0=BS_K_coupling,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,DB_L0=cpl_exch_disk,&
&       VAR="    |Exchange           [cpl]:",L0=BS_cpl_K_exchange,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,DB_L0=cpl_corr_disk,&
&       VAR="    |Correlation        [cpl]:",L0=BS_cpl_K_corr,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR="    |ALDA kernel in R-space  :",L0=BS_K_is_ALDA,CHECK=.true.,OP=(/"=="/))
   !
   if (res_exch_disk.or.cpl_exch_disk) &
&   call io_elemental(ID,&
&       VAR="    |RL vectors    [exchange]:",I0=BS_n_g_exch,CHECK=.true.,OP=(/"=="/))
   if (res_corr_disk.or.cpl_corr_disk) &
&    call io_elemental(ID,&
&       VAR="    |RL vectors [correlation]:",I0=BS_n_g_W,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,UNIT=HARTREE,&
&       VAR="    |E/h energy range    [ev]:",R1=BS_eh_en,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,&
&       VAR="    |Coupling range     [o/o]:",R1=BS_eh_win,CHECK=.true.,OP=(/"==","=="/))
   if (res_corr_disk.or.cpl_corr_disk) then
     call io_elemental(ID,&
&         VAR="  W |Interaction is diagonal :",L0=BS_W_is_diagonal,CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,&
&         VAR="    |Matrix size             :",I0=X%ng,WARN=.true.,OP=(/"=="/))
     call io_elemental(ID,&
&         VAR="    |Bands                   :",I1=X%ib,CHECK=.true.,OP=(/"==","=="/))
     call io_elemental(ID,UNIT=HARTREE,&
&         VAR="    |e/h energy range    [ev]:",R1=X%ehe,CHECK=.true.,OP=(/">=","<="/))
     call io_elemental(ID,&
&         VAR="    |Poles              [o/o]:",R0=X%cg_percentual,CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,&
&         VAR="    |Rl vectors in the sum   :",I0=X%ngostnts,WARN=.true.,OP=(/"<="/))
     call io_elemental(ID,&
&         VAR="    |[r,Vnl] included        :",L0=X%Vnl_included,WARN=.true.,OP=(/"=="/))
     !
     if (ver_is_gt_or_eq(ID,(/3,0,9/))) then
       !
       ! v 3.0.8: Inclusion of %long_gauge 
       !
       call io_elemental(ID,&
&           VAR="    |Longitudinal Gauge      :",L0=X%long_gauge,WARN=.true.,OP=(/"=="/))
     endif
     !
     call io_elemental(ID,&
&         VAR="    |Field direction         :",R1=X%q0,CHECK=.true.,OP=(/"==","==","=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     if (ver_is_gt_or_eq(ID,(/3,0,11/))) then
       !
       ! v 3.0.11: Inclusion of cut_geometry
       !
       call io_elemental(ID,VAR="W_Cutoff",VAR_SZ=1,CH0="",MENU=0)
       call io_elemental(ID,&
&           VAR="    |Columbian Cutoff        :",CH0=cut_geometry,CHECK=.true.,OP=(/"=="/))
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     endif
     !
     ! W_TDDFT_KERNEL
     !
     call io_elemental(ID,VAR="W_TDDFT_KERNEL",VAR_SZ=1,CH0="",MENU=0)
     call io_elemental(ID,&
&       VAR='    |xc-Kernel               :',CH0=X%f_xc,CHECK=.true.)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     ! W_RIM
     !
     call io_elemental(ID,VAR="W_RIM",VAR_SZ=2,MENU=0)
     call io_elemental(ID,I0=RIM_ng,&
&       VAR=' RIM|RL components      [col]:',CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,I0=RIM_n_rand_pts,&
&       VAR='    |Random points      [col]:',CHECK=.true.,OP=(/"=="/))
   endif
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   ioBS=io_status(ID)
   if (ioBS/=0) goto 1
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
   if (write_is_on(ID)) call mat_c2r(BS_cpl_mat(:,:),BS_mat_disk)
   call io_bulk(ID,R3=BS_mat_disk)
   if (read_is_on(ID)) call mat_r2c(BS_mat_disk,&
&     BS_mat(BS_blk_coord(1)+1:BS_blk_coord(1)+BS_blk_dim(ik12(1)),&
&            BS_K_dim+BS_blk_coord(2)+1:BS_K_dim+BS_blk_coord(2)+BS_blk_dim(ik12(2))))
 endif
 deallocate(BS_mat_disk)
 !
 call io_check_restart(trim(db_name),ID,now=i_pointer,todo=cumulative_index(1,nXkibz))
 ioBS=io_restart(ID)
 !
1 call io_disconnect(ID)
 !
 end function
