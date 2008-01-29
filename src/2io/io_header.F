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
integer function io_header(ID,QPTS,R_LATT,WF,IMPOSE_SN,QP_ctl_K,&
&                          T_EL,KPTS,D_LATT)
 !
 use pars,          ONLY:SP,HARTREE
 use drivers,       ONLY:list_dbs
 use com,           ONLY:code_version,msg,warning,code_revision
 use D_lattice,     ONLY:Tel,a,alat,input_Tel_fixed
 use electrons,     ONLY:n_sp_pol,n_spinor
 use R_lattice,     ONLY:nqibz,nqbz,nkibz,nkbz,q_pt,k_pt
 use wave_func,     ONLY:wf_ng
 use vec_operate,   ONLY:v_is_zero
 use QP_m,          ONLY:QP_ctl_E,QP_ctl_short_descr
 use IO_m,          ONLY:io_elemental,io_bulk,io_code_version,io_status,&
&                        read_is_on,write_is_on,serial_number,&
&                        io_serial_number,io_file,io_com,io_mode,&
&                        DUMP,VERIFY,NONE,io_fragmented,dbs_fragment,&
&                        ver_is_gt_or_eq,db_alat,io_code_revision
 use zeros,         ONLY:define_zeros
 !
 implicit none
 integer            :: ID
 logical,optional   :: QPTS,R_LATT,WF,IMPOSE_SN,QP_ctl_K,T_EL,KPTS,D_LATT
 !
 ! Work Space
 !
 integer             :: i1,nqibz_disk,nkibz_disk,R_LATT_vec(4),&
&                       R_LATT_vec_disk(4),MODE,MENU,SPIN_vec_disk(2)
 real(SP),allocatable:: l_pt(:,:)
 real(SP)            :: local_zero(3),D_LATT_vec_disk(3),D_LATT_vec(3),save_Tel
 logical             :: WARN
 !
 io_header=0
 !
 ! If I am scanning the DBs (self -D) not print warnings
 !
 WARN=.true.
 if (list_dbs) WARN=.false.
 !
 ! Things that CANNOT be dumped
 !==============================
 !
 MENU=1
 MODE=io_mode(ID)
 if (io_mode(ID)==DUMP) io_mode(ID)=VERIFY
 !
 call io_elemental(ID,VAR="FRAGMENTED",VAR_SZ=1,MENU=0)
 call io_elemental(ID,DB_L0=io_fragmented(ID),L0=dbs_fragment)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR="HEAD_VERSION",VAR_SZ=3,MENU=0)
 call io_elemental(ID,DB_I1=io_code_version(ID,:),I1=code_version)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 if (ver_is_gt_or_eq(ID,(/3,0,12/))) then
   call io_elemental(ID,VAR="HEAD_REVISION",VAR_SZ=1,MENU=0)
   call io_elemental(ID,DB_I0=io_code_revision(ID),I0=code_revision)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 else
   io_code_revision(ID)=0
 endif
 !
 call io_elemental(ID,VAR="SERIAL_NUMBER",VAR_SZ=1,MENU=0)
 call io_elemental(ID,DB_I0=io_serial_number(ID),I0=serial_number)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 if (ver_is_gt_or_eq(ID,(/3,0,8/))) then
   call io_elemental(ID,VAR="SPIN_VARS",VAR_SZ=2,MENU=0)
   call io_elemental(ID,DB_I1=SPIN_vec_disk,I1=(/n_sp_pol,n_spinor/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 ! Serial Number
 !
 if (io_serial_number(ID)/=serial_number) then
   if (present(IMPOSE_SN)) then 
     io_header=-1
     if (io_com(ID)/=NONE) &
&      call msg('sr','Incompatible serial number for '//trim(io_file(ID)))
     goto 1
   endif
   if (io_com(ID)/=NONE) then 
     call warning('Wrong serial number for '//trim(io_file(ID)))
   endif
 endif
 !
 ! SPIN polarizations/spinors
 !
 if (ver_is_gt_or_eq(ID,(/3,0,8/))) then
   if (SPIN_vec_disk(1)/=n_sp_pol.or.SPIN_vec_disk(2)/=n_spinor) then
     io_header=-1
     if (io_com(ID)/=NONE) &
&      call msg('sr','Incompatible SPIN pol/spinors for '//trim(io_file(ID)))
     goto 1
   endif
 endif
 !
 ! DL lattice
 !
 db_alat=alat
 !
 if (present(D_LATT).and.ver_is_gt_or_eq(ID,(/3,0,11/))) then
   D_LATT_vec=alat
   call io_elemental(ID,VAR="HEAD_D_LATT",VAR_SZ=3,MENU=MENU)
   MENU=0
   call io_elemental(ID,DB_R1=D_LATT_vec_disk,R1=D_LATT_vec,&
&       VAR=' Lattice constants                :',WARN=WARN,&
&       OP=(/"==","==","=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   db_alat=D_LATT_vec_disk
 endif
 !
 ! RL lattice
 !
 if (present(R_LATT)) then
   R_LATT_vec=(/nqibz,nqbz,nkibz,nkbz/)
   call io_elemental(ID,VAR="HEAD_R_LATT",VAR_SZ=4,MENU=MENU)
   MENU=0
   call io_elemental(ID,DB_I1=R_LATT_vec_disk,I1=R_LATT_vec,&
&       VAR=' Brillouin Zone Q/K grids (IBZ/BZ):',WARN=WARN,&
&       OP=(/"==","==","==","=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   nqibz_disk=R_LATT_vec_disk(1)
   nkibz_disk=R_LATT_vec_disk(3)
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif
 !
 ! Wave Functions RL vectors
 !
 if (present(WF)) then
   call io_elemental(ID,VAR="HEAD_WF",VAR_SZ=1,MENU=MENU)
   MENU=0
   call io_elemental(ID,I0=wf_ng,&
&       VAR=' RL vectors                   (WF):',WARN=WARN,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif
 !
 ! Q points
 !
 if (present(QPTS)) then
   !
   allocate(l_pt(nqibz_disk,3))
   !
   if (write_is_on(ID)) l_pt=q_pt
   !
   call io_bulk(ID,VAR="HEAD_QPT",VAR_SZ=(/nqibz_disk,3/))
   call io_bulk(ID,R2=l_pt)
   !
   if (read_is_on(ID).and.allocated(q_pt)) then
     !
     call define_zeros(q_pt(:,1),zero_=local_zero(1))
     call define_zeros(q_pt(:,2),zero_=local_zero(2))
     call define_zeros(q_pt(:,3),zero_=local_zero(3))
     !
     do i1=1,nqibz
       if (.not.v_is_zero(q_pt(i1,:)-l_pt(i1,:),zero_=local_zero)) io_header=-1
     enddo
     if (io_header/=0) then
       if (WARN) call warning('Wrong Q-points coordinates/order')
       goto 1
     endif
   endif
   !
   deallocate(l_pt)
 endif
 !
 ! K points
 !
 if (present(KPTS).and.ver_is_gt_or_eq(ID,(/3,0,9/)) ) then
   !
   allocate(l_pt(nkibz_disk,3))
   !
   if (write_is_on(ID)) l_pt=k_pt
   !
   call io_bulk(ID,VAR="HEAD_KPT",VAR_SZ=(/nkibz_disk,3/))
   call io_bulk(ID,R2=l_pt)
   !
   if (read_is_on(ID).and.allocated(k_pt)) then
     !
     call define_zeros(k_pt(:,1),zero_=local_zero(1))
     call define_zeros(k_pt(:,2),zero_=local_zero(2))
     call define_zeros(k_pt(:,3),zero_=local_zero(3))
     !
     do i1=1,nkibz
       if (.not.v_is_zero(k_pt(i1,:)-l_pt(i1,:),zero_=local_zero)) io_header=-1
     enddo
     if (io_header/=0) then
       if (WARN) call warning('Wrong K-points coordinates/order')
       goto 1
     endif
   endif
   !
   deallocate(l_pt)
 endif
 !
 ! Things that can be dumped
 !===========================
 !
 ! Temperature
 !
 if (present(T_EL).and.ver_is_gt_or_eq(ID,(/3,0,2/)) ) then
   !
   io_mode(ID)=MODE
   !
   call io_elemental(ID,VAR="T_EL",VAR_SZ=1,MENU=MENU)
   MENU=0
   save_Tel=Tel
   call io_elemental(ID,R0=Tel,&
&       VAR=' Electronic Temperature       [ev]:',UNIT=HARTREE,WARN=WARN,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   if (input_Tel_fixed) Tel=save_Tel
   !
   io_header=io_status(ID)
   if (io_header/=0) goto 1
   !
 endif
 !
 ! Kernel QP corrections
 !
 if (present(QP_ctl_K)) then
   !
   io_mode(ID)=MODE
   !
   if (ver_is_gt_or_eq(ID,(/3,0,10/))) then
     call io_elemental(ID,VAR="QP_CTL_K_E",VAR_SZ=3,MENU=MENU)
   else
     call io_elemental(ID,VAR="QP_CTL_K",VAR_SZ=3,MENU=MENU)
   endif
   MENU=0
   call io_elemental(ID,R1=QP_ctl_E(2,:),&
&       VAR=' QPs (scissor) :',CHECK=WARN,OP=(/"==","==","=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   if (ver_is_gt_or_eq(ID,(/3,0,10/))) then
     !
     call io_elemental(ID,VAR='QP_CTL_K_db_desc',CH0="",VAR_SZ=1,MENU=0)
     call io_elemental(ID,CH0=QP_ctl_short_descr(2),VAR=' QPs (db)      :',&
&                      CHECK=WARN,OP=(/"==","==","=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
   endif
   !
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif
 !
1 io_mode(ID)=MODE
 !
end function
