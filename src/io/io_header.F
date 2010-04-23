!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
integer function io_header(ID,QPTS,R_LATT,WF,IMPOSE_SN,T_EL,KPTS,D_LATT,XC_KIND,CUTOFF)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use units,         ONLY:HARTREE,Kelvin
 use drivers,       ONLY:list_dbs
 use LOGO,          ONLY:code_version,code_revision
 use com,           ONLY:msg,warning
 use stderr,        ONLY:string_split
 use D_lattice,     ONLY:Tel,a,alat,input_Tel_commands_over_header,Bose_Temp
 use electrons,     ONLY:n_sp_pol,n_spinor
 use R_lattice,     ONLY:nqibz,nqbz,nkibz,nkbz,q_pt,k_pt
#if defined _TEST_2
 use R_lattice,     ONLY:cut_description 
#endif
 use wave_func,     ONLY:wf_ng
 use vec_operate,   ONLY:v_is_zero
 use IO_m,          ONLY:io_elemental,io_bulk,io_code_version,io_status,&
&                        read_is_on,write_is_on,serial_number,&
&                        io_serial_number,io_file,io_com,io_mode,&
&                        DUMP,VERIFY,NONE,io_fragmented,Fragmented_IO,&
&                        ver_is_gt_or_eq,db_alat,io_code_revision
 use zeros,         ONLY:define_zeros
 use global_XC,     ONLY:G_E_xc_string,X_E_xc_string,K_E_xc_string,loaded_WF_xc_string,&
&                        G_WF_xc_string,X_WF_xc_string,K_WF_xc_string
 !
 implicit none
 integer               :: ID
 logical,     optional :: QPTS,R_LATT,WF,IMPOSE_SN,T_EL,KPTS,D_LATT,CUTOFF
 character(*),optional :: XC_KIND
 !
 ! Work Space
 !
 integer             :: i1,nqibz_disk,nkibz_disk,R_LATT_vec(4),&
&                       R_LATT_vec_disk(4),MODE,MENU,SPIN_vec_disk(2),VAR_SZ
 real(SP),allocatable:: l_pt(:,:)
 real(SP)            :: local_zero(3),D_LATT_vec_disk(3),D_LATT_vec(3),&
&                       save_Tel(2)
 logical             :: WARN,xc_kind_force
 character(schlen)   :: xc_string_kinds(10)
 !
 io_header=0
 !
 ! If I am scanning the DBs (yambo -D) not print warnings
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
 call io_elemental(ID,DB_L0=io_fragmented(ID),L0=Fragmented_IO)
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
     if (IMPOSE_SN) then
       io_header=-1
       if (io_com(ID)/=NONE) &
&        call warning('Incompatible serial number for '//trim(io_file(ID)))
       goto 1
     endif
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
&      call warning('Incompatible SPIN pol/spinors for '//trim(io_file(ID)))
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
     call define_zeros(vector_=q_pt,zero_=local_zero)
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
     call define_zeros(vector_=k_pt,zero_=local_zero)
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
 ! Cutoff
 !
#if defined _TEST_2
 if (present(CUTOFF)) then
   if (CUTOFF) then
     call io_elemental(ID,VAR="CUTOFF",CH0='',VAR_SZ=1,MENU=MENU)
     MENU=0
     call io_elemental(ID,CH0=cut_description,VAR=" Coulomb cutoff potential         :",WARN=WARN,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     io_header=io_status(ID)
     if (io_header/=0) goto 1
   endif
 endif
#endif 
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
   save_Tel=(/Tel,Bose_Temp/)
   if (ver_is_gt_or_eq(ID,revision=527).or.write_is_on(ID)) then
     call io_elemental(ID,VAR="TEMPERATURES",VAR_SZ=2,MENU=MENU)
     MENU=0
     call io_elemental(ID,R0=Tel,&
&         VAR=' Electronic Temperature        [K]:',UNIT=Kelvin,WARN=WARN,OP=(/"=="/))
     call io_elemental(ID,R0=Bose_Temp,&
&         VAR=' Bosonic    Temperature        [K]:',UNIT=Kelvin,WARN=WARN,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   else
     call io_elemental(ID,VAR="T_EL",VAR_SZ=1,MENU=MENU)
     MENU=0
     call io_elemental(ID,R0=Tel,&
&         VAR=' Electronic Temperature        [K]:',UNIT=Kelvin,WARN=WARN,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
   if (input_Tel_commands_over_header) then
     Tel      =save_Tel(1)
     Bose_Temp=save_Tel(2)
   endif
   !
   io_header=io_status(ID)
   if (io_header/=0) goto 1
   !
 endif
 !
 if (present(XC_KIND).and.ver_is_gt_or_eq(ID,(/3,0,15/)) ) then
   !
   call string_split(XC_KIND,xc_string_kinds)
   !
   io_mode(ID)=MODE
   !
   do i1=1,9
     xc_kind_force=trim(xc_string_kinds(i1+1))=='force'
     select case (trim(xc_string_kinds(i1)))
       case('G')
         call XC_E_KIND_io('G',G_E_xc_string,  ' Green`s function   energies      :')
         call XC_WF_KIND_io('G',G_WF_xc_string, '                    wavefunctions :')
       case('G_WF')
         call XC_WF_KIND_io('G',G_WF_xc_string,' Green`s function   wavefunctions :')
       case('Xx')
         call XC_E_KIND_io('Xx',X_E_xc_string(1),  ' Polariz. function  energies      :')
         call XC_WF_KIND_io('Xx',X_WF_xc_string(1), '                    wavefunctions :')
       case('Xs')
         call XC_E_KIND_io('Xs',X_E_xc_string(2),  ' Static diel. fun.  energies      :')
         call XC_WF_KIND_io('Xs',X_WF_xc_string(2), '                    wavefunctions :')
       case('Xp')
         call XC_E_KIND_io('Xp',X_E_xc_string(3),  ' PPA    diel. fun.  energies      :')
         call XC_WF_KIND_io('Xp',X_WF_xc_string(3), '                    wavefunctions :')
       case('Xd')
         call XC_E_KIND_io('Xd',X_E_xc_string(4),  ' Dyn. dielectric f. energies      :')
         call XC_WF_KIND_io('Xd',X_WF_xc_string(4), '                    wavefunctions :')
       case('K')
         call XC_E_KIND_io('K',K_E_xc_string,  ' BS kernel          energies      :')
         call XC_WF_KIND_io('K',K_WF_xc_string, '                    wavefunctions :')
       case('K_E')
         call XC_E_KIND_io('K',K_E_xc_string,  ' BS kernel          energies      :')
       case('K_WF')
         call XC_WF_KIND_io('K',K_WF_xc_string, ' BS kernel          wavefunctions :')
     end select
   enddo
   !
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif
 !
1 io_mode(ID)=MODE
 !
 contains
   !
   subroutine XC_E_KIND_io(VAR_,string_,desc_)
     !
     character(*)      :: VAR_,desc_
     character(lchlen) :: string_
     !
     call io_elemental(ID,VAR=VAR_//'_energies_xc_KIND',CH0="",VAR_SZ=1,MENU=MENU)
     MENU=0
     if (xc_kind_force) then
       call io_elemental(ID,CH0=string_,VAR=desc_,CHECK=WARN,OP=(/"=="/))
     else
       call io_elemental(ID,CH0=string_,VAR=desc_,WARN=WARN,OP=(/"=="/))
     endif
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
   end subroutine
   !
   subroutine XC_WF_KIND_io(VAR_,string_,desc_)
     !
     character(*)      :: VAR_,desc_
     character(lchlen) :: string_
     !
     call io_elemental(ID,VAR=VAR_//'_wavefunctions_xc_KIND',CH0="",VAR_SZ=1,MENU=MENU)
     MENU=0
     if (read_is_on(ID)) then
       if (IO_mode(ID)==DUMP) then
         call io_elemental(ID,CH0=string_,VAR=desc_)
       else
         if (xc_kind_force) then
           call io_elemental(ID,DB_CH0=string_,CH0=loaded_WF_xc_string,VAR=desc_,CHECK=WARN,OP=(/"=="/),DO_NOT_DUMP=.TRUE.)
         else
           call io_elemental(ID,DB_CH0=string_,CH0=loaded_WF_xc_string,VAR=desc_,WARN=WARN,OP=(/"=="/),DO_NOT_DUMP=.TRUE.)
         endif
       endif
     else
       call io_elemental(ID,CH0=loaded_WF_xc_string,VAR=desc_,WARN=WARN,OP=(/"=="/))
     endif
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
   end subroutine
   !
end function
