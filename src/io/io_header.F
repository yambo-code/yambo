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
integer function io_header(ID,QPTS,R_LATT,WF,IMPOSE_SN,T_EL,KPTS,D_LATT,XC_KIND,&
&                          CUTOFF,GAUGE,IMPOSE_GAUGE,PARALLEL_STRUCTURE,FRAG,IMPOSE,NOWARN_SER)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use units,         ONLY:HA2KEL
 use X_m,           ONLY:global_gauge 
 use drivers,       ONLY:list_dbs
 use C_driver,      ONLY:code_version,code_revision,code_GPL_revision
 use stderr,        ONLY:STRING_split
 use D_lattice,     ONLY:Tel,alat,input_Tel_is_negative,Bose_Temp
 use electrons,     ONLY:n_sp_pol,n_spinor
 use parallel_m,    ONLY:CPU_structure
 use R_lattice,     ONLY:nqibz,nqbz,nkibz,nkbz,q_pt,k_pt,cut_description
 use wave_func,     ONLY:wf_ng
 use vec_operate,   ONLY:v_is_zero
 use IO_m,          ONLY:io_elemental,io_bulk,io_code_version,io_status,&
&                        read_is_on,write_is_on,serial_number,variable_exist,&
&                        io_serial_number,io_file,io_com,io_mode,&
&                        DUMP,VERIFY,NONE,ver_is_gt_or_eq,&
&                        db_alat,io_code_revision,read_is_on
 use zeros,         ONLY:define_zeros
 use global_XC,     ONLY:G_E_xc_string,X_E_xc_string,K_E_xc_string,loaded_WF_xc_string,&
&                        G_WF_xc_string,X_WF_xc_string,K_WF_xc_string
 !
 implicit none
 !
 integer               :: ID
 logical,     optional :: QPTS,R_LATT,WF,IMPOSE_SN,T_EL,KPTS,D_LATT,CUTOFF,GAUGE,IMPOSE_GAUGE,FRAG,IMPOSE,NOWARN_SER
 character(*),optional :: XC_KIND,PARALLEL_STRUCTURE
 !
 ! Work Space
 !
 integer             :: i1,nqibz_disk,nkibz_disk,R_LATT_vec(4),p_structure,&
&                       R_LATT_vec_disk(4),MODE,MENU,SPIN_vec_disk(2)
 integer,    external:: io_out_of_date
 real(SP),allocatable:: l_pt(:,:)
 real(SP)            :: local_zero(3),D_LATT_vec_disk(3),D_LATT_vec(3),&
&                       save_Tel(2)
 logical             :: WARN,WARN_SERIAL,xc_kind_force,l_IMPOSE,l_R_LATT
 character(schlen)   :: xc_string_kinds(10),PARALLEL_STRUCTURE_
 !
 io_header=0
 !
 ! If I am scanning the DBs (yambo -D) not print warnings
 !
 WARN=.true.
 if (list_dbs) WARN=.false.
 !
 WARN_SERIAL=.true.
 if(present(NOWARN_SER)) WARN_SERIAL=.not.NOWARN_SER
 !
 l_IMPOSE=.false.
 if (present(IMPOSE)) l_IMPOSE=IMPOSE
 !
 l_R_LATT=.false.
 if (present(R_LATT)) l_R_LATT=R_LATT
 !
 MENU=1
 MODE=io_mode(ID)
 if (io_mode(ID)==DUMP) io_mode(ID)=VERIFY
 !
 ! Things that CANNOT be dumped
 !==============================
 call io_elemental(ID,VAR="HEAD_VERSION",VAR_SZ=3,MENU=0)
 call io_elemental(ID,DB_I1=io_code_version(ID,:),I1=code_version)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR="HEAD_REVISION",VAR_SZ=1,MENU=0)
 call io_elemental(ID,DB_I0=io_code_revision(ID,1),I0=code_revision)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 if (ver_is_gt_or_eq(ID,revision=4350).and.variable_exist(ID,"HEAD_GPL_REVISION")) then
   call io_elemental(ID,VAR="HEAD_GPL_REVISION",VAR_SZ=1,MENU=0)
   call io_elemental(ID,DB_I0=io_code_revision(ID,2),I0=code_GPL_revision)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 else
   io_code_revision(ID,2)=code_GPL_revision
 endif
 !
 ! Check if the DB revision is too old and not supported anymore
 !
 io_header=io_out_of_date(ID)
 if (io_header/=0) then
   call warning(trim(io_file(ID))//' is a no longer supported database')
   goto 1
 endif
 !
 call io_elemental(ID,VAR="SERIAL_NUMBER",VAR_SZ=1,MENU=0)
 call io_elemental(ID,DB_I0=io_serial_number(ID),I0=serial_number,WARN=WARN_SERIAL)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR="SPIN_VARS",VAR_SZ=2,MENU=0)
 call io_elemental(ID,DB_I1=SPIN_vec_disk,I1=(/n_sp_pol,n_spinor/))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
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
   if (WARN_SERIAL.and.io_com(ID)/=NONE) then 
     call warning('Wrong serial number for '//trim(io_file(ID)))
   endif
 endif
 !
 ! SPIN polarizations/spinors
 !
 if (SPIN_vec_disk(1)/=n_sp_pol.or.SPIN_vec_disk(2)/=n_spinor) then
   io_header=-1
   if (io_com(ID)/=NONE) call warning('Incompatible SPIN pol/spinors for '//trim(io_file(ID)))
   goto 1
 endif
 !
 ! DL lattice
 !
 db_alat=alat
 !
 if (present(D_LATT)) then
   D_LATT_vec=alat
   call io_elemental(ID,VAR="HEAD_D_LATT",VAR_SZ=3,MENU=MENU)
   MENU=0
   call io_elemental(ID,DB_R1=D_LATT_vec_disk,R1=D_LATT_vec,&
&       VAR='Lattice constants                ',WARN=WARN,CHECK=l_IMPOSE,&
&       OP=(/"==","==","=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   db_alat=D_LATT_vec_disk
 endif
 !
 ! RL lattice
 !
 if (l_R_LATT) then
   R_LATT_vec=(/nqibz,nqbz,nkibz,nkbz/)
   call io_elemental(ID,VAR="HEAD_R_LATT",VAR_SZ=4,MENU=MENU)
   MENU=0
   call io_elemental(ID,DB_I1=R_LATT_vec_disk,I1=R_LATT_vec,&
&       VAR='Brillouin Zone Q/K grids (IBZ/BZ)',WARN=WARN,CHECK=l_IMPOSE,&
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
&       VAR='RL vectors',TERMINATOR='WF',WARN=WARN,CHECK=l_IMPOSE,OP=(/"=="/))
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
       if (WARN    ) call warning('Wrong Q-points coordinates/order')
       if (l_IMPOSE) call   error('Wrong Q-points coordinates/order')
       goto 1
     endif
   endif
   !
   deallocate(l_pt)
 endif
 !
 ! K points
 !
 if (present(KPTS)) then
   !
   allocate(l_pt(nkibz_disk,3))
   !
   if (write_is_on(ID)) l_pt=k_pt
   !
   call io_bulk(ID,VAR="HEAD_KPT",VAR_SZ=(/nkibz_disk,3/))
   call io_bulk(ID,R2=l_pt)
   !
   if (read_is_on(ID).and.allocated(k_pt).and.KPTS) then
     !
     call define_zeros(vector_=k_pt,zero_=local_zero)
     !
     do i1=1,nkibz
       if (.not.v_is_zero(k_pt(i1,:)-l_pt(i1,:),zero_=local_zero)) io_header=-1
     enddo
     if (io_header/=0) then
       if (WARN    ) call warning('Wrong K-points coordinates/order')
       if (l_IMPOSE) call warning('Wrong K-points coordinates/order')
       goto 1
     endif
     !
   endif
   !
   deallocate(l_pt)
   !
 endif
 !
 ! Cutoff
 !
 if (present(CUTOFF)) then
   if (CUTOFF) then
     call io_elemental(ID,VAR="CUTOFF",CH0='',VAR_SZ=1,MENU=MENU)
     MENU=0
     call io_elemental(ID,CH0=cut_description,VAR=" Coulomb cutoff potential          ",WARN=WARN,CHECK=l_IMPOSE,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     io_header=io_status(ID)
     if (io_header/=0) goto 1
   endif
 endif
 !
 PARALLEL_STRUCTURE_="NONE"
 if (present(PARALLEL_STRUCTURE)) PARALLEL_STRUCTURE_=trim(PARALLEL_STRUCTURE)

 if (trim(PARALLEL_STRUCTURE_)/="NONE") then
   ! 
   if (trim(PARALLEL_STRUCTURE)=="DIPOLES")          p_structure=1
   if (trim(PARALLEL_STRUCTURE)=="Response_G_space_Finite_Momentum") p_structure=2
   if (trim(PARALLEL_STRUCTURE)=="Response_G_space") p_structure=3
   if (trim(PARALLEL_STRUCTURE)=="Response_T_space") p_structure=4
   if (trim(PARALLEL_STRUCTURE)=="Self_Energy")      p_structure=5
   if (trim(PARALLEL_STRUCTURE)=="Real_Time")        p_structure=6
   if (trim(PARALLEL_STRUCTURE)=="ScaLapacK") p_structure=7
   if (trim(PARALLEL_STRUCTURE)=="Non_Linear") p_structure=8
   !
   call io_elemental(ID,VAR="PARALLEL_CPUs",CH0='',VAR_SZ=1,MENU=MENU)
   call io_elemental(ID,CH0=CPU_structure(p_structure)%CPU_string,&
&                    VAR='Parallel CPUs                    ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   io_header=io_status(ID)
   call io_elemental(ID,VAR="PARALLEL_ROLES",CH0='',VAR_SZ=1,MENU=MENU)
   call io_elemental(ID,CH0=CPU_structure(p_structure)%ROLE_string,&
&                    VAR='Parallel Roles                   ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif
 !
 ! Things that can be dumped
 !===========================
 !
 ! Fragmentation
 !
 if (present(FRAG)) then
   !
   io_mode(ID)=MODE
   !
   call io_elemental(ID,VAR="FRAGMENTED",VAR_SZ=1,MENU=MENU)
   MENU=0
   call io_elemental(ID,L0=FRAG,&
&                    VAR='Fragmentation                    ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 ! Temperature
 !
 if (present(T_EL)) then
   !
   io_mode(ID)=MODE
   !
   save_Tel=(/Tel,Bose_Temp/)
   call io_elemental(ID,VAR="TEMPERATURES",VAR_SZ=2,MENU=MENU)
   MENU=0
   call io_elemental(ID,R0=Tel,&
&       VAR='Electronic Temperature',TERMINATOR='K',UNIT=HA2KEL,WARN=WARN,CHECK=l_IMPOSE,OP=(/"=="/))
   call io_elemental(ID,R0=Bose_Temp,&
&       VAR='Bosonic    Temperature',TERMINATOR='K',UNIT=HA2KEL,WARN=WARN,CHECK=l_IMPOSE,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   if (input_Tel_is_negative) then
     Tel      =save_Tel(1)
     Bose_Temp=save_Tel(2)
   endif
   !
   io_header=io_status(ID)
   if (io_header/=0) goto 1
   !
 endif
 !
 if (present(XC_KIND)) then
   !
   call STRING_split(XC_KIND,xc_string_kinds)
   !
   io_mode(ID)=MODE
   !
   do i1=1,9
     xc_kind_force=trim(xc_string_kinds(i1+1))=='force'
     select case (trim(xc_string_kinds(i1)))
       case('G')
         call XC_KIND_io('E','G',G_E_xc_string,  ' Green`s function   energies      ')
         call XC_KIND_io('WF','G',G_WF_xc_string, '                    wavefunctions ')
       case('G_WF')
         call XC_KIND_io('WF','G',G_WF_xc_string,' Green`s function   wavefunctions ')
       case('Xx')
         call XC_KIND_io('E','Xx',X_E_xc_string(1),  ' Polariz. function  energies      ')
         call XC_KIND_io('WF','Xx',X_WF_xc_string(1), '                    wavefunctions ')
       case('Xs')
         call XC_KIND_io('E','Xs',X_E_xc_string(2),  ' Static diel. fun.  energies      ')
         call XC_KIND_io('WF','Xs',X_WF_xc_string(2), '                    wavefunctions ')
       case('Xp')
         call XC_KIND_io('E','Xp',X_E_xc_string(3),  ' PPA    diel. fun.  energies      ')
         call XC_KIND_io('WF','Xp',X_WF_xc_string(3), '                    wavefunctions ')
       case('Xd')
         call XC_KIND_io('E','Xd',X_E_xc_string(4),  ' Dyn. dielectric f. energies      ')
         call XC_KIND_io('WF','Xd',X_WF_xc_string(4), '                    wavefunctions ')
       case('K')
         call XC_KIND_io('E','K',K_E_xc_string,  ' BS kernel          energies      ')
         call XC_KIND_io('WF','K',K_WF_xc_string, '                    wavefunctions ')
       case('K_E')
         call XC_KIND_io('E','K',K_E_xc_string,  ' BS kernel          energies      ')
       case('K_WF')
         call XC_KIND_io('WF','K',K_WF_xc_string, ' BS kernel          wavefunctions ')
     end select
   enddo
   !
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif
 !
 ! GAUGE
 !
 if (present(GAUGE)) then
   call io_elemental(ID,VAR='GAUGE',CH0="",VAR_SZ=1,MENU=MENU)
   MENU=0
   if (present(IMPOSE_GAUGE)) then
     if (IMPOSE_GAUGE) then
       call io_elemental(ID,CH0=global_gauge,&
&           VAR='Global Gauge                     ',CHECK=.true.,OP=(/"=="/))
     else
       call io_elemental(ID,CH0=global_gauge,&
&           VAR='Global Gauge                     ',WARN=.true.,OP=(/"=="/))
     endif
   else
     call io_elemental(ID,CH0=global_gauge,&
&         VAR='Global Gauge                     ',WARN=.true.,OP=(/"=="/))
   endif
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif
 !
1 io_mode(ID)=MODE
 !
 contains
   !
   subroutine XC_KIND_io(what,VAR_,string_,desc_)
     !
     character(*)      :: what,VAR_,desc_
     character(lchlen) :: string_
     !
     if (what=="WF") call io_elemental(ID,VAR=VAR_//'_wavefunctions_xc_KIND',CH0="",VAR_SZ=1,MENU=MENU)
     if (what=="E" ) call io_elemental(ID,VAR=VAR_//'_energies_xc_KIND',CH0="",VAR_SZ=1,MENU=MENU)
     MENU=0
     if (read_is_on(ID)) then
       if (IO_mode(ID)==DUMP) then
         call io_elemental(ID,CH0=string_,VAR=desc_)
       else
         if (xc_kind_force) then
           call io_elemental(ID,CH0=string_,VAR=desc_,CHECK=WARN,OP=(/"=="/),DO_NOT_DUMP=.TRUE.)
         else
           call io_elemental(ID,CH0=string_,VAR=desc_,WARN=WARN,OP=(/"=="/),DO_NOT_DUMP=.TRUE.)
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
