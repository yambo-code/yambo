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
 integer function ioQP(what,qp,ID)
 !
 use pars,           ONLY:SP,schlen
 use D_lattice,      ONLY:alat
 use R_lattice,      ONLY:nqbz
 use par_proc_m,     ONLY:myid
 use memory_m,       ONLY:mem_est
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use QP_m,           ONLY:QP_t,QP_state,QP_nk,QP_nb,QP_dSc_steps,&
&                         QP_n_G_bands,QP_cg_percent,QP_n_states,QP_W,&
&                         QP_n_W_freqs,QP_W_dr,QP_W_er,QP_W_partially_done,&
&                         GWo_iterations
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,io_header,&
&                         io_elemental,io_status,io_bulk,read_is_on,&
&                         write_is_on,io_mode,DUMP,VERIFY,&
&                         io_fragment,io_check_restart,io_restart,&
&                         io_netcdf_support,ver_is_gt_or_eq,db_alat
 !
 implicit none
 type(QP_t)  ::qp
 character(*)::what 
 !             'QP'/'W'
 integer     ::ID
 !
 ! Work Space
 !
 integer              ::i1,i2,iqbz,QP_n_G_bands_disk(2),i_found,&
&                       QP_nk_disk,QP_nb_disk,QP_n_states_disk,n_descs_disk
 real(SP),allocatable ::qp_E_Eo_Z_disk(:,:,:),QP_W_disk(:,:,:)
 integer ,allocatable ::qp_table_disk(:,:)
 character(schlen)    ::ch
 character(2)         ::OP(1)
 !
 if (what=="QP".or.what=="W") then
   ioQP=io_connect(desc=what,type=2,ID=ID)
 else
   ioQP=io_connect(desc=what,type=-2,ID=ID)
 endif
 if (ioQP/=0) goto 1
 !
 ! Check the restart point 
 !  
 if (what=="W") call io_check_restart("W",ID)
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   if (what=="W") then
     ioQP=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,T_EL=.true.,D_LATT=.true.)
   else
     ioQP=io_header(ID,T_EL=.true.,D_LATT=.true.)
   endif
   if (ioQP/=0) goto 1
   !
   if (ver_is_gt_or_eq(ID,(/3,0,1/)).and.what/="W") then
     call io_elemental(ID,VAR="PARS",VAR_SZ=5,MENU=0)
   else
     call io_elemental(ID,VAR="PARS",VAR_SZ=4,MENU=0)
   endif
   !
   if (what/="W") OP="<="
   if (what=="W" ) OP="=="
   !
   call io_elemental(ID,I0=qp%nb,DB_I0=QP_nb_disk,CHECK=.true.,OP=OP)
   call io_elemental(ID,I0=qp%nk,DB_I0=QP_nk_disk,CHECK=.true.,OP=OP)
   call io_elemental(ID,I0=qp%n_states,DB_I0=QP_n_states_disk,&
&       VAR=' QP tot states          :',CHECK=.true.,OP=OP)
   !
   if (ver_is_gt_or_eq(ID,(/3,0,1/)).and.what/="W") &
&    call io_elemental(ID,I0=GWo_iterations,&
&       VAR=' GW SC iterations       :',CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(ID,I0=qp%n_descs,DB_I0=n_descs_disk,CHECK=.true.,OP=OP)
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   do i1=1,n_descs_disk
     write (ch,'(a,i5.5)') 'DESC_strings_',i1
     call io_elemental(ID,VAR=trim(ch),CH0="",VAR_SZ=1,MENU=0)
     !
     ! if db.W I don't check the lines
     !
     ! - GW solver       
     ! - dS/dw steps
     ! - dS/dw step
     ! - Sx RL components
     ! - QP
     !
     ! In any case the QP lines are skipped. The QP_state check is
     ! used instead. Also the GW SC iterations are skipped, as
     ! they are written before.
     !
     if (index(qp%description(i1),'GW SC')>0) then
       call io_elemental(ID,CH0=qp%description(i1))
       cycle
     endif
     !
     if (what=="W") then
       if (index(qp%description(i1),'QP')>0) then
          call io_elemental(ID,CH0=qp%description(i1))
       else if (index(qp%description(i1),'GW solver')>0.or.&
&               index(qp%description(i1),'dS/dw step')>0.or.&
&               index(qp%description(i1),'Sx RL components')>0) then
         call io_elemental(ID,CH0=qp%description(i1))
       else
         call io_elemental(ID,CH0=qp%description(i1),VAR='',CHECK=.true.,OP=(/"=="/)) 
       endif
     else
       if (index(qp%description(i1),'QP')>0) then
         call io_elemental(ID,CH0=qp%description(i1),VAR='')
       else
         call io_elemental(ID,CH0=qp%description(i1),VAR='',CHECK=.true.,OP=(/"=="/)) 
       endif
     endif
     !
     if (i1<n_descs_disk) call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     if (i1==n_descs_disk) call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   enddo
   !
   ioQP=io_status(ID)
   if (ioQP/=0) goto 1
   !
   ! The table is used to check the requested QP states with the disk ones.
   ! Note that I do not extract the W m.e. for a subset of Qp states
   ! (too complicated) while I check in the QP case that the states 
   ! reqeusted have been not already done.
   !
   ! So to be here in VERIFY mode you MUST have allocated and defined the qp%table 
   !
   allocate(qp_table_disk(qp_n_states_disk,3))
   !
   if (write_is_on(ID)) qp_table_disk=qp%table
   !
   call io_bulk(ID,VAR="QP_table",VAR_SZ=(/qp_n_states_disk,3/))
   call io_bulk(ID,I2=qp_table_disk)
   !
   if (io_mode(ID)==VERIFY.and.associated(qp%table)) then
     do i1=1,qp%n_states
       i_found=-1
       do i2=1,qp_n_states_disk
         if (all((/qp_table_disk(i2,:)==qp%table(i1,:)/))) then
           i_found=0
         endif
       enddo
       if (i_found/=0) io_status(ID)=-1
     enddo
   endif
   !
   ! From the W db I can extract the QP_table. Not from the QP db that 
   ! can be used with different ns* dbs'. IN this latter case I 
   ! only define the qp%table component.
   !
   if (io_mode(ID)==DUMP) then
     if (what=="W") then
       if (allocated(QP_state)) deallocate(QP_state)
       QP_nb=QP_nb_disk
       QP_nk=QP_nk_disk
       allocate(QP_state(QP_nb,QP_nk))
       QP_state=.false.
       do i1=1,QP_n_states_disk
         QP_state(qp_table_disk(i1,1),qp_table_disk(i1,3))=.true.
       enddo
     else
       allocate(qp%table(qp_n_states_disk,3))
       qp%table=qp_table_disk
     endif
   endif
   !
   deallocate(qp_table_disk)
   !
   ioQP=io_status(ID)
   if (ioQP/=0) goto 1
   !
 endif
 !
 ! Sections 1 & 2 are used in VERIFY mode.
 ! Now that the menu is closed I can return with ioQP/=0 if
 ! there is a restart point
 !
 if (what=="W") then
   ioQP=io_restart(ID)
   if (ioQP/=0.and.io_netcdf_support(ID)) then
     QP_W_partially_done=.true.
     goto 1
   endif
 endif
 !
 if (any((/io_sec(ID,:)==3/)).and.what/="W") then
   !
   ! I arrive here only in DUMP mode as in qp_solver I use only sections
   ! 1 and 2 to VERIFY. IN case everything is fine I do not load the 
   ! corrections.
   !
   call io_bulk(ID,VAR="QP_kpts",VAR_SZ=(/qp%nk,3/))
   !
   if (.not.associated(qp%k)) allocate(qp%k(qp%nk,3))
   call io_bulk(ID,R2=qp%k)
   if (read_is_on(ID)) then
     do i1=1,qp%nk
       qp%k(i1,:)=qp%k(i1,:)/db_alat(:)*alat(:)
     enddo
   endif
   !
   allocate(qp_E_Eo_Z_disk(3,qp%n_states,2))
   !  
   if (write_is_on(ID)) then
     do i1=1,qp%n_states
       qp_E_Eo_Z_disk(1,i1,1)=real(qp%E(i1))
       qp_E_Eo_Z_disk(1,i1,2)=aimag(qp%E(i1))
       qp_E_Eo_Z_disk(2,i1,1)=qp%E_bare(i1)
       qp_E_Eo_Z_disk(2,i1,2)=0.
       qp_E_Eo_Z_disk(3,i1,1)=real(qp%Z(i1))
       qp_E_Eo_Z_disk(3,i1,2)=aimag(qp%Z(i1))
     enddo
   endif
   !
   call io_bulk(ID,VAR="QP_E_Eo_Z",VAR_SZ=(/3,qp%n_states,2/))
   !
   call io_bulk(ID,R3=qp_E_Eo_Z_disk)
   !
   if (read_is_on(ID)) then
     allocate(qp%Z(qp%n_states),qp%E(qp%n_states),qp%E_bare(qp%n_states))
     call mem_est("qp_Z qp_E qp_E_bare",(/qp%n_states,qp%n_states,qp%n_states/))
     do i1=1,qp%n_states
       qp%E(i1) =cmplx(qp_E_Eo_Z_disk(1,i1,1),qp_E_Eo_Z_disk(1,i1,2),SP)
       qp%E_bare(i1)=cmplx(qp_E_Eo_Z_disk(2,i1,1),qp_E_Eo_Z_disk(2,i1,2),SP)
       qp%Z(i1) =cmplx(qp_E_Eo_Z_disk(3,i1,1),qp_E_Eo_Z_disk(3,i1,2),SP)
     enddo
   endif
   !
   deallocate(qp_E_Eo_Z_disk)
   !
   ioQP=io_status(ID)
   if (ioQP/=0) goto 1
   !
 endif
 !
 iqbz=1
 do i1=1,size(io_sec(ID,:))
   if (io_sec(ID,i1)/=0) iqbz=max(io_sec(ID,i1)-2,1)
 enddo
 !
 if (any((/io_sec(ID,:)==2+iqbz/)).and.what=="W") then
   ! 
   ! Fragmentation
   ! 
   call io_fragment(ID,i_pointer=iqbz)
   !
   write (ch,'(a,i5.5)') "W_PARS_",iqbz
   !
   call io_elemental(ID,VAR=trim(ch),VAR_SZ=7,MENU=0)
   !
   ! No checks as they are done using the %description 
   !
   call io_elemental(ID,I1=QP_n_G_bands,DB_I1=QP_n_G_bands_disk)
   call io_elemental(ID,I0=QP_n_W_freqs,DB_I0=QP_n_W_freqs)
   call io_elemental(ID,R1=QP_W_er,DB_R1=QP_W_er)
   call io_elemental(ID,R1=QP_W_dr,DB_R1=QP_W_dr)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   allocate(qp_W_disk(QP_n_states,QP_n_G_bands(2),2))
   !
   write (ch,'(a,i5.5)') "W_Q_",iqbz
   !
   call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/QP_n_states,QP_n_G_bands_disk(2),2,QP_n_W_freqs/))
   !
   do i1=1,QP_n_W_freqs
     !
     if (write_is_on(ID)) call mat_c2r(QP_W(:,:,i1),qp_W_disk)
     !
     call io_bulk(ID,R3=qp_W_disk,IPOS=(/1,1,1,i1/))
     !
     if (read_is_on(ID)) call mat_r2c(qp_W_disk,QP_W(:,:,i1))
     !
   enddo
   !
   deallocate(qp_W_disk)
   !
   ioQP=io_status(ID)
   if (ioQP/=0) goto 1
   !
   call io_check_restart("W",ID,now=iqbz,todo=nqbz)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
 end function
