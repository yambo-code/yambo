!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
integer function io_G_retarded(ID,what)
 !
 use pars,           ONLY:SP
 use units,          ONLY:FS2AUT,HA2EV
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_sec,io_elemental,&
&                         write_is_on,io_bulk,io_status
 use real_time,      ONLY:Gr,Gr_kind,Gr_bands,Gr_T_steps,Gr_T_window,Gr_E_steps,Gr_E_window,&
&                         Gr_mat_dim,Gr_ee_matrix,Gr_peak_pos,Gr_E_ranges,l_G_DB_has_el_ph,Gr_T_step,&
&                         Gr_QP_width,Gr_ep_matrix
 use SC,             ONLY:SC_bands
 use ELPH,           ONLY:ph_modes
 use R_lattice,      ONLY:nkibz,nqbz
 use stderr,         ONLY:intc
 !
 implicit none
 character(*)::what
 integer     ::ID
 !
 ! Work Space
 !
 integer              :: i_k,i_s,T_steps_each_section,T_section(2)
 real(SP),allocatable :: Gr_disk(:,:,:)
 !
 io_G_retarded=io_connect(desc=what,type=2,ID=ID)
 if (io_G_retarded/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_G_retarded=io_header(ID,XC_KIND="G")
   !
   call io_elemental(ID,VAR='G_retarded_KIND',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=Gr_kind,&
&       VAR=' Retarded Green`s function       :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=10,MENU=0)
   !
   if (what=="G_retarded") then
     call io_elemental(ID,I1=Gr_bands,&
&         VAR=' Bands                           :',CHECK=.true.,OP=(/"==","=="/))
   else
     call io_elemental(ID,I1=SC_bands,&
&         VAR=' Bands                           :',CHECK=.true.,OP=(/"==","=="/))
   endif
   !
   call io_elemental(ID,R0=Gr_QP_width,&
&       VAR=' QP damping                  [eV]:',UNIT=HA2EV,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R0=Gr_T_window,&
&       VAR=' Time range                  [fs]:',UNIT=1./FS2AUT,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R0=Gr_T_step,&
&       VAR=' Time step                   [fs]:',UNIT=1./FS2AUT,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=Gr_T_steps,&
&       VAR=' Time steps                      :',CHECK=.true.,OP=(/"=="/))
   !
   if (trim(Gr_kind)/="HC") then
     !
     call io_elemental(ID,R0=Gr_E_window,&
&         VAR=' Energy range                [eV]:',UNIT=HA2EV,CHECK=.true.,OP=(/"=="/))
     !
     call io_elemental(ID,I0=Gr_E_steps,&
&         VAR=' Energy steps                    :',CHECK=.true.,OP=(/"=="/))
     !
   endif
   !
   if (what=="G_retarded_matrix") then
     !
     call io_elemental(ID,L0=l_G_DB_has_el_ph,&
&         VAR=' Electron-Phonon submatrix       :',CHECK=.true.,OP=(/"=="/))
     !
   endif
   !
   call io_elemental(ID,VAR=" ",VAR_SZ=0)
   !
   io_G_retarded=io_status(ID)
   !
   if (io_G_retarded/=0) goto 1
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)==2/))) return
 !
 if (what=="G_retarded") then
   !
   T_steps_each_section=Gr_T_steps/10
   !
   allocate(Gr_disk(Gr_bands(2),T_steps_each_section,2))
   !
   do i_k=1,nkibz
     !
     do i_s=1,10
       !
       T_section=(/(i_s-1)*T_steps_each_section+1,min( i_s*T_steps_each_section,Gr_T_steps )/)
       !
       call io_bulk(ID,VAR="G_retarded_k"//trim(intc(i_k))//"_Tsec"//trim(intc(i_s)),VAR_SZ=(/Gr_bands(2),T_steps_each_section,2/))
       if(write_is_on(ID)) then
         Gr_disk(:,1:T_section(2)-T_section(1)+1,1) =real(Gr(i_k,:,T_section(1):T_section(2)))
         Gr_disk(:,1:T_section(2)-T_section(1)+1,2)=aimag(Gr(i_k,:,T_section(1):T_section(2)))
         call io_bulk(ID,R3=Gr_disk)
       else
         call io_bulk(ID,R3=Gr_disk)
         Gr(i_k,:,T_section(1):T_section(2))=CMPLX(Gr_disk(:,1:T_section(2)-T_section(1)+1,1),Gr_disk(:,1:T_section(2)-T_section(1)+1,2))
       endif
       !
     enddo
     !
   enddo
   !
   deallocate(Gr_disk)
   !
   call io_bulk(ID,VAR="Gr_Peak_Position",VAR_SZ=(/nkibz,Gr_bands(2)/))
   call io_bulk(ID,R2=Gr_peak_pos)
   !
   call io_bulk(ID,VAR="Gr_Energy_Ranges",VAR_SZ=(/nkibz,Gr_bands(2),2/))
   call io_bulk(ID,R3=Gr_E_ranges)
   !
 else if (what=="G_retarded_matrix") then
   !
   call io_bulk(ID,VAR="G_retarded_matrix_SIZE",VAR_SZ=(/1/))
   call io_bulk(ID,I0=Gr_mat_dim)
   !
   call io_bulk(ID,VAR="G_retarded_matrix",VAR_SZ=(/Gr_mat_dim,Gr_mat_dim/))
   call io_bulk(ID,R2=Gr_ee_matrix)
   !
   call io_bulk(ID,VAR="G_ep_retarded_matrix_1",VAR_SZ=(/Gr_mat_dim,nqbz,ph_modes/))
   call io_bulk(ID,R3=Gr_ep_matrix(:,:,:,1))
   !
   call io_bulk(ID,VAR="G_ep_retarded_matrix_2",VAR_SZ=(/Gr_mat_dim,nqbz,ph_modes/))
   call io_bulk(ID,R3=Gr_ep_matrix(:,:,:,2))
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function
