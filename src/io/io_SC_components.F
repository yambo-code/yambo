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
integer function io_SC_components(what,E_sc,ID)
 !
 ! what can be E WF V
 !
 use pars,           ONLY:SP,HARTREE
 use R_lattice,      ONLY:nkibz
 use stderr,         ONLY:intc
 use QP_m,           ONLY:QP_band_mixing
 use SC,             ONLY:SC_bands,it_now,rho_convergence,E_convergence,SC_potential,&
&                         SC_R,V_xc_sc,local_V,H_nl_sc
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_elemental,io_status,&
&                         io_sec,io_bulk,io_fragment,read_is_on,write_is_on,&
&                         manage_RD_WR_CL_IF_END
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use electrons,      ONLY:levels,n_sp_pol
 use FFT_m,          ONLY:fft_size
 implicit none
 !
 integer               :: ID
 type(levels)          :: E_sc
 character(*)          :: what
 !
 ! Work Space
 !
 real(SP), allocatable :: R_H_disk(:,:,:)
 integer               :: ik
 logical               :: l_wf,l_V,l_E
 !
 l_E =what=="E"
 l_wf=what=="WF"
 l_V =what=="V"
 !
 if (l_wf) io_SC_components=io_connect(desc="scWFs",type=2,ID=ID)
 if (l_E)  io_SC_components=io_connect(desc="scE",type=2,ID=ID)
 if (l_V)  io_SC_components=io_connect(desc="scV",type=2,ID=ID)
 !
 if (io_SC_components/=0) goto 1
 !
 ik = maxval(io_sec(ID,:))-1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_SC_components=io_header(ID,IMPOSE_SN=.true.)
   if (io_SC_components/=0) goto 1
   !
   call io_elemental(ID,VAR='XC_FUNCTIONAL',CH0="",VAR_SZ=1)
   call io_elemental(ID,CH0=SC_potential,&
&       VAR=' xc Functional                   :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=6,MENU=0)
   call io_elemental(ID,L0=Local_V,&
&       VAR=' The potential is local          :')
   call io_elemental(ID,I0=it_now,DB_I0=it_now,&
&       VAR=' Iteration                       :')
   call io_elemental(ID,I0=SC_bands,&
&       VAR=' Total number of bands           :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R0=QP_band_mixing,&
&       VAR=' Bands mixing                    :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R0=rho_convergence)
   call io_elemental(ID,R0=E_convergence)
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
   io_SC_components=io_status(ID)
   if (io_SC_components/=0) goto 1
   !
 endif
 ! 
 call manage_RD_WR_CL_IF_END(ID,ik,nkibz)
 !
 ! Potential
 !===========
 !
 if (l_V.and.local_V.and.ik==0) then
   !
   call io_bulk(ID,VAR="V_xc_sc",VAR_SZ=(/fft_size/))
   call io_bulk(ID,R1=V_xc_sc)
   !
 endif
 !
 if ( l_E ) then
   !
   ! Energies
   !==========
   !
   call io_bulk(ID,VAR="E_sc",VAR_SZ=(/SC_bands,nkibz,n_sp_pol/))
   call io_bulk(ID,R3=E_sc%E(:SC_bands,:,:))
   !
 endif
 !
 if (ik==0) goto 1
 !
 ! Fragmentation
 !===============
 !
 call io_fragment(ID,i_pointer=ik)
 !
 if (l_V.and..not.local_V) then
   !
   ! H_sc
   !======
   !
   allocate(R_H_disk(SC_bands,SC_bands,2))
   !
   call io_bulk(ID,VAR="H_nl_sc_K"//trim(intc(ik)),&
&                 VAR_SZ=(/SC_bands,SC_bands,2/))
   !
   if (write_is_on(ID)) call mat_c2r(H_nl_sc(:,:),R_H_disk)
   call io_bulk(ID,R3=R_H_disk)
   if (read_is_on(ID))  call mat_r2c(R_H_disk,H_nl_sc(:,:))
   !
   deallocate(R_H_disk)
   !
 endif
 !
 if ( l_WF ) then
   !
   ! Rotation matrix 
   !=================
   !
   allocate(R_H_disk(SC_bands,SC_bands,2))
   !
   call io_bulk(ID,VAR="Rotation_Matrix_K"//trim(intc(ik)),&
&                 VAR_SZ=(/SC_bands,SC_bands,2/))
   !
   if (write_is_on(ID)) call mat_c2r(SC_R(:,:,ik),R_H_disk)
   call io_bulk(ID,R3=R_H_disk)
   if (read_is_on(ID))  call mat_r2c(R_H_disk,SC_R(:,:,ik))
   !
   deallocate(R_H_disk)
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function
