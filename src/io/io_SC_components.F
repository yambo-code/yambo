!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
integer function io_SC_components(what,E_sc,ID)
 !
 ! what can be E WF V
 !
 use pars,           ONLY:SP
 use units,          ONLY:HARTREE
 use drivers,        ONLY:l_sc_run
 use R_lattice,      ONLY:nkibz
 use D_lattice,      ONLY:atoms_string
 use stderr,         ONLY:intc
 use SC,             ONLY:SC_bands,it_now,rho_convergence,E_convergence,SC_potential,&
&                         SC_R,V_xc_sc,local_V,H_nl_sc,SC_band_mixing,SC_kind,&
&                         SC_xc_functional,V_hartree_sc,NSC_shot,SC_fft_size,&
&                         SC_ng_Sx,SC_X_bands,SC_X_size
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_elemental,io_status,&
&                         io_sec,io_bulk,io_fragment,read_is_on,write_is_on,&
&                         manage_RD_WR_CL_IF_END
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use electrons,      ONLY:levels,n_sp_pol
 use global_XC,      ONLY:SC_EXX,SC_HF,SC_CHOSEX,SC_EXXC
 implicit none
 !
 integer               :: ID
 type(levels)          :: E_sc
 character(*)          :: what
 !
 ! Work Space
 !
 real(SP), allocatable :: R_H_disk(:,:,:),E_disk(:,:,:)
 integer               :: ik,bands_to_load,ib,SC_kind_disk,io_status_save
 logical               :: l_wf,l_V,l_E,l_IMPOSE_SN,CHECK_
 !
 l_E =what=="E"
 l_wf=what=="WF"
 l_V =what=="V"
 !
 ! When the db.scV is used for single non-SC shot the number
 ! of bands and SC_mixing can be not respected. 
 !
 CHECK_=.TRUE.
 if (NSC_shot) CHECK_=.FALSE.
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
   l_IMPOSE_SN=.not.l_V
   !
   io_SC_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN)
   !
   if (io_SC_components/=0) goto 1
   !
   call io_elemental(ID,VAR='ATOMS_STRING',CH0="",VAR_SZ=1)
   call io_elemental(ID,CH0=atoms_string,&
&       VAR=' Unit cell atoms                 :',CHECK=CHECK_,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='XC_FUNCTIONAL',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=SC_potential,&
&       VAR=' xc Functional                   :',CHECK=CHECK_,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="XC_KIND",VAR_SZ=2,MENU=0)
   call io_elemental(ID,I0=SC_kind,DB_I0=SC_kind_disk)
   call io_elemental(ID,I0=SC_xc_functional)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   io_status_save=io_status(ID)
   call io_elemental(ID,VAR="LOCAL_ITER_BANDS_MIX",VAR_SZ=4,MENU=0) 
   call io_elemental(ID,L0=Local_V,&
&       VAR=' The potential is local          :')
   call io_elemental(ID,I0=it_now,DB_I0=it_now,&
&       VAR=' Iteration                       :')
   !
   if (l_sc_run) then
     call io_elemental(ID,I0=SC_bands,&
&         VAR=' Total number of bands           :',CHECK=CHECK_,OP=(/"=="/))
   else
     call io_elemental(ID,I0=SC_bands,&
&         VAR=' Total number of bands           :',CHECK=.true.,OP=(/"<="/))
   endif
   !
   call io_elemental(ID,R0=SC_band_mixing,&
&       VAR=' Bands mixing                    :',CHECK=CHECK_,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   if (any((/SC_kind_disk==SC_EXX,SC_kind_disk==SC_EXXC,SC_kind_disk==SC_HF,&
&            SC_kind_disk==SC_CHOSEX/))) then
     call io_elemental(ID,VAR="SIGMAx_G",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,I0=SC_ng_Sx,&
&         VAR=' Sx RL components                :',CHECK=CHECK_,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
   if ( SC_kind_disk==SC_CHOSEX   ) then
     call io_elemental(ID,VAR="X_bands_size",VAR_SZ=3,MENU=0) 
     call io_elemental(ID,I1=SC_X_bands,&
&         VAR=' Response function bands         :',CHECK=CHECK_,OP=(/"==","=="/))
     call io_elemental(ID,I0=SC_X_size,&
&         VAR='                   size          :',CHECK=CHECK_,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
   if (io_status_save==0.and.io_status(ID)<0) then
     io_status(ID)=-2
     if (NSC_shot) io_status(ID)=0
   endif
   !
   if (all((/SC_kind_disk/=SC_HF,SC_kind_disk/=SC_CHOSEX/))) then
     call io_elemental(ID,VAR="FFT_SIZE",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,I0=SC_fft_size,&
&         VAR=' FFT mesh points                 :',CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
   call io_elemental(ID,VAR="RHOCONV_ECONV",VAR_SZ=2,MENU=0) 
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
 if (l_V.and.local_V.and.ik==0.and.allocated(V_xc_sc)) then
   !
   call io_bulk(ID,VAR="V_xc_sc",VAR_SZ=(/SC_fft_size/))
   call io_bulk(ID,R1=V_xc_sc)
   !
   call io_bulk(ID,VAR="V_hartree",VAR_SZ=(/SC_fft_size/))
   call io_bulk(ID,R1=V_hartree_sc)
   !
 endif
 !
 if ( l_E.and.associated(E_sc%E)) then
   !
   ! Energies
   !==========
   !
   call io_bulk(ID,VAR="E_sc",VAR_SZ=(/SC_bands,nkibz,n_sp_pol/))
   if (write_is_on(ID)) then
     call io_bulk(ID,R3=E_sc%E(:SC_bands,:,:))
   else
     allocate(E_disk(SC_bands,nkibz,n_sp_pol))
     call io_bulk(ID,R3=E_disk)
     !
     bands_to_load=size(E_sc%E,1)
     !
     if (bands_to_load>SC_bands) then
       !
       ! For either the H_sc and WFs I first check to 
       ! have enough bands (in wf_load and X_O_strengths).
       ! For the energies instead I need to shift the levels > SC_bands (if any).
       !
       forall (ib=SC_bands+1:bands_to_load) E_sc%E(ib,:,:)=E_sc%E(ib,:,:)-E_sc%E(SC_bands,:,:)
       E_sc%E(:SC_bands,:,:)=E_disk(:SC_bands,:,:)
       forall (ib=SC_bands+1:bands_to_load) E_sc%E(ib,:,:)=E_sc%E(ib,:,:)+E_sc%E(SC_bands,:,:)
       !
     else
       E_sc%E(:bands_to_load,:,:)=E_disk(:bands_to_load,:,:)
     endif
     deallocate(E_disk)
   endif
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
