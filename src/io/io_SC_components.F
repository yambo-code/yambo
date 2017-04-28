!
!        Copyright (C) 2000-2017 the YAMBO team
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
integer function io_SC_components(what,E_sc,ID)
 !
 ! what can be E WF V JPSM G
 !
 use pars,           ONLY:SP,lchlen
 use drivers,        ONLY:l_sc_run
 use R_lattice,      ONLY:nkibz
 use stderr,         ONLY:intc
 use SC,             ONLY:SC_bands,rho_convergence,magn_convergence,E_convergence,&
&                         SC_R,l_NSC_shot,SC_fft_size,V_mean
 use hamiltonian,    ONLY:V_xc_sc,V_hartree_sc,H_nl_sc,l_sc_V_is_local
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_elemental,io_status,&
&                         io_sec,io_bulk,read_is_on,write_is_on,frag_SC,&
&                         manage_OP_IF_START_and_CL_IF_END,variable_exist,io_SC,IO_NOT_ALLOWED,&
&                         io_fragment,ver_is_gt_or_eq
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size
 use electrons,      ONLY:levels,n_sp_pol,n_spin
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 implicit none
 !
 integer, intent(in)      :: ID
 type(levels)             :: E_sc
 character(*), intent(in) :: what
 !
 ! Work Space
 !
 integer, external     :: variables_SC
 character(lchlen)     :: local_description(100)
 !
 real(SP), allocatable :: E_disk(:,:,:)
 integer               :: ik,i_sp_pol,ib,i_sec,bands_to_load,VAR_SIZE
 logical               :: l_WF,l_V,l_E,l_IMPOSE_SN,CHECK_,l_MP,l_T_EL
 !
 if (.not.io_SC) then
   io_SC_components=IO_NOT_ALLOWED
   call io_disconnect(ID=ID,FORCE=.TRUE.)
   return
 endif
 !
#if defined _TIMING
 call timing('io_SC_components',OPR='start')
#endif
 !
 l_E =what=="E"
 l_MP=what=="MP"
 l_WF=what=="WF"
 l_V =what=="V"
 !
 ! When the db.scV is used for single non-SC shot the number
 ! of bands and SC_mixing can be not respected. 
 !
 CHECK_=.TRUE.
 if (l_NSC_shot) CHECK_=.FALSE.
 !
 if (l_MP) io_SC_components=io_connect(desc="sc_MeanPotential",type=2,ID=ID)
 if (l_WF) io_SC_components=io_connect(desc="scWFs",type=2,ID=ID)
 if (l_E)  io_SC_components=io_connect(desc="scE",type=2,ID=ID)
 if (l_V)  io_SC_components=io_connect(desc="scV",type=2,ID=ID)
 !
 if (io_SC_components/=0) goto 1
 !
 ik = maxval(io_sec(ID,:))-1
 !
 i_sec=maxval(io_sec(ID,:))-1
 if(i_sec<=nkibz) then
   i_sp_pol=1
   ik=i_sec
 else
   i_sp_pol=2
   ik=i_sec-nkibz
 endif
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   l_IMPOSE_SN=.not.l_V
   l_T_EL=.false.
#if defined _ELPH
   l_T_EL=.true.
#endif
   !
   if (any((/l_E,l_V,l_WF,l_MP/))) then
     if (write_is_on(ID).or.variable_exist(ID,"TEMPERATURES")) then
       io_SC_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN,CUTOFF=.true.,T_EL=l_T_EL,FRAG=frag_SC)
     else
       io_SC_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN,CUTOFF=.true.,FRAG=frag_Sc)
     endif
   endif
   !
   if (io_SC_components/=0) goto 1
   !
   io_SC_components=variables_SC(ID,local_description,what)
   !=======================================================
   if (io_SC_components/=0) goto 1
   !
   if(any((/l_E,l_V,l_WF,l_MP/))) then
     !
     VAR_SIZE=2
     if (ver_is_gt_or_eq(ID,revision=4464)) VAR_SIZE=3
     call io_elemental(ID,VAR="RHOCONV_ECONV",VAR_SZ=VAR_SIZE,MENU=0) 
     call io_elemental(ID,R0=rho_convergence)
     if (ver_is_gt_or_eq(ID,revision=4464)) call io_elemental(ID,R0=magn_convergence)
     call io_elemental(ID,R0=E_convergence)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
   endif
   !
   io_SC_components=io_status(ID)
   if (io_SC_components/=0) goto 1
   !
 endif
 ! 
 call manage_OP_IF_START_and_CL_IF_END(ID,ik,nkibz)
 !
 ! Potential
 !===========
 !
 if (l_V.and.l_sc_V_is_local.and.ik==0.and.allocated(V_xc_sc)) then
   !
   call io_bulk(ID,VAR="V_xc_sc",VAR_SZ=(/SC_fft_size,n_spin/))
   call io_bulk(ID,R2=V_xc_sc)
   !
   call io_bulk(ID,VAR="V_hartree",VAR_SZ=(/SC_fft_size,n_spin/))
   call io_bulk(ID,R2=V_hartree_sc)
   !
 endif
 !
 if ( l_E.and.associated(E_sc%E)) then
   !
   ! Energies
   !==========
   !
   if(read_is_on(ID)) then
     bands_to_load=size(E_sc%E,1)
     if(l_sc_run .and. .not.associated(E_sc%Eo) ) then
       allocate(E_sc%Eo(E_sc%nb,E_sc%nk,n_sp_pol))
       call mem_est("E-Eo",(/size(E_sc%Eo)/),(/SP/))
       E_sc%Eo=E_sc%E
     endif
   else
     bands_to_load=SC_bands(2)
   endif
   !
   call io_bulk(ID,VAR="E_sc",VAR_SZ=(/SC_bands(2),nkibz,n_sp_pol/))
   call io_bulk(ID,R3=E_sc%E(:SC_bands(2),:,:))
   !
   if (bands_to_load>SC_bands(2)) then
     !
     ! For either the H_sc and WFs I first check to 
     ! have enough bands (in wf_load and X_O_strengths).
     ! For the energies instead I need to shift the levels > SC_bands(2) (if any).
     !
     forall (ib=SC_bands(2)+1:bands_to_load) &
     & E_sc%E(ib,:,:)=E_sc%Eo(ib,:,:)+ ( E_sc%E(SC_bands(2),:,:)-E_sc%Eo(SC_bands(2),:,:) )
     !
   endif
   !
 endif
 !
 if(any((/l_E,l_V,l_WF,l_MP/))) then
   !
   if (ik==0) goto 1
   !
   ! Fragmentation
   !===============
   !
   call io_fragment(ID,i_fragment=ik)
   !
 endif
 !
 if (l_V.and..not.l_sc_V_is_local) then
   !
   ! H_sc
   !======
   !
   call io_bulk(ID,VAR="H_nl_sc_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol)),&
&               VAR_SZ=(/2,SC_bands(2),SC_bands(2)/))
   call io_bulk(ID,C2=H_nl_sc)
   !
 endif
 !
 if ( l_WF ) then
   !
   ! Rotation matrix 
   !=================
   !
   call io_bulk(ID,VAR="Rotation_Matrix_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol)),&
&               VAR_SZ=(/2,SC_bands(2),SC_bands(2)/))
   call io_bulk(ID,C2=SC_R(:,:,ik,i_sp_pol))
   !
 endif
 !
 if ( l_MP ) then
   !
   ! Mean Field Potential 
   !======================
   !
   call io_bulk(ID,VAR="Mean_Field_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol)),&
&               VAR_SZ=(/2,fft_size,SC_bands(2)/))
   call io_bulk(ID,C2=V_mean)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_SC_components',OPR='stop')
#endif
 !
end function
