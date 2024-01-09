!
! License-Identifier: GPL
!
! Copyright (C) 2008 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function io_SC_components(what,E_sc,ID)
 !
 ! what can be E WF V OBSERVABLES (J,P,D...) G
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_sc_run
 use R_lattice,      ONLY:nkibz
 use stderr,         ONLY:intc
 use SC,             ONLY:SC_bands,rho_convergence,magn_convergence,E_convergence,&
&                         l_NSC_shot,SC_fft_size,V_mean
 use hamiltonian,    ONLY:V_xc_sc,V_hartree_sc,H_nl_sc,l_sc_V_is_local,H_rotation,H_rotation_ref
 use IO_m,           ONLY:io_status,io_sec,read_is_on,write_is_on,frag_SC,&
&                         variable_exist,io_SC,IO_NOT_ALLOWED
 use IO_int,         ONLY:io_connect,io_disconnect,io_header,io_elemental,&
&                         io_bulk,io_fragment,ver_is_gt_or_eq
 use FFT_m,          ONLY:fft_size
 use electrons,      ONLY:levels,n_sp_pol,n_spin
 use timing_m,      ONLY:timing
#include<memory.h>
 !
 integer, intent(in)      :: ID
 type(levels)             :: E_sc
 character(*), intent(in) :: what
 !
 ! Work Space
 !
 integer, external     :: variables_SC
 integer               :: ik,i_sp_pol,ib,i_sec,bands_to_load,VAR_SIZE,ID_frag
 logical               :: l_WF,l_V,l_E,l_IMPOSE_SN,CHECK_,l_MP,l_TEMP
 real(SP), allocatable :: E_tmp(:,:)
 !
 if (.not.io_SC) then
   io_SC_components=IO_NOT_ALLOWED
   call io_disconnect(ID=ID,FORCE=.TRUE.)
   return
 endif
 !
 call timing('io_SC_components',OPR='start')
 !
 l_E =what=="E"
 l_MP=what=="MP"
 l_WF=what=="WF"
 l_V =what=="V"
 !
 ID_frag=0
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
   l_TEMP=.false.
#if defined _ELPH
   l_TEMP=.true.
#endif
   !
   if (any((/l_E,l_V,l_WF,l_MP/))) then
     if (write_is_on(ID).or.variable_exist(ID,"TEMPERATURES")) then
       io_SC_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN,CUTOFF=.true.,TEMP=l_TEMP,FRAG=frag_SC)
     else
       io_SC_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN,CUTOFF=.true.,FRAG=frag_Sc)
     endif
   endif
   !
   if (io_SC_components/=0) goto 1
   !
   io_SC_components=variables_SC(ID,what)
   !======================================
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
 if ( l_E.and.allocated(E_sc%E)) then
   !
   ! Energies
   !==========
   !
   if(read_is_on(ID)) then
     bands_to_load=size(E_sc%E,1)
     !
     ! Make a copy of Kohn-Sham Energies in E_sc%Eo
     !
     if(l_sc_run .and. .not.allocated(E_sc%Eo) ) then
       YAMBO_ALLOC(E_sc%Eo,(E_sc%nb,E_sc%nk,n_sp_pol))
       E_sc%Eo=E_sc%E
     endif
   else
     bands_to_load=SC_bands(2)
   endif
   !
   if (bands_to_load>SC_bands(2)) then
     YAMBO_ALLOC(E_tmp,(E_sc%nk,n_sp_pol))
     E_tmp=-E_sc%E(SC_bands(2),:,:)
   endif
   !
   call io_bulk(ID,VAR="E_sc",VAR_SZ=(/SC_bands(2),nkibz,n_sp_pol/))
   call io_bulk(ID,R3=E_sc%E(:SC_bands(2),:,:))
   !
   !
   if (bands_to_load>SC_bands(2)) then
     !
     ! For either the H_sc and WFs I first check to 
     ! have enough bands (in wf_load and X_O_strengths).
     ! For the energies instead I need to shift the levels > SC_bands(2) (if any).
     !
     E_tmp=E_tmp+E_sc%E(SC_bands(2),:,:)
     !
     forall (ib=SC_bands(2)+1:bands_to_load) &
     & E_sc%E(ib,:,:)=E_sc%E(ib,:,:)+ E_tmp(:,:)
     !
     YAMBO_FREE(E_tmp)
     !
   endif
   !
 endif
 !
 if (ik==0) goto 1
 !
 ! Fragmentation
 !===============
 if(any((/l_E,l_V,l_WF,l_MP/))) call io_fragment(ID,ID_frag,i_fragment=ik)
 !
 if (l_V.and..not.l_sc_V_is_local) then
   !
   ! H_sc
   !======
   !
   call io_bulk(ID_frag,VAR="H_nl_sc_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol)),&
&               VAR_SZ=(/2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1/))
   call io_bulk(ID_frag,C2=H_nl_sc)
   !
 endif
 !
 if ( l_WF ) then
   !
   ! Rotation matrix 
   !=================
   !
   call io_bulk(ID_frag,VAR="Rotation_Matrix_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol)),&
&               VAR_SZ=(/2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1/))
   call io_bulk(ID_frag,C2=H_rotation(:,:,ik,i_sp_pol))
   !
   if (allocated(H_rotation_ref).and.(write_is_on(ID).or..not.l_sc_run)) then
     call io_bulk(ID,VAR="REF_Rotation_Matrix_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol)),&
&               VAR_SZ=(/2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1/))
     call io_bulk(ID,C2=H_rotation_ref(:,:,ik,i_sp_pol))
   endif
   !
 endif
 !
 if ( l_MP ) then
   !
   ! Mean Field Potential 
   !======================
   !
   call io_bulk(ID_frag,VAR="Mean_Field_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol)),&
&               VAR_SZ=(/2,fft_size,SC_bands(2)/))
   call io_bulk(ID_frag,C2=V_mean)
   !
 endif
 !
 call io_fragment_disconnect(ID,ID_frag)
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_SC_components',OPR='stop')
 !
end function
