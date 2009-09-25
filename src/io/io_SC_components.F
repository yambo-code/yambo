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
 ! what can be E WF V J G
 !
 use pars,           ONLY:SP
 use units,          ONLY:AU2Tesla
 use R_lattice,      ONLY:nkibz
 use D_lattice,      ONLY:atoms_string
 use stderr,         ONLY:intc
 use QP_m,           ONLY:SC_band_mixing
 use SC,             ONLY:SC_bands,it_now,rho_convergence,E_convergence,SC_potential,&
&                         SC_R,V_xc_sc,local_V,H_nl_sc,SC_kind,SC_perturbation,&
&                         SC_xc_functional,V_hartree_sc,NSC_shot,SC_fft_size,&
&                         SC_ng_Sx,SC_X_bands,SC_X_size
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_elemental,io_status,&
&                         io_sec,io_bulk,io_fragment,read_is_on,write_is_on,&
&                         manage_RD_WR_CL_IF_END,ver_is_gt_or_eq,io_reset,IO_INCOMPATIBLE_VAR
#if defined _RT
 use real_time,      ONLY:l_rt_efield,l_RPA,RT_step,Tot_steps,       &
&                         Thermal_steps,Integrator_name,l_qp_analyze,DiagH_Iter,     &
&                         LifeTime,G_lesser,i_time,l_no_diamagnetic,j_current,A_norm,&
&                         n_ext_fields,Efield,max_rt_steps_disk,current_cache
 use IO_m,           ONLY:io_restart,io_check_restart
 use QP_m,           ONLY:QP_nk
#endif                                
#if defined _MAGNETIC
 use magnetic,       ONLY:MAG_hamiltonian_type,MAG_B_z,MAG_radius,MAG_gauge
 use global_XC,      ONLY:EXT_Bz_LANDAU,EXT_Bz_PAULI,EXT_Bz_MAGNETISM
#endif
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use electrons,      ONLY:levels,n_sp_pol,n_spin
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
 integer               :: ik,bands_to_load,ib,SC_kind_disk,SC_perturbation_disk,&
&                         io_status_save,i_spin,i1
 logical               :: l_wf,l_V,l_E,l_IMPOSE_SN,CHECK_
#if defined _RT		
 integer               :: file_number
 logical               :: l_J,l_G,IO_current
 real(SP)              :: current_pot_disk(3+2*n_ext_fields)
#endif
 !
 l_E =what=="E"
 l_wf=what=="WF"
 l_V =what=="V"
#if defined _RT		
 l_J =what=="J"
 l_G =what=="G"
#endif
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
#if defined _RT
 !
 ! Write only every max_rt_steps_disk
 !
 if(l_J)then
   !      
   file_number=(i_time-1)/max_rt_steps_disk
   !      
   if(io_sec(ID,1)==1) then 
     io_SC_components=io_connect(desc="rtJ",type=2,ID=ID)
   else
     !      
     IO_current=.false.
     if(mod(i_time,max_rt_steps_disk)  ==0.and.write_is_on(ID))  IO_current=.true.
     if(mod(i_time-1,max_rt_steps_disk)==0.and.read_is_on(ID))   IO_current=.true.
     !
     if(IO_current) then
       io_SC_components=io_connect(desc="rtJ_"//trim(intc(file_number)),type=2,ID=ID)
     else
       io_SC_components=0
     endif
     !
   endif
 !
 endif
 !
 if(l_G) then
   if(mod(i_time,max_rt_steps_disk)/=0.and.write_is_on(ID)) then
     io_SC_components=0
     call io_reset(ID)
     return
   endif
   io_SC_components=io_connect(desc="rtG",type=2,ID=ID)
 endif
 !
#endif
 !
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
   if (ver_is_gt_or_eq(ID,revision=455)) then
     call io_elemental(ID,VAR="XC_GLOBALS",VAR_SZ=3,MENU=0)
     call io_elemental(ID,I0=SC_perturbation,DB_I0=SC_perturbation_disk)
   else
     call io_elemental(ID,VAR="XC_KIND",VAR_SZ=2,MENU=0)
   endif
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
   call io_elemental(ID,I0=SC_bands,&
&       VAR=' Total number of bands           :',CHECK=CHECK_,OP=(/"=="/))
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
     io_status(ID)=IO_INCOMPATIBLE_VAR
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
#if defined _MAGNETIC
   if (any((/SC_perturbation_disk==EXT_Bz_LANDAU,SC_perturbation_disk==EXT_Bz_PAULI,&
&            SC_perturbation_disk==EXT_Bz_MAGNETISM/))) then
     call io_elemental(ID,VAR="MAG_hamiltonian",CH0="",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,CH0=MAG_hamiltonian_type, &
&         VAR=' [MAG] Hamiltonian kind          :',CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     call io_elemental(ID,VAR="B_FIELD_parameters",VAR_SZ=2,MENU=0) 
     call io_elemental(ID,UNIT=AU2Tesla,R0=MAG_B_z,&
&         VAR=' [MAG] z-components B filed   [T]:',CHECK=.true.,OP=(/"=="/))
     if (SC_perturbation_disk/=EXT_Bz_PAULI) then
       call io_elemental(ID,R0=MAG_radius,&
&         VAR=' [MAG] B flux radius             :',CHECK=.true.,OP=(/"=="/))
     endif
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     call io_elemental(ID,VAR='MAG_GAUGE',CH0="",VAR_SZ=1,MENU=0)
     call io_elemental(ID,CH0=MAG_gauge,&
&         VAR=' [MAG] Gauge                     :',CHECK=CHECK_,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
   endif
#endif
   !
#if defined _RT
   !
   if(any((/l_J,l_G/))) then
     call io_elemental(ID,VAR="RT_DYNAMICS",VAR_SZ=4,MENU=0) 
     call io_elemental(ID,L0=l_RPA,VAR=' RPA optics in real time  :', &
&     CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,&
&       VAR=" external field intensity          :",R0=RT_step,CHECK=.true.,OP=(/"=="/))
     if(l_G) call io_elemental(ID, &
&       VAR=" total number of steps          :",I0=Tot_steps,CHECK=.true.,OP=(/">="/))
     call io_elemental(ID, &
&       VAR=" number of thermalization steps :",I0=Thermal_steps,CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     call io_elemental(ID,VAR="INTEGRATOR",CH0="",VAR_SZ=1) 
     call io_elemental(ID,CH0=Integrator_name, &
&         VAR=' integrator                   :',CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     call io_elemental(ID,VAR="RT_OTHERS",VAR_SZ=7,MENU=0) 
     call io_elemental(ID,L0=l_qp_analyze,VAR=' analize quasi-particles :', &
&     CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,I0=DiagH_Iter,VAR='diagonalization interval :',&
&     CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,I0=max_rt_steps_disk,VAR='cache size :',&
&     CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,R0=LifeTime,VAR=' quasi-particle life time :', &
&     CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,L0=l_rt_efield,VAR=' External Field is present :', &
&      CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,L0=l_no_diamagnetic,VAR=' No diamagnetic term        :',& 
&       CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     do i1=1,n_ext_fields
       call io_elemental(ID,VAR="EXTERNAL_FIELD"//trim(intc(i1)),VAR_SZ=7,MENU=0) 
       call io_elemental(ID,R1=Efield(i1)%versor,CHECK=.true., &
&       VAR=" electric field versor             :",OP=(/"==","==","=="/))
       call io_elemental(ID,&
&       VAR=" external field intensity          :",R0=Efield(i1)%intensity,CHECK=.true.,OP=(/"=="/))
       call io_elemental(ID,&
&       VAR=" external field frequency          :",R0=Efield(i1)%frequency,CHECK=.true.,OP=(/"=="/))
       call io_elemental(ID,L0=Efield(i1)%no_intraband,VAR=' No intraband transitions   :',&
&       CHECK=.true.,OP=(/"=="/))
       call io_elemental(ID,R0=Efield(i1)%t_initial,VAR=' initial time for the external field :',&
&       CHECK=.true.,OP=(/"=="/))
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       !                       
       call io_elemental(ID,VAR="FIELD_NAME"//trim(intc(i1)),CH0="",VAR_SZ=1) 
       call io_elemental(ID,CH0=Efield(i1)%ef_name, &
&         VAR=' external field                  :',CHECK=.true.,OP=(/"=="/))
       call io_elemental(ID,VAR="",VAR_SZ=0)
     enddo
     !
   endif
   !
#endif
   !
#if defined _RT
   if(any((/l_E,l_V,l_wf/))) then
#endif
   !
   call io_elemental(ID,VAR="RHOCONV_ECONV",VAR_SZ=2,MENU=0) 
   call io_elemental(ID,R0=rho_convergence)
   call io_elemental(ID,R0=E_convergence)
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
#if defined _RT
   endif
#endif
   !
   io_SC_components=io_status(ID)
   if (io_SC_components/=0) goto 1
   !
 endif
 ! 
#if defined _RT
 if(l_J) then
   call manage_RD_WR_CL_IF_END(ID,ik,Tot_steps)
 else
#endif
   call manage_RD_WR_CL_IF_END(ID,ik,nkibz)
#if defined _RT
 endif
#endif
 !
 ! Potential
 !===========

 !
 if (l_V.and.local_V.and.ik==0.and.allocated(V_xc_sc)) then
   !
   call io_bulk(ID,VAR="V_xc_sc",VAR_SZ=(/SC_fft_size,n_spin/))
   call io_bulk(ID,R2=V_xc_sc)
   !
   call io_bulk(ID,VAR="V_hartree",VAR_SZ=(/SC_fft_size,n_sp_pol/))
   call io_bulk(ID,R2=V_hartree_sc)
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
#if defined _RT
 if(any((/l_E,l_V,l_wf,l_G/))) then
#endif
 !
 if (ik==0) goto 1
 !
 ! Fragmentation
 !===============
 !
 call io_fragment(ID,i_pointer=ik)
 !
#if defined _RT
 endif
#endif
 !
 if (l_V.and..not.local_V) then
   !
   ! H_sc
   !======
   !
   allocate(R_H_disk(SC_bands,SC_bands,2))
   !
   do i_spin=1,n_sp_pol
     !
     call io_bulk(ID,VAR="H_nl_sc_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&                 VAR_SZ=(/SC_bands,SC_bands,2/))
     !
     if (write_is_on(ID)) call mat_c2r(H_nl_sc(:,:,i_spin),R_H_disk)
     call io_bulk(ID,R3=R_H_disk)
     if (read_is_on(ID))  call mat_r2c(R_H_disk,H_nl_sc(:,:,i_spin))
     !
   enddo
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
   do i_spin=1,n_sp_pol
     call io_bulk(ID,VAR="Rotation_Matrix_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&                 VAR_SZ=(/SC_bands,SC_bands,2/))
     !
     if (write_is_on(ID)) call mat_c2r(SC_R(:,:,ik,i_spin),R_H_disk)
     call io_bulk(ID,R3=R_H_disk)
     if (read_is_on(ID))  call mat_r2c(R_H_disk,SC_R(:,:,ik,i_spin))
     !
   enddo
   deallocate(R_H_disk)
 endif
 !
#if defined _RT
 !
 if ( l_G ) then
   !
   ! lesser Green Function
   !=================
   !
   if (io_SC_components/=0) goto 1  
   !
   allocate(R_H_disk(SC_bands,SC_bands,2))
   !
!  do i_spin=1,n_sp_pol
   i_spin=1
   !
   call io_bulk(ID,VAR="G_lesser_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&       VAR_SZ=(/SC_bands,SC_bands,2/))
   !
   if (write_is_on(ID)) call mat_c2r(G_lesser(:,:,ik),R_H_disk)
   call io_bulk(ID,R3=R_H_disk)
   if (read_is_on(ID))  call mat_r2c(R_H_disk,G_lesser(:,:,ik))
   !
!  enddo
   deallocate(R_H_disk)
   !
 endif
 !
 if ( l_J ) then
   !
   ! Current and Vector Potential
   !=================
   !        
   if(io_sec(ID,1)/=1) then
     !
     if(write_is_on(ID)) then
       !
       current_cache(:3,mod(i_time-1,max_rt_steps_disk)+1)=j_current(:3)
       do i1=1,n_ext_fields
         current_cache(2*i1+2,mod(i_time-1,max_rt_steps_disk)+1)= real(A_norm(i1))
         current_cache(2*i1+3,mod(i_time-1,max_rt_steps_disk)+1)=aimag(A_norm(i1))
       enddo
       !
     endif
     !
     if (IO_current) then
       !                                
       call io_bulk(ID,VAR="Current_and_Potential_TIME"//trim(intc(file_number)), &
&                  VAR_SZ=(/3+2*n_ext_fields,max_rt_steps_disk/))
       !
       call io_bulk(ID,R2=current_cache)
       !
       call io_disconnect(ID)
       !
     endif
     !
     if(read_is_on(ID)) then
       !
       ! Read from cache
       !
       j_current(:)=current_cache(:,mod(i_time-1,max_rt_steps_disk)+1)
       do i1=1,n_ext_fields
         A_norm(i1)=cmplx(current_cache(2*i1+2,mod(i_time-1,max_rt_steps_disk)+1), &
&                         current_cache(2*i1+3,mod(i_time-1,max_rt_steps_disk)+1))
       enddo
       !
     endif 
     !
     if(.not.IO_current) call io_reset(ID) 
     !
   endif
   !
 endif       
 !
#endif
 !
1 call io_disconnect(ID=ID)
 !
end function
